use std::collections::HashMap;

use crate::{
    compiler::{Local, Upvalue},
    value::LuaString,
};

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) locals: Vec<Local>,
    pub(crate) upvalues: HashMap<LuaString, Upvalue>,
    parent_frame: *mut Frame,
}

#[derive(Debug)]
pub(crate) enum ResolvedName<'a> {
    Local { register: u8, local: &'a Local },
    Upvalue { index: u8, attributes: u8 },
    None,
}

impl<'a> ResolvedName<'a> {
    pub(crate) fn attributes(&self) -> u8 {
        match self {
            ResolvedName::Local { local, .. } => local.attributes,
            ResolvedName::Upvalue { attributes, .. } => *attributes,
            ResolvedName::None => 0,
        }
    }

    pub(crate) fn is_constant(&self) -> bool {
        self.attributes() & crate::value::LuaVariableAttribute::Constant as u8 != 0
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum UpvalueLocation {
    Local { register: u8 },
    Upvalue { index: u8 },
}

#[derive(Debug, Clone, Copy)]
enum ResolvedUpvalue {
    Local { register: u8, attributes: u8 },
    Upvalue { index: u8, attributes: u8 },
}

impl ResolvedUpvalue {
    fn attributes(&self) -> u8 {
        match self {
            ResolvedUpvalue::Local { attributes, .. } => *attributes,
            ResolvedUpvalue::Upvalue { attributes, .. } => *attributes,
        }
    }

    fn source(&self) -> UpvalueLocation {
        match self {
            ResolvedUpvalue::Local { register, .. } => UpvalueLocation::Local {
                register: *register,
            },
            ResolvedUpvalue::Upvalue { index, .. } => UpvalueLocation::Upvalue { index: *index },
        }
    }
}

// NOTE: All of these splits below between the entry methods and their `_inner` variants are just
// to make the borrow checker happy; to make it clear that there's a split between what we borrow
// from `self.locals` and from `self.upvalues`.
impl Frame {
    pub(crate) fn new() -> Self {
        Self {
            locals: vec![],
            upvalues: HashMap::new(),
            parent_frame: std::ptr::null_mut(),
        }
    }

    pub(crate) fn with_parent(parent_frame: *mut Self) -> Self {
        Self {
            locals: vec![],
            upvalues: HashMap::new(),
            parent_frame,
        }
    }

    /// Try to resolve a name to either a local or an upvalue.
    pub(crate) fn resolve_name<'a>(&'a mut self, name: &LuaString) -> ResolvedName<'a> {
        Self::resolve_name_inner(&self.locals, &mut self.upvalues, self.parent_frame, name)
    }

    /// Find a local by its name, returning its register index and a reference to it.
    pub(crate) fn resolve_local(&self, name: &LuaString) -> Option<(u8, &Local)> {
        Self::resolve_local_inner(&self.locals, name)
    }

    fn resolve_upvalue(&mut self, name: &LuaString) -> Option<ResolvedUpvalue> {
        Self::resolve_upvalue_inner(&self.locals, &mut self.upvalues, self.parent_frame, name)
    }

    /// Add a new upvalue to this frame.
    pub(crate) fn add_upvalue(
        &mut self,
        name: LuaString,
        source: UpvalueLocation,
        attributes: u8,
    ) -> &Upvalue {
        Self::add_upvalue_inner(&mut self.upvalues, name, source, attributes)
    }

    fn resolve_name_inner<'a>(
        locals: &'a [Local],
        upvalues: &'a mut HashMap<LuaString, Upvalue>,
        parent_frame: *mut Self,
        name: &LuaString,
    ) -> ResolvedName<'a> {
        if let Some((register, local)) = Self::resolve_local_inner(locals, name) {
            return ResolvedName::Local { register, local };
        }

        if let Some(upvalue) = Self::resolve_upvalue_inner(locals, upvalues, parent_frame, name) {
            let ResolvedUpvalue::Upvalue { index, attributes } = upvalue else {
                unreachable!("we already know it's not a local");
            };
            return ResolvedName::Upvalue { index, attributes };
        }

        ResolvedName::None
    }

    fn resolve_local_inner<'l>(locals: &'l [Local], name: &[u8]) -> Option<(u8, &'l Local)> {
        locals.iter().enumerate().rev().find_map(|(i, local)| {
            if local.name == name {
                Some((i as u8, local))
            } else {
                None
            }
        })
    }

    /// Find an upvalue by its name, returning a reference to it. If no such upvalue exists, but a
    /// local  (or an upvalue) by that name exists in a parent frame, an upvalue will be added, and
    /// the upvalue will be registered in all relevant parent frames.
    fn resolve_upvalue_inner(
        locals: &[Local],
        upvalues: &mut HashMap<LuaString, Upvalue>,
        parent_frame: *mut Self,
        name: &LuaString,
    ) -> Option<ResolvedUpvalue> {
        if let Some(upvalue) = upvalues.get(name) {
            return Some(ResolvedUpvalue::Upvalue {
                index: upvalue.index,
                attributes: upvalue.attributes,
            });
        }
        //
        // polonius!(|upvalues| -> Option<ResolvedUpvalue> {
        //     if let Some(upvalue) = upvalues.get(name) {
        //         polonius_return!(Some(ResolvedUpvalue::Upvalue {
        //             index: upvalue.index,
        //             attributes: upvalue.attributes,
        //         }));
        //     }
        // });

        if let Some((local_register, local)) = Self::resolve_local_inner(locals, name) {
            // return Some(Self::add_upvalue_inner(
            //     upvalues,
            //     name.clone(),
            //     // local_register,
            //     // true,
            //     UpvalueLocation::Local {
            //         register: local_register,
            //     },
            //     local.attributes,
            // ));
            return Some(ResolvedUpvalue::Local {
                register: local_register,
                attributes: local.attributes,
            });
        }

        if parent_frame.is_null() {
            return None;
        }

        if let Some(upvalue) = unsafe { parent_frame.as_mut().unwrap() }.resolve_upvalue(name) {
            let attributes = upvalue.attributes();
            let own_upvalue =
                Self::add_upvalue_inner(upvalues, name.clone(), upvalue.source(), attributes);

            Some(ResolvedUpvalue::Upvalue {
                index: own_upvalue.index,
                attributes,
            })
        } else {
            None
        }
    }

    fn add_upvalue_inner(
        upvalues: &mut HashMap<LuaString, Upvalue>,
        name: LuaString,
        source: UpvalueLocation,
        attributes: u8,
    ) -> &Upvalue {
        let num_upvalues = upvalues.len() as u8;
        upvalues.entry(name).or_insert_with(|| Upvalue {
            index: num_upvalues,
            source,
            attributes,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::value::LuaVariableAttribute;

    // use crate::compiler::Frame;
    use super::*;

    #[test]
    fn frame_resolve_names() {
        let mut frame1 = Frame::new();

        // Doesn't have any names yet
        assert!(matches!(
            frame1.resolve_name(&(b"x".into())),
            ResolvedName::None
        ));

        // After adding, x resolves to the local
        frame1.locals.push(Local {
            name: b"x".into(),
            depth: 0,
            span: None,
            attributes: 0,
        });
        assert!(matches!(
            frame1.resolve_name(&b"x".into()),
            ResolvedName::Local { register: 0, .. }
        ));

        // Registers are incrementing
        frame1.locals.push(Local {
            name: b"foo".into(),
            depth: 0,
            span: None,
            attributes: 0,
        });
        assert!(matches!(
            frame1.resolve_name(&b"foo".into()),
            ResolvedName::Local { register: 1, .. }
        ));

        // Can add an upvalue manually and resolve it
        frame1.add_upvalue(
            b"y".into(),
            UpvalueLocation::Upvalue { index: 2 },
            LuaVariableAttribute::ToBeClosed as u8,
        );
        assert!(matches!(
            frame1.resolve_name(&(b"y".into())),
            ResolvedName::Upvalue { .. }
        ));

        // Create a new frame with frame1 as parent
        let mut frame2 = Frame::with_parent(std::ptr::from_mut(&mut frame1));
        assert!(matches!(
            frame2.resolve_name(&(b"z".into())),
            ResolvedName::None
        ));

        // Should not contain an upvalue yet
        assert!(!frame2.upvalues.contains_key(&b"y".into()));

        // But we can resolve it, and then it exists
        assert!(matches!(
            frame2.resolve_name(&(b"y".into())),
            ResolvedName::Upvalue {
                // Note that index 0 here is the index of the upvalue in the parent frame, not
                // where the parent frame gets the value from!
                index: 0,
                attributes: attr,
            } if attr == LuaVariableAttribute::ToBeClosed as u8
        ));
        // And now we're tracking the upvalue
        assert!(frame2.upvalues.contains_key(&b"y".into()));

        // We'll give frame 2 its own local
        frame2.locals.push(Local {
            name: b"z".into(),
            depth: 0,
            span: None,
            attributes: 0,
        });

        // From frame3, we can resolve the local `x` from the grandparent, its source is an upvalue
        // from frame2 with index 1 (since frame2 also already has an upvalue for `y`)
        let mut frame3 = Frame::with_parent(std::ptr::from_mut(&mut frame2));
        assert!(!frame2.upvalues.contains_key(&b"x".into()));
        assert!(matches!(
            frame3.resolve_name(&(b"x".into())),
            ResolvedName::Upvalue { index: 0, .. }
        ));
        assert!(matches!(
            frame3.upvalues.get(&b"x".into()),
            Some(Upvalue {
                index: 0,
                source: UpvalueLocation::Upvalue { index: 1 },
                ..
            })
        ));
        // Frame2 must now have the upvalue x, pointing to the local variable in the parent.
        assert!(matches!(
            frame2.upvalues.get(&b"x".into()),
            Some(Upvalue {
                source: UpvalueLocation::Local { register: 0 },
                ..
            })
        ));
    }
}
