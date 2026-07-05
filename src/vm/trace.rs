use std::{
    collections::HashMap,
    fmt::Write,
    sync::{Arc, RwLock},
};

use crate::{
    chunk::Chunk,
    debug,
    value::{LuaValue, UpValue},
    vm::ChunkIndex,
};

#[derive(Debug)]
pub(crate) struct VMTracer {
    previous_frame_stack: Vec<LuaValue>,
    previous_multres: usize,
    previous_varargs: Vec<LuaValue>,
    previous_upvalues: Option<HashMap<usize, Arc<RwLock<UpValue>>>>,
}

const DIM: &str = "\x1b[90m";
const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0m";

impl VMTracer {
    pub(crate) fn new() -> Self {
        VMTracer {
            previous_frame_stack: Vec::new(),
            previous_multres: 0,
            previous_varargs: Vec::new(),
            previous_upvalues: None,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn trace_instruction(
        &mut self,
        frame_stack: &[LuaValue],
        frame_varargs: &[LuaValue],
        frame_upvalues: Option<&HashMap<usize, Arc<RwLock<UpValue>>>>,
        chunk: &Chunk,
        chunk_index: ChunkIndex,
        ip: usize,
        multres: usize,
    ) {
        let frame_stack_str = Self::get_highlighted_changes_str(
            frame_stack.iter().enumerate(),
            self.previous_frame_stack.as_slice(),
            '[',
            ']',
        );
        let frame_varargs_str = Self::get_highlighted_changes_str(
            frame_varargs.iter().enumerate(),
            self.previous_varargs.as_slice(),
            '[',
            ']',
        );
        let frame_upvalues_str = Self::get_highlighted_changes_str(
            frame_upvalues
                .map(|map| {
                    let n = map.len();
                    (0..n).map(|i| (i, map.get(&i).unwrap()))
                })
                .into_iter()
                .flatten(),
            self.previous_upvalues.as_ref(),
            '{',
            '}',
        );

        let multres_str = if self.previous_multres == multres {
            format!("{DIM}{multres}{RESET}")
        } else {
            format!("{GREEN}{multres}{RESET}")
        };

        eprintln!(
            "frame_stack={frame_stack_str} frame_varargs={frame_varargs_str} multres={multres_str} upvalues={frame_upvalues_str}",
        );
        debug::print_instruction(chunk, chunk_index, ip, chunk.instructions[ip]);

        self.previous_frame_stack = frame_stack.to_vec();
        self.previous_varargs = frame_varargs.to_vec();
        self.previous_multres = multres;
        self.previous_upvalues = frame_upvalues.cloned();
    }

    fn get_highlighted_changes_str<'a, T: std::fmt::Debug + 'a, V>(
        values: V,
        previous_values: impl TraceValueSet<T>,
        open_bracket: char,
        close_bracket: char,
    ) -> String
    where
        V: IntoIterator<Item = (usize, &'a T)>,
    {
        let mut str = open_bracket.to_string();
        for (i, v) in values {
            if i > 0 {
                str.push_str(", ");
            }

            str.push_str(if previous_values.has_changed(i, v) {
                GREEN
            } else {
                DIM
            });

            let _ = write!(str, "{i}={v:?}");
            str.push_str(RESET);
        }
        str.push(close_bracket);
        str
    }
}

trait TraceValueSet<T> {
    fn has_changed(&self, index: usize, current_value: &T) -> bool;
}

impl TraceValueSet<LuaValue> for &[LuaValue] {
    fn has_changed(&self, index: usize, current_value: &LuaValue) -> bool {
        self.get(index) != Some(current_value)
    }
}

impl TraceValueSet<Arc<RwLock<UpValue>>> for Option<&HashMap<usize, Arc<RwLock<UpValue>>>> {
    fn has_changed(&self, index: usize, current_value: &Arc<RwLock<UpValue>>) -> bool {
        match self {
            Some(map) => map.get(&index).is_none_or(|v| {
                !Arc::ptr_eq(v, current_value)
                    || *v.read().unwrap() != *current_value.read().unwrap()
            }),
            None => true,
        }
    }
}
