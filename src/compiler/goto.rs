use crate::{token::Span, vm::JumpAddr};

/// Goto statement waiting to be resolved. Gotos are tricky, we might be jumping out of a scope
/// where we may or may not need to close values (marked as to-be-closed), or even out of multiple
/// scopes; and we don't know this until we find the label we're looking for.
///
/// ```lua
/// do
///   local x <close> = foo()
///   do
///     local y <close> = foo()
///     goto somewhere -- A
///     local z <close> = foo()
///   end -- B
///   local something = bar()
///   ::somewhere:: -- C
///   -- ...
/// end
/// ```
///
/// We handle this as follows:
/// - When we find the `goto` statement, we haven't found the label yet, so this struct gets
///   created, along with a Jmp instruction with the undecided address in [`Self::addr`].
/// - At `B` the scope ends. We know that `y` was in-scope (from [`Self::current_register`]), so
///   we'll generate: an undecided jump (A), a `Close <scope_starting_register` to close `y`, and a
///   new jump for the goto (patching [`Self::addr`]). Then we'll path jump `A` so regular flow
///   that doesn't hit the `goto` can jump over the generated close instruction. We update
///   [`Self::current_register`] to match the starting register of the scope.
/// - At `C`, we find the pending goto, and patch the jump address. Based on the updated
///   `current_register`, we know that `something` is a new local. This is fine, _unless_ the block
///   contains any more statements. If this happens, we'll raise an error.
///
/// Note that gotos cannot jump into a narrower scope, which is why we keep both [`Self::depth`]
/// and [`Self::scope_id`]. Then, when encountering a label, we can verify the current depth is
/// less than or equal to the goto's depth, and that the goto's scope id is part of the current
/// scope stack (and not an unrelated depth).
///
/// For example, this is okay:
///
/// ```lua
/// do
///   do
///     do
///       goto foo
///     end
///   end
///   ::foo::
/// end
/// ```
///
/// While this is not:
///
/// ```lua
/// do
///   do
///     goto foo
///   end
///   do
///     -- depth is the same, but the goto's scope id is no longer accessible
///     ::foo::
///   end
/// end
/// ```
#[derive(Debug)]
pub(crate) struct Goto {
    pub(crate) addr: JumpAddr,
    pub(crate) span: Span,
    pub(crate) current_register: u8,
    pub(crate) depth: u8,
    pub(crate) scope_id: usize,
}

#[derive(Debug)]
pub(crate) struct Label {
    pub(crate) addr: JumpAddr,
    pub(crate) current_register: u8,
    pub(crate) depth: u8,
}
