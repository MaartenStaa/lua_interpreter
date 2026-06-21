use std::borrow::Cow;

use crate::{
    LuaState,
    error::{LuaError, PendingCallFrame, lua_error},
    value::LuaValue,
    vm::call_frame::{CallFrameKind, PoppedCallFrame},
};

impl LuaState {
    pub(crate) fn print_backtrace(&mut self, err: &mut LuaError) {
        let print_until = if err.origin.is_some() {
            self.call_stack_index
        } else {
            self.call_stack_index - 1
        }
        .min(self.call_stack_index);

        // Attach stack trace
        for (i, frame) in self.call_stack[0..print_until].iter().enumerate() {
            let frame_err = self.error_for_frame(frame.kind, i);

            eprint!("{frame_err:?}");
        }

        // Print pending frames too. Note that they're in order of how they were popped, so we'll
        // reverse them to print in call order.
        for pending_frame in err.pending_frames.iter().rev() {
            let frame_err = self.error_for_frame(pending_frame.kind, pending_frame.index);

            eprint!("{frame_err:?}");
        }

        // Attach source code to the original error if possible
        let origin_kind = if let Some(origin_kind) = err.origin {
            origin_kind
        } else {
            // In this case, the error originated from the current top call frame.
            self.call_frame().kind
        };

        if let CallFrameKind::Lua {
            chunk: chunk_idx,
            ip,
        } = origin_kind
        {
            self.with_chunk(chunk_idx, |chunk| {
                if let Some(span) = chunk.instruction_spans.get(&ip) {
                    let source = chunk.named_source();
                    err.set_source_code(source).set_labels(*span);
                }
            });
        }

        eprintln!("{err:?}");
    }

    /// If an error occurred and we want to propagate it up and pop call frames until it's aligned
    /// with the original call stack index, we can use this function to do so. The error will be
    /// modified so all intermediate call frames are captured in the error.
    pub(crate) fn pop_call_frames_into_error(
        &mut self,
        original_call_stack_index: usize,
        mut error: LuaError,
    ) -> Box<LuaError> {
        let frames_to_pop = self.call_stack_index - original_call_stack_index;
        assert!(
            frames_to_pop > 0,
            "no call frames to pop (original: {}, current: {})",
            original_call_stack_index,
            self.call_stack_index
        );

        let PoppedCallFrame { kind, .. } = self.pop_call_frame();
        error.origin = Some(kind);

        for _ in 0..(frames_to_pop - 1) {
            // NOTE: Since we call pop_call_frame(), the call_stack_index is decremented
            // every iteration, before we use it here.
            let index = self.call_stack_index - 1;
            let popped_frame = self.pop_call_frame();
            error
                .pending_frames
                .push(PendingCallFrame::new(popped_frame.kind, index));
        }

        Box::new(error)
    }

    pub(crate) fn op_err(
        &self,
        op_name: &str,
        a: &LuaValue,
        b: Option<&LuaValue>,
    ) -> Box<LuaError> {
        lua_error!(
            "cannot {op_name} a '{a_type}'{and_b}",
            a_type = a.type_name(),
            and_b = if let Some(b) = b {
                Cow::Owned(format!(" and a '{}'", b.type_name()))
            } else {
                Cow::Borrowed("")
            }
        )
    }

    fn error_for_frame(&self, kind: CallFrameKind, index: usize) -> LuaError {
        match kind {
            CallFrameKind::Lua {
                chunk: chunk_idx,
                ip,
            } => {
                let (chunk_name, span, source) = self
                    .with_chunk(chunk_idx, |chunk| {
                        (
                            chunk.name.clone(),
                            chunk.instruction_spans.get(&ip).copied(),
                            chunk.named_source(),
                        )
                    })
                    .unwrap();
                let mut err =
                    LuaError::new(format!("#{index} {}", String::from_utf8_lossy(&chunk_name)));

                // The initial error should already have a span attached, beyond
                // that we want to ensure that we attach labels for every frame
                // of the call stack.
                // Note that on the frames:
                // - We have the name, as used above.
                // - We have the `caller_addr`, which refers to from where that frame was invoked.
                //   Combine that with the `chunk` on the parent frame, to find the actual
                //   location.
                // - We skip the top item in the call stack (see above), as there's no source for
                //   that "call".
                if let Some(span) = span {
                    err = err.with_source_code(source).with_labels(span);
                }

                err
            }
            CallFrameKind::Native { name } => LuaError::new(format!("#{index} <native> {name}")),
        }
    }
}
