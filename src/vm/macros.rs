macro_rules! handle_execution_result {
    ($self:ident, $result:expr, $dest_r:expr) => {
        match $result {
            $crate::state::ExecutionResult::Completed(mut values) => {
                if values.is_empty() {
                    LuaValue::NIL
                } else {
                    values.swap_remove(0)
                }
            }
            result @ $crate::state::ExecutionResult::Yielded(_) => {
                $self.status = $crate::state::ThreadStatus::Suspended {
                    resume_args_register: Some($dest_r),
                };
                $self.call_frame_mut().inc_ip();
                return Ok(result.into());
            }
        }
    };
}

pub(super) use handle_execution_result;
