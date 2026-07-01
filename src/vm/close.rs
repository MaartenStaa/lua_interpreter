use crate::{
    LuaState,
    error::{LuaError, lua_error},
    value::{
        LuaValue,
        callable::{Callable, Method},
    },
};

impl LuaState {
    #[inline]
    pub(crate) fn close(
        &mut self,
        from_register: u8,
        error: Option<&LuaError>,
    ) -> crate::Result<()> {
        // This clone is unfortunate, but otherwise the borrow-checker won't be happy
        // with us calling `run_closure` or `run_native_function` below.
        let to_close = self.call_frame().to_close.clone();

        // Iterate in reverse order (LIFO)
        let mut stopped_at = 0;
        for (index, register) in to_close.into_iter().enumerate().rev() {
            if register < from_register {
                stopped_at = index + 1;
                break;
            }

            let value = self.get(register);
            if value.is_nil() || (value.is_bool() && !unsafe { value.as_bool() }) {
                // nil and false are ignored as to-be-closed values
                continue;
            }

            if let Some(__close) = value.get_metavalue(&crate::value::metatables::CLOSE_KEY) {
                let close: Callable = (&__close)
                    .try_into()
                    .map_err(|_| lua_error!("__close metamethod must be a callable value"))?;

                let error = error
                    .map(|e| e.message.clone().into_bytes().into())
                    .unwrap_or(LuaValue::NIL);
                let args = if close.is_metamethod {
                    vec![__close, value, error]
                } else {
                    vec![value, error]
                };
                match close.method {
                    Method::Closure(closure) => {
                        let num_args = args.len();
                        let args_r = self.extend_stack(LuaValue::NIL, args);
                        self.run_closure(&closure, args_r, num_args)?;
                    }
                    nf @ Method::NativeFunction(name, func) => {
                        let num_args = args.len();
                        let args_r = self.extend_stack(nf, args);
                        self.run_native_function(name, func, args_r, num_args)?;
                    }
                }
            } else {
                return Err(lua_error!(
                    "variable marked for closing has a non-closable value"
                ));
            }
        }

        // Remove closed registers from to_close list
        self.call_frame_mut().to_close.truncate(stopped_at);
        self.clear_registers_from(from_register);

        Ok(())
    }
}
