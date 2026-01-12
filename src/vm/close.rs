use crate::{
    error::LuaError,
    value::{
        LuaValue,
        callable::{Callable, Method},
    },
    vm::VM,
};

impl<'source> VM<'source> {
    #[inline]
    pub(crate) fn close(
        &mut self,
        from_register: u8,
        error: Option<&LuaError>,
    ) -> crate::Result<()> {
        // This close is unfortunate, but otherwise the borrow-checker won't be happy
        // with us calling `run_closure` or `run_native_function` below.
        let to_close = self.call_stack[self.call_stack_index - 1].to_close.clone();

        // Iterate in reverse order (LIFO)
        let mut stopped_at = 0;
        for (index, register) in to_close.into_iter().enumerate().rev() {
            if register < from_register {
                stopped_at = index + 1;
                break;
            }

            let value = self.get(register);
            if let LuaValue::Nil | LuaValue::Boolean(false) = value {
                // nil and false are ignored as to-be-closed values
                continue;
            }

            if let Some(__close) = value.get_metavalue(&crate::value::metatables::CLOSE_KEY) {
                let close: Callable = (&__close)
                    .try_into()
                    .map_err(|_| self.err("__close metamethod must be a callable value"))?;

                let error = error
                    .map(|e| e.message.clone().into_bytes().into())
                    .unwrap_or(LuaValue::Nil);
                let args = if close.is_metamethod {
                    vec![__close, value, error]
                } else {
                    vec![value, error]
                };
                match close.method {
                    Method::Closure(closure) => {
                        self.run_closure(closure, args)?;
                    }
                    Method::NativeFunction(_, func) => {
                        self.run_native_function(func, args)?;
                    }
                }
            } else {
                return Err(self.err("variable marked for closing has a non-closable value"));
            }
        }

        // Remove closed registers from to_close list
        self.call_stack[self.call_stack_index - 1]
            .to_close
            .truncate(stopped_at);
        self.clear_registers_from(from_register);

        Ok(())
    }
}
