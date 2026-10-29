use cranelift::codegen::ir::{InstBuilder, Value, types};

use crate::resolved_ast::{self, BinaryOperator, Block, Expression, Literal, Statement, TokenTree};

impl super::CraneliftCompilerInner<'_, '_> {
    pub(super) fn compile_binary_op(
        &mut self,
        op: &TokenTree<BinaryOperator>,
        lhs: Value,
        rhs: Value,
    ) -> crate::Result<Option<Value>> {
        match &op.node {
            BinaryOperator::Add => {
                let fast = self.builder.create_block();
                let slow = self.builder.create_block();
                let encode = self.builder.create_block();
                let merge = self.builder.create_block();

                // Both operands must be small integers.
                let lhs_small = self.emit_is_small_int(lhs);
                let rhs_small = self.emit_is_small_int(rhs);
                let both_small = self.builder.ins().band(lhs_small, rhs_small);

                self.builder.ins().brif(both_small, fast, &[], slow, &[]);

                // FAST: decode, add, and check if we can encode inline.
                self.builder.switch_to_block(fast);

                let a = self.emit_decode_small_int(lhs);
                let b = self.emit_decode_small_int(rhs);
                let sum = self.builder.ins().iadd(a, b);

                let fits = self.emit_fits_small_int(sum);

                self.builder
                    .ins()
                    .brif(fits, encode, &[sum.into()], slow, &[]);

                // Encode the result as a small integer.
                self.builder.append_block_param(encode, types::I64);
                self.builder.switch_to_block(encode);

                let sum = self.builder.block_params(encode)[0];
                let result = self.emit_encode_small_int(sum);

                self.builder.ins().jump(merge, &[result.into()]);

                // SLOW: delegate to the Lua runtime.
                self.builder.switch_to_block(slow);

                let result = tc!(self.emit_add_slow(lhs, rhs));

                self.builder.ins().jump(merge, &[result.into()]);

                // MERGE: both branches produce a LuaValue (raw i64).
                self.builder.append_block_param(merge, types::I64);
                self.builder.switch_to_block(merge);

                Ok(Some(self.builder.block_params(merge)[0]))
            }
            b => {
                eprintln!("binary op not yet supported: {b:#?}");
                Ok(None)
            }
        }
    }

    fn emit_add_slow(&mut self, lhs: Value, rhs: Value) -> crate::Result<Option<Value>> {
        let call = self
            .builder
            .ins()
            .call(self.runtime.binary_add, &[self.state, lhs, rhs]);

        Ok(Some(self.builder.inst_results(call)[0]))
    }
}
