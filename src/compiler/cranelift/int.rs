use cranelift::codegen::ir::{InstBuilder, Value, condcodes::IntCC};

impl super::CraneliftCompilerInner<'_, '_> {
    pub(super) fn emit_is_small_int(&mut self, bits: Value) -> Value {
        const MASK: i64 = 0xFFFE_0000_0000_0000_u64 as i64;
        const EXPECTED: i64 = 0xFFF6_0000_0000_0000_u64 as i64;

        let masked = self.builder.ins().band_imm_u(bits, MASK);

        self.builder
            .ins()
            .icmp_imm_u(IntCC::Equal, masked, EXPECTED)
    }

    pub(super) fn emit_decode_small_int(&mut self, bits: Value) -> Value {
        let shifted = self.builder.ins().ishl_imm_u(bits, 15);
        self.builder.ins().sshr_imm_u(shifted, 15)
    }

    pub(super) fn emit_encode_small_int(&mut self, integer: Value) -> Value {
        const TAG: i64 = 0xFFF6_0000_0000_0000_u64 as i64;
        const PAYLOAD_MASK: i64 = 0x0001_FFFF_FFFF_FFFF;

        let payload = self.builder.ins().band_imm_u(integer, PAYLOAD_MASK);

        self.builder.ins().bor_imm_u(payload, TAG)
    }

    pub(super) fn emit_fits_small_int(&mut self, integer: Value) -> Value {
        let min =
            self.builder
                .ins()
                .icmp_imm_u(IntCC::SignedGreaterThanOrEqual, integer, -(1_i64 << 48));

        let max =
            self.builder
                .ins()
                .icmp_imm_u(IntCC::SignedLessThanOrEqual, integer, (1_i64 << 48) - 1);

        self.builder.ins().band(min, max)
    }
}
