use crate::{ast, instruction::Instruction, value, vm::VM};

pub struct Compiler {
    vm: VM,
}

impl Compiler {
    pub fn new() -> Self {
        Self { vm: VM::new() }
    }

    pub fn compile(mut self, ast: crate::ast::Block) -> VM {
        self.compile_block(ast);
        self.vm.push_instruction(Instruction::Halt);

        self.vm
    }

    fn compile_block(&mut self, ast: ast::Block) {
        for statement in ast.statements {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: crate::ast::Statement) {
        match statement {
            crate::ast::Statement::FunctionCall(function_call) => {
                self.compile_function_call(function_call);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_function_call(&mut self, function_call: crate::ast::FunctionCall) {
        for argument in function_call.args {
            self.compile_expression(argument);
        }

        match &*function_call.function {
            ast::PrefixExpression::Variable(ast::Variable::Name(name))
                if name.identifier == "print" =>
            {
                self.vm.push_instruction(Instruction::Print);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&mut self, expression: crate::ast::Expression) {
        match expression {
            crate::ast::Expression::Literal(literal) => {
                self.compile_load_literal(literal);
            }
            crate::ast::Expression::BinaryOp { op, lhs, rhs } => {
                self.compile_expression(*lhs);
                self.compile_expression(*rhs);
                self.compile_binary_operator(op);
            }
            crate::ast::Expression::PrefixExpression(function_call) => {
                self.compile_prefix_expression(function_call);
            }
            crate::ast::Expression::UnaryOp { op, rhs } => {
                self.compile_expression(*rhs);
                match op {
                    crate::ast::UnaryOperator::Neg => self.vm.push_instruction(Instruction::Neg),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn compile_prefix_expression(&mut self, prefix_expression: crate::ast::PrefixExpression) {
        match prefix_expression {
            crate::ast::PrefixExpression::FunctionCall(function_call) => {
                self.compile_function_call(function_call);
            }
            crate::ast::PrefixExpression::Parenthesized(expression) => {
                self.compile_expression(*expression);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_load_literal(&mut self, literal: crate::ast::Literal) {
        let const_index = match literal {
            crate::ast::Literal::Nil => self.vm.register_const(value::LuaConst::Nil),
            crate::ast::Literal::Boolean(b) => self.vm.register_const(value::LuaConst::Boolean(b)),
            crate::ast::Literal::Number(ast::Number::Float(f)) => self
                .vm
                .register_const(value::LuaConst::Number(value::LuaNumber::Float(f))),
            crate::ast::Literal::Number(ast::Number::Integer(i)) => self
                .vm
                .register_const(value::LuaConst::Number(value::LuaNumber::Integer(i))),
            crate::ast::Literal::String(s) => self.vm.register_const(value::LuaConst::String(s)),
        };

        self.vm.push_instruction(Instruction::LoadConst);
        self.vm.push_instruction(const_index);
    }

    fn compile_binary_operator(&mut self, op: crate::ast::BinaryOperator) {
        match op {
            crate::ast::BinaryOperator::Add => self.vm.push_instruction(Instruction::Add),
            crate::ast::BinaryOperator::Sub => self.vm.push_instruction(Instruction::Sub),
            crate::ast::BinaryOperator::Mul => self.vm.push_instruction(Instruction::Mul),
            crate::ast::BinaryOperator::Div => self.vm.push_instruction(Instruction::Div),
            crate::ast::BinaryOperator::Pow => self.vm.push_instruction(Instruction::Pow),
            // crate::ast::BinaryOperator::Concat => self.vm.push_instruction(Instruction::Concat),
            _ => unimplemented!(),
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
