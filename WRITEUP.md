# Writing a Lua bytecode VM

This is a writeup of my experience writing a Lua bytecode VM in Rust, during the
Q3 hackathon in Client Platform. The goal of this project was to learn more
about:

- Lexing & parsing
- Bytecode VMs (which I'd never done before)
- Rust (which I love and am always trying to improve)
- Lua (which I use to configure Neovim)

In hindsight, this was not a one-week project by any stretch of the imagination,
but while I didn't get as far as I'd hoped, I did learn a lot and accomplished
the goals above, at least to some extent.

The inspiration for this project came from a video on YouTube titled
[Implementing a Lox interpreter in Rust][yt-video], by Jon Gjengset[^1]. Jon
creates a lot of different videos on Rust, and I've learned a lot from them. In
this video, he goes through the process of writing an interpreter for the
fictional Lox programming language, from the book [Crafting
Interpreters][crafting-interpreters]. I thought it would be fun to do something
similar, but with a "real" language instead, as I think it's more interesting to
leave room for figuring out how to do things, rather than just following the
exercices in the book.

## Lexing

The first step to writing any sort of program that takes source code as its
input, whether it is a linter, an interpreter, a bytecode VM, or a straight-up
compiler, is lexing. Lexing is the process of taking a string of characters and
turning it into a list of tokens, which are the smallest units of meaning in the
language. For example, in the Lua language, the string `local x = 42` would be
lexed into the following tokens:

- `Local` (a keyword)
- `Identifier("x")`
- `Equals`
- `Integer(42)`

Notably, lexers make no assumptions about the meaning of the tokens they
produce, or about how they relate to each other. So while the following Lua code
is completely invalid, each individual token by itself is fine, and the lexer
will happily produce tokens for it:

```lua
local +6(if 42) = while <=
```

To write the lexer, I largely followed the approach Jon took in his video, with
the caveat that he was parsing a different language. But most importantly, I did
not use any existing lexing (or parsing) crates that are available to Rust.

The lexer is implemented as a stateful iterator, which is a common pattern for
both Rust and lexers. This means that the parser can ask for one token at a
time, rather than producing the entire list of tokens, and _then_ parsing them.
In short, the lexer looks like this (some details omitted or simplified for
brevity):

```rust
struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,
    position: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = miette::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rest.is_empty() {
            return None;
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_start = self.position;

            self.rest = chars.as_str();
            self.position += c.len_utf8();

            let state = match c {
                '+' => return Some(Ok(Token::Plus)),
                '*' => return Some(Ok(Token::Star)),
                // ...

                '/' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '/',
                        next_matches: Token::SlashSlash,
                    },
                    option_b: None,
                    next_does_not_match: Token::Slash,
                },
                // ...
                '<' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '=',
                        next_matches: Token::LessEquals,
                    },
                    option_b: Some(MultiCharacterOperatorOption {
                        next: '<',
                        next_matches: Token::ShiftLeft,
                    }),
                    next_does_not_match: Token::Less,
                },
                // ...

                '.' => State::Dot,
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => State::Ident,
                b'"' | b'\'' => State::String,
                b'0'..=b'9' => State::Number,

                c if c.is_ascii_whitespace() => continue,

                _ => {
                    // return an error
                }
            };

            break Some(match state {
                // ...
            })
        }
    }
}
```

A few things to note about this code:

- As mentioned, the lexer is implemented as an iterator, which means that it
  produces one token at a time.
- The lexer keeps track of its position in the source code, and holds both a
  reference to the full source code (used when printing errors) and a
  reference to the remaining source code to lex (equivalent to
  `&self.source[self.position..]`.
- While some tokens are single characters, others are multi-character tokens,
  such as `<=` or `==`. These are handled by a state machine that keeps track
  of the current state of the lexer. The `MaybeMultiCharacterOperator` state
  peeks the next character, and can return one of three different tokens,
  depending on which option is matched.
- The lexer also handles identifiers, strings, and numbers, which are more
  complex to parse than tokens with a static length.
- I use the [`miette`][miette] crate for error handling, as it is especially
  well suited for this kind of application, where errors (often) correspond to a
  location in some source code.

Probably the most tricky part of the lexer to get right was handling strings and
numbers. Strings, like in many other languages, can contain escape sequences,
which the lexer decodes as it comes across them. Notably, though, strings in Lua
can also be so-called long-form strings, which can span multiple lines, and have
a variable delimiter:

```lua
local long_string = [==[
This is a long string
]]===] <- this does not end the string
that spans multiple lines
]==]
```

Numbers in Lua can be integers or floats, and can be written in decimal or
hexadecimal, and can contain an exponent. Or, as the spec says:

> A numeric constant (or numeral) can be written with an optional fractional
> part and an optional decimal exponent, marked by a letter 'e' or 'E'. Lua also
> accepts hexadecimal constants, which start with 0x or 0X. Hexadecimal
> constants also accept an optional fractional part plus an optional binary
> exponent, marked by a letter 'p' or 'P' and written in decimal. (For instance,
> 0x1.fp10 denotes 1984, which is 0x1f / 16 multiplied by 210.)
>
> A numeric constant with a radix point or an exponent denotes a float;
> otherwise, if its value fits in an integer or it is a hexadecimal constant, it
> denotes an integer; otherwise (that is, a decimal integer numeral that
> overflows), it denotes a float. Hexadecimal numerals with neither a radix
> point nor an exponent always denote an integer value; if the value overflows,
> it wraps around to fit into a valid integer.

Let's just say getting the semantics of all of this exactly right was quite a
hassle, but we got there in the end.

## Testing

Before moving on to parsing, let's discuss testing. Writing all this code is
fun, but how do we know we got it right? Well, [the Lua manual][lua-manual]'s
got a bunch of sample inputs (especially for number literals, which is great),
which served as a source for some unit tests. Additionally though, the
[lua/lua][github-lua] repository has a directory called `testes`, containing
test files written in Lua. Not only do these serve us great later on to test the
VM as a whole, it's also a great source of test cases for the lexer and parser.
Simply running the Lexer using the test files as input, surfaced many nuances
that I got wrong. Since these are basically end-to-end tests written in Lua,
they should not contain any lexing or parsing errors[^2].

## Parsing

Great! Now that we have a sequence of tokens, we can move on to parsing, which
is essentially the process of turning the tokens produced by the lexer into an
abstract syntax tree (AST). The AST is a tree-like structure that represents the
structure of the program, and is used to generate the bytecode that the VM will
execute.

Going back to the example from the lexer section, the AST for the Lua code
`local x = 42` would look something like this (again, details omitted for
brevity—in particular, the AST also holds information about the source spans;
the location in the source code that a particular node corresponds to):

```rust
Block {
    statements: [
        Statement::LocalDeclaraction {
            names: [
                AttributedName {
                    name: Name("x"),
                    attribute: None,
                },
            ],
            values: [
                Expression::Literal(
                    Literal::Number(
                        Number::Integer(
                            42,
                        ),
                    ),
                ),
            ],
        }
    ],
    return_statement: None,
}
```

The most common type of parser is a recursive descent parser, which is a type of
top-down parser that starts at the top of the grammar and works its way down to
the leaves. The advantage of recursive descent parsers is that they are easy to
write by hand, and they are easy to understand. The disadvantage is that they
are not as efficient as other types of parsers, and they can be difficult to
write for languages with ambiguous grammars.

As opposed to the Crafting Interpreters book, which uses a recursive descent
parser, Jon Gjengset's video uses a Pratt parser, which is a type of top-down
parser that is more efficient than a recursive descent parser. The advantage of
Pratt parsers is that they are easy to write by hand, and they are easy to
understand. The disadvantage is that they may be not as efficient as other types
of parsers. I liked how easy Pratt parsing made it to handle operator
precedence, and followed the blog post [Simple but Powerful Pratt
Parsing][pratt-parsing] by Alex Kladov, which explains the concept in a very
clear and concise way.

One aspect of Jon's video I did not like, was that he seemed to be using Pratt
parsing for everything, including parsing statements, whereas I think it's more
appropriate to use it just for parsing expressions, which is what I opted for.

As opposed to the lexer, the parser is not an iterator. After all, the output of
the parser is a single AST, not a sequence of objects of some kind. The
structure looks roughly like this:

```rust
struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    // The top-level node of a Lua program is a block
    fn parse(&mut self) -> miette::Result<Block> {
        let result = self.parse_block();

        // Ensure that we've consumed all tokens
        if self.lexer.next().is_some() {
            return Err(miette!("Unexpected token, expected EOF"));
        }

        Ok(result)
    }

    fn parse_block(&mut self) -> Block {
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }

        let return_statement = if self.lexer.peek().is_some() {
            Some(self.parse_return_statement()?)
        } else {
            None
        };

        Block {
            statements,
            return_statement,
        }
    }

    fn parse_statement(&mut self) -> miette::Result<Option<Statement>> {
        loop {
            let next_token = self.lexer.peek()?;
            break match next_token {
                Some(Token::Semicolon) => {
                    // Delimiter of statements, skip--also fine to have multiple
                    self.lexer.next();
                    continue;
                }
                Some(Token::Break) => {
                    self.lexer.next();
                    Ok(Some(Statement::Break))
                },
                Some(Token::While) => {
                    self.lexer.next();
                    Ok(Some(Statement::While(self.parse_while()?))
                }
                // ...
                Some(
                    Token::Return
                    | Token::End
                    | Token::ElseIf
                    | Token::Else
                    | Token::Until
                ) => {
                    // Handled by the block parser or outside of it
                    Ok(None)
                }
                Some(_) => {
                    Err(miette!("Unexpected token, expected statement"))
                }

                // Reached EOF
                None => Ok(None),
            }
        }
    }

    fn parse_while(&mut self) -> miette::Result<While> {
        let condition = self.expect_expression()?;
        self.lexer.expect(|t| t == &Token::Do)?;
        let block = self.parse_block()?;
        self.lexer.expect(|t| t == &Token::End)?;

        Ok(While { condition, block })
    }
}
```

And so on. In short, we check one token at a time, and choose which path to go
down, and then expect certain tokens. For example, you will note that the parser
asserts that a `while` token is followed by an expression, then a `do` keyword,
then a block, and finally an `end` keyword. If any of these are missing, an
error is returned and shown to the user. This is a very common pattern in
recursive descent parsers, and is used to enforce the grammar of the language.

You may have also noticed that the excerpt above calls some new methods on the
lexer, such as `peek` and `expect`. These are helper methods that make it easier
to work with the lexer, allowing to check ahead what the next token is going to
be, or to assert that the next token is a certain type. Keeping track of this in
the parser itself would be more cumbersome.

However, I promised you Pratt parsing, which is used to parse expressions. The
Pratt parser works with the concept of a "binding power" for each operator,
which is just a more convenient way to to express operator precedence (read
Alex's article for more details).

Again, omitting details, parsing expressions looks like this:

```rust
impl<'a> Parser<'a> {
    fn parse_expression_within(&mut self, min_bp: u8) -> miette::Result<Expression> {
        let mut lhs = match self.lexer.peek()? {
            Some(Token::Identifier(_) | Token::OpenParen) => {
                Expression::PrefixExpression(self.parse_prefix_expression()?)
            }
            Some(Token::Nil) => {
                self.lexer.next();
                Expression::Literal(Literal::Nil)
            }
            Some(Token::True) => {
                self.lexer.next();
                Expression::Literal(Literal::Boolean(true))
            }
            // ...
            Some(Token::Minus) => { // This is a prefix operator
                self.lexer.next();
                let ((), r_bp) = self.get_prefix_binding_power(Token::Minus);
                let rhs = self.parse_expression_within(r_bp)?;

                Expression::UnaryOperation {
                    operator: UnaryOperator::Minus,
                    rhs: Box::new(rhs),
                }
            }
        };

        loop {
            let op = match self.lexer.peek()? {
                Some(token) if token.kind.is_operator() => token,
                //...
                _ => break,
            };

            if let Some((l_bp, r_bp)) = self.get_infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();
                let rhs = self.parse_expression_within(r_bp);
                lhs = Expression::BinaryOperation {
                    lhs: Box::new(lhs),
                    operator: BinaryOperator::from(&op),
                    rhs: Box::new(rhs?),
                };
            }
        }

        Ok(Some(lhs))
    }

    fn get_prefix_binding_power(&self, token: Token) -> ((), u8) {
        match token {
            Token::Not | Token::Minus => ((), 21),
            _ => unreachable!(),
        }
    }

    fn get_infix_binding_power(&self, token: &Token) -> Option<(u8, u8)> {
        Some(match token {
            Token::Or = (1, 2),
            Token::And = (3, 4),
            // ...
            Token::DotDot => (16, 15), // Right associative
            Token::Plus | Token::Minus => (17, 18),
            // ...

            _ => return None,
        })
    }
}
```

So what are we looking at here? The first thing to note is that the Pratt parser
is primarily iterative, and only secondarily recursive, as opposed to a
recursive descent parser. It can also be a bit tricky at first to tell what is
going on exactly. Roughly speaking, the process looks like this:

1. Parse the left-hand side of the expression, which is either a literal, a
   variable, or a prefix operator.
2. If it's a prefix operator, the right-hand side gets parsed right away.
3. We then enter a loop, where we check if the next token is an operator.
4. If it is, we check if it is an infix operator, and if it has a higher binding
   power than the minimum binding power we are allowed to parse. In other words,
   this is the part that handles operator precedence.
5. If the operator is an infix operator, we parse the right-hand side of the
   expression, and create a new binary operation node in the AST.
6. Now we loop! Now that we've got a binary operation such as `a + b`, we go
   back to step 3, and check if there is another operator to the right of `b`.
   If so, that may turn the new value of `lhs` into a binary operation like `(a
   - b) \* c`, and so on.

Lua does not have any unary suffix operators, so we do not need to worry about
that. Otherwise, that would be handled in the loop, right before the infix
operators (again, see Alex's excellent article for more details).

You may have noticed the reference to a so-called prefix expression. This is a
syntax construction in Lua, which puts some constraints on how expressions can
be written. In short, a prefix expression may only be a variable reference, a
function call, or a parenthesized expression.

## Optimisation

Wait, I hear you thinking, weren't we about to write a bytecode VM? Why yes, and
we'll get there soon, but I took a slight detour to write an optimiser for the
AST. Now, you could make all sorts of analyses and optimisations given an AST,
but I chose to implement one (arguably the simplest): constant folding. This
means taking any expression that can be evaluated at compile time, and replacing
it with the result of that evaluation. For example, the expression `1 + 2` would
be replaced with `3`, and the expression `2 * 3 + 4` would be replaced with
`10`. This way, we don't have to evaluate these expressions at runtime, which
_might_ save us some time, especially if this ends up in a loop on the critical
path.

The optimiser is implemented as a simple recursive function that traverses the
AST, and replaces any expression that can be evaluated with the result of that
evaluation. The optimiser is run after parsing, but before generating the
bytecode. It looks something like this:

```rust
fn optimize_block(block: Block) -> Block {
    let statements = block.statements
        .into_iter()
        .map(|statement| optimize_statement(statement))
        .collect();
    let return_statement = block.return_statement
        .map(|return_statement| optimize_return_statement(return_statement));

    Block {
        statements,
        return_statement,
    }
}

// ...

fn optimize_expression(expression: Expression) -> Expression {
    match expression {
        Expression::BinaryOperation { lhs, operator, rhs } => {
            optimize_binary_operation(lhs, operator, rhs)
        },

        // ...
    }
}

fn optimize_binary_expression(lhs: Expression, operator: BinaryOperator, rhs: Expression) -> Expression {
    let lhs = optimize_expression(lhs);
    let rhs = optimize_expression(rhs);

    match (lhs, operator, rhs) {
        (
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            BinaryOperator::Plus,
            Expression::Literal(Literal::Number(Number::Integer(rhs)))
        ) => {
            Expression::Literal(Literal::Number(Number::Integer(lhs + rhs)))
        },
        (
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            BinaryOperator::Minus, Expression::Literal(Literal::Number(Number::Integer(rhs)))) => {
            Expression::Literal(Literal::Number(Number::Integer(lhs - rhs)))
        },
        // ...
        _ => Expression::BinaryOperation { lhs, operator, rhs },
    }
}
```

Is this the most efficient way to do constant folding? Probably not. But it's
simple, and most importantly: it works.

## Bytecode VM

Finally, we get to the bytecode VM. Before we dive into the details, let's
clarify what a bytecode VM is. A bytecode VM is a virtual machine that executes
bytecode, which is a low-level representation of a program. The bytecode is
generated by a compiler, and is designed to be easy to interpret by the VM.

Woah, okay, a lot of words there. Let's break it down. Think of the device
you're reading this on right now. Whether that's a laptop, a phone, or something
else, it contains a processor, let's say an Apple Silicon processor. This
processor is running low-level machine code, which is a series of instructions
(bytes) that the processor understands.

The upside of this is that it's _extremely_ fast. The downside is that it's
very hard to write, and that (importantly) it's not portable. In other words, if
you compile a program to machine code on an Apple Silicon processor, with the
ARM64 instruction set, it won't run on an Intel x64 processor, nor
vice-versa[^3]. The solution to this problem is to use a bytecode VM, which
is a program pretending to be a processor, that runs a series of instructions
which _are_ portable. This is basically what Java does too, and why you can run
Java programs on any device that has a JVM (Java Virtual Machine).

With that out of the way, we know what we're building. We're taking the AST, and
turning into a flat list of instructions (encoded as bytes), which the VM
understands and can execute. At this point, we can also keep referring back to
both the [Lua manual][lua-manual] and the [Crafting Interpreters
book][crafting-interpreters] for guidance on how to structure the bytecode. For
example, we need a notion of **values**, **variables**, and so forth. The Lua
manual can tell us what kind of values Lua has, e.g. integers, floats, strings,
and so forth, and their semantics. The book, in the meantime, can give us some
ideas on how to structure the bytecode, how to execute it, and where values live
(spoiler alert: on a stack for local variables, and in a table for globals[^4]).

The first step is to come up with our instruction set. This is the list of
instructions that the VM understands, and we'll generate in the compiler.
Luckily, Rust makes it easy to define an enum for this, which can be represented
as a byte:

```rust
#[derive(Debug)]
#[repr(u8)]
enum Instruction {
    // Stack operations
    LoadConst,
    Pop,

    // Binary operations
    Add,
    Sub,
    Mul,
    // ...

    And,
    Or,

    // Variables
    SetGlobal,
    GetGlobal,
    SetLocal,
    GetLocal,

    Call,
    Return,
    Jmp,
    JmpTrue,

    Error,
}
```

For some of these instructions, we need to pass additional information, such as
a reference to a constant value (an index into a constant table), or the index
of a local variable. This is done by adding additional bytes to the instruction,
which are read by the VM when executing that instruction, and it will then jump
over (so that it doesn't try to execute the constant index as an instruction).

At this point, we can start making a mental model of what a set of instructions
will look like for a given program, and then generate that from the compiler.

For example, take the following (very simple) Lua program:

```lua
print(3 + 4 * 5)
```

In the very first versions of the VM, I had a dedicated `PRINT` instruction (in
order to not have to deal with the semantics of function calls yet), as well as
a `HALT` instruction (no function calls means no `RETURN` instruction yet
either). The bytecode for this program would then look something like this
(ignoring constant folding):

```
00: LOAD_CONST 0 // Load the constant 3
02: LOAD_CONST 1 // Load the constant 4
04: LOAD_CONST 2 // Load the constant 5
05: MUL          // Multiply 4 and 5
06: ADD          // Add 3 and the result of the multiplication
07: PRINT        // Print the result
08: HALT         // Stop execution
```

I've included the indices of the instructions here for clarity. The VM would
then execute these instructions in order, and use the stack to keep track of
intermediate values. For example, the `MUL` instruction would pop the two
previous values from the stack, multiply them, and push the result back onto the
stack. So the VM at this stage would look something like this:

```rust
struct VM {
    instructions: Vec<Instruction>,
    constants: Vec<ConstValue>,
    stack: Vec<Value>,
}

impl VM {
    fn run(&self) {
        let mut ip = 0; // ip is short of instruction pointer
        loop {
            let instruction = self.instructions[ip];
            match instruction {
                Instruction::LoadConst => {
                    let index = self.read_u8(ip + 1);
                    let value = self.constants[index as usize];
                    self.stack.push(value.into());
                    ip += 2;
                },
                Instruction::Add => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(lhs + rhs);
                    ip += 1;
                },
                // ...
                Instruction::Print => {
                    let value = self.stack.pop().unwrap();
                    println!("{}", value);
                    ip += 1;
                },
                Instruction::Halt => {
                    break;
                },
                _ => {
                    panic!("Unknown instruction: {:?}", instruction);
                }
            }
        }
    }
}
```

And the stack would look like this at each step:

```
00: [] // Start with an empty stack
02: [3]
04: [3, 4]
05: [3, 4, 5]
06: [3, 20] // 4 * 5 = 20
07: [23] // 3 + 20 = 23
08: [] // Printed the result, which consumed the value
```

### Variables

The next step is to add support for variables. In Lua, variables can be local or
global, and can be assigned to, read from, and passed to functions. The VM needs
to keep track of these variables, and their values, in order to execute the
program correctly.

In the VM, local variables are stored on the stack, with an offset from the
frame pointer, a concept we will dive into later when we implement function
calls. For now, we can just assume that the index of a local variable
corresponds to its position on the stack.

Global variables, on the other hand, are stored in a table in the VM, which maps
variable names to values. This table is shared between all functions, and is
used to store and reference global variables.

Let's look at another example, this time using both local and global variables:

```lua
local x = 3 * 4
y = 5 + x
x = x + 1
local z = x + y
print(z)
```

Here, we're declaring a local variable `x`, assigning it the value `3 * 4`, then
assigning the value `5 + x` to the global variable `y`, then assigning the value
`x + y` to the local variable `z`, and finally printing the value of `z`.

The bytecode for this program would look something like this:

```
00: LOAD_CONST 0 // Load the constant 3
02: LOAD_CONST 1 // Load the constant 4
04: MUL          // Multiply 3 and 4
05: LOAD_CONST 2 // Load the constant 5
07: GET_LOCAL 0  // Get the value of x
09: ADD          // Add 5 and x
10: SET_GLOBAL 3 // Set the value of y
12: GET_LOCAL 0  // Get the value of x
14: LOAD_CONST 4 // Load the constant 1
16: ADD          // Add x and 1
17: SET_LOCAL 0  // Set the value of x
19: GET_LOCAL 0  // Get the value of x
21: GET_GLOBAL 3 // Get the value of y
23: ADD          // Add x and y
24: GET_LOCAL 2  // Get the value of z
26: PRINT        // Print the value of z
27: HALT         // Stop execution
```

Perhaps the most surprising part here, is that there are no `SET_LOCAL`
instructions corresponding to the initial assignments of `x` and `z`. These are
not needed, as both the "temporary" we work with, and the local variables
themselves, are kept on the stack. Imagine we had the Lua code `local x = 3`,
we could implement that as follows:

```
00: LOAD_CONST 0 // Load the constant 3
02: SET_LOCAL 0  // Set the value of x
```

This would push the constant `3` onto the stack, and then pop it off again and
store it back on the stack at index 0. But `3` was _already_ on the stack at
index 0, so the `SET_LOCAL` instruction is effectively a no-op. Therefore, we
only use `SET_LOCAL` when _updating_ a pre-existing local variable.

Another thing to note is that the `GET_GLOBAL` and `SET_GLOBAL` instructions
take an index into the constant table, rather than the name of the variable
itself. At compile time, the name of the variable is registered as a constant,
so the bytecode can stay small, without any variable-length instructions (for
now...).

Just for completeness sake, this is what the constant table would look like for
the program above, and what the stack and globals table would look like after
each instruction.

Globals:

| index | value       |
| ----- | ----------- |
| 0     | int(3)      |
| 1     | int(4)      |
| 2     | int(5)      |
| 3     | string("y") |
| 4     | int(1)      |

Stack and globals after each instruction:

| instruction pointer | stack        | globals     |
| ------------------- | ------------ | ----------- |
| 00:                 | [3]          | {}          |
| 02:                 | [3, 4]       | {}          |
| 04:                 | [12]         | {}          |
| 05:                 | [12, 5]      | {}          |
| 07:                 | [12, 5, 12]  | {}          |
| 09:                 | [12, 17]     | {}          |
| 10:                 | [12]         | { "y": 17 } |
| 12:                 | [12, 12]     | { "y": 17 } |
| 14:                 | [12, 12, 1]  | { "y": 17 } |
| 16:                 | [12, 13]     | { "y": 17 } |
| 17:                 | [13]         | { "y": 17 } |
| 19:                 | [13, 13]     | { "y": 17 } |
| 21:                 | [13, 13, 17] | { "y": 17 } |
| 23:                 | [13, 30]     | { "y": 17 } |
| 24:                 | [13, 30, 30] | { "y": 17 } |
| 26:                 | [13, 30]     | { "y": 17 } |

### Function calls

Lua programs aren't that much fun if you can't call functions, so the next step
is to add support for those. After all, you can't spell function without `fun`.

In Lua, functions are first-class values, which means that they can be assigned
to variables, passed as arguments to other functions, and returned from
functions. Functions can be defined using the `function` keyword, and can take
zero or more arguments. Functions can also return zero or more values. Finally,
functions can have a special type of argument called a "vararg" argument, which
is used to collect any extra arguments that are passed to the function.

For example, consider the following Lua program:

```lua
local function add(a, b)
    return a + b, true
end

local function var(a, ...)
    local allextra = {...}
    return a, a + #allextra
end

print(var(add(3, 4), 5, 6, 7))
```

This program defines two functions, `add` and `var`, and then calls `var` with
the result of calling `add` with the arguments `3` and `4`, and the arguments
`5`, `6`, and `7`. `var` then returns both `a`, and the sum of `a` and the
number of extra arguments that were passed to it. The expected output of this
program is `7 10`. Note that the second return value of `add` is ignored. Lua
calls this a multires expression, and the Lua manual says:

> When a multires expression is used as the last element of a list of
> expressions, all results from the expression are added to the list of values
> produced by the list of expressions. Note that a single expression in a place
> that expects a list of expressions is the last expression in that (singleton)
> list.

Here, the result of `add` is a multires expression, but it's not the last
element in a list of expressions (it's followed by the constant expressions `5`,
`6`, and `7`), so any values beyond its first one are ignored. All of this means
a bunch of extra bookkeeping in the VM. Since both the number of arguments and
number of return values can vary, we need to keep track of these at runtime,
rather than e.g. encoding this in the bytecode. For this reason, I came up with
a special "marker" value, which can be stored on the stack. User code cannot
create or consume a marker value, but the VM uses it behind the scenes to keep
track of where the lists of arguments and return values start.

Bringing these concepts into practice, the bytecode for the program above would
look like this:

```
0000: LOAD_CONST      NIL      // Declare the function add
0005: JMP             0026     // Jump over the function body
0008: ALIGN           2        // Start of the function, make sure we have space for 2 locals
0010: LOAD_CONST      MARKER   // Load the marker value
0015: GET_LOCAL       0        // Load the first argument
0017: GET_LOCAL       1        // Load the second argument
0019: ADD                      // Add the two arguments
0020: LOAD_CONST      true     // Load the constant true
0025: RETURN                   // Return the sum and true
0026: LOAD_CLOSURE    FUNCTION<add:0008> // Load the function add
0031: SET_LOCAL       0        // Save it in the local variable add
0033: LOAD_CONST      NIL      // Declare the function var
0038: JMP             0067     // Jump over the function body
0041: ALIGN_VARARG    1        // Start of the function, make sure we have space for 1 local and 1 vararg
0043: NEW_TABLE                // Create a new table for the vararg
0044: LOAD_CONST      MARKER   // Load the marker value
0049: LOAD_VARARG     1 multi  // Load the vararg as a multires expression
0052: APPEND_TO_TABLE          // Append all varargs to the table
0053: LOAD_CONST      MARKER   // Load the marker value
0058: GET_LOCAL       0        // Load the first argument
0060: GET_LOCAL       0        // Load the first argument again
0062: GET_LOCAL       2        // Load the table of varargs
0064: LEN                      // Get the length of the table
0065: ADD                      // Add the first argument and the length of the table
0066: RETURN                   // Return the first argument and the sum
0067: LOAD_CLOSURE    FUNCTION<var:0041> // Load the function var
0072: SET_LOCAL       1        // Save it in the local variable var
0074: LOAD_CONST      MARKER   // Load the marker value for the "print" return values
0079: LOAD_CONST      MARKER   // Load the marker value for the "var" return values
0084: LOAD_CONST      MARKER   // Load the marker value for the "add" return values
0089: LOAD_CONST      MARKER   // Load the marker for the "add" arguments
0094: LOAD_CONST      INT 3    // Load the constant 3
0099: LOAD_CONST      INT 4    // Load the constant 4
0104: GET_LOCAL       0        // Load the function add
0106: CALL            single   // Call the function add with 3 and 4, expect a single return value
0108: LOAD_CONST      INT 5    // Load the constant 5
0113: LOAD_CONST      INT 6    // Load the constant 6
0118: LOAD_CONST      INT 7    // Load the constant 7
0123: GET_LOCAL       1        // Load the function var
0125: CALL            multi    // Call the function var with the result of add, 5, 6, and 7, allow multiple return values
0127: PRINT                    // Print the return values of var
0128: DISCARD                  // Discard the return values of print
```

Phew, that was a lot! You can see all of these markers make quite a mess of our
bytecode, and of the stack at runtime too. Unfortunately, you need to handle
this bookkeeping. This could be done in a separate data structure as well, for
example by keeping track of indices in the stack where the arguments and return
values start, but this was easier to implement and reason about. I may
restructure this in the future, but for now, it works.

Some other things to note:

- The bytecode of the functions is inline with the rest of the program. A
  different approach would be to keep the bytecode of the functions separate,
  and swap them out when calling the function. This would make the bytecode more
  compact, but would require more bookkeeping in the VM.
- The `ALIGN` and `ALIGN_VARARG` instructions are used to make sure that the
  stack is aligned correctly at the start of a function. In other words, it
  makes sure that the number of arguments passed to the function matches what
  the function expects, either discarding any extra arguments, or filling in
  missing arguments with `nil`. `ALIGN_VARARG` collects the extra arguments into
  a special local variable called `...` as a sequence table (like an array).
- The `NEW_TABLE`, `APPEND_TO_TABLE`, and `LEN` instructions are used to, in
  order, create a new table, append all values on the stack after the latest
  marker to the table, and then get the length of the table (or whichever value
  is at the top of the stack). `LEN` could, for example, also be used with
  string values.
- When defining functions, they are loaded from the constant table. All the
  function object in the constant table knows is the name of the function, and
  the instruction pointer where the function starts.

We use the `CALL` instruction to call a function that is at the top of the
stack. This instruction pushes a new frame onto the call stack, which is a
bookkeeping structure so that we can return to the correct instruction after the
function call. It also tracks whether the caller is okay to accept multiple
return values, or only a single one. Finally, it keeps a value called the "frame
pointer", which is an index into the stack where the arguments of the function
start. This means that from now on, all `GET_LOCAL` and `SET_LOCAL` instructions
will be relative to the frame pointer, rather than (by chance) matching their
absolute index[^5].

Finally, the `RETURN` instruction is used to return from a function. It pops the
top of the call stack, and updates the instruction pointer to the correct value
to return to the caller. It also pops the return values from the stack (using
the marker to find where they start), discards all values on the stack from the
start of the frame pointer, and then pushes back either only the first return
value, or all of them, depending on what the caller expects.

Let's take a look at the changes to our VM compared to the last snapshot:

```rust
struct VM {
    instructions: Vec<Instruction>,
    constants: Vec<ConstValue>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    call_stack: Vec<CallFrame>,
}

impl VM {
    fn run(&mut self) {
        let mut ip = 0;
        loop {
            let instruction = self.instructions[ip];
            match instruction {
                // ... previous instructions

                // Variable handling
                Instruction::SetGlobal => {
                    let index = self.read_u8(ip + 1);
                    let name = self.constants[index as usize].as_string().unwrap();
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(name, value);
                    ip += 2;
                },
                Instruction::GetGlobal => {
                    let index = self.read_u8(ip + 1);
                    let name = self.constants[index as usize].as_string().unwrap();
                    let value = self.globals.get(name).cloned().unwrap_or(Value::Nil);
                    self.stack.push(value);
                    ip += 2;
                },
                Instruction::SetLocal => {
                    let index = self.read_u8(ip + 1);
                    let value = self.stack.pop().unwrap();
                    let frame = self.call_stack.last().unwrap();
                    self.stack[frame.frame_pointer + index as usize] = value;
                    ip += 2;
                },
                Instruction::GetLocal => {
                    let index = self.read_u8(ip + 1);
                    let frame = self.call_stack.last().unwrap();
                    let value = self.stack[frame.frame_pointer + index as usize].clone();
                    self.stack.push(value);
                    ip += 2;
                },

                // Tables
                Instruction::NewTable => {
                    self.stack.push(Value::Table(HashMap::new()));
                    ip += 1;
                },
                Instruction::AppendToTable => {
                    let marker_index = self
                        .stack
                        .iter()
                        .rposition(|v| v == &Value::Marker)
                        .expect("no marker found");
                    let table = self.stack[marker_index - 1].clone();
                    let num_values = self.stack.len() - marker_index - 1;

                    for i in 0..num_values {
                        let value = self.pop().unwrap();
                        let index = (num_values - i) as i64;
                        table.insert(index.into(), value);
                    }

                    // Pop the marker
                    self.stack.pop();

                    ip += 1;
                },

                // Function handling
                instr @ Instruction::Align | instr @ Instruction::AlignVararg => {
                    let collect_varargs = matches!(instr, Instruction::AlignVararg);
                    let align_amount = instr_param!();

                    // ...
                }
                Instruction::Call => {
                    let function = self.stack.pop().unwrap();
                    let marker_index = self
                        .stack
                        .iter()
                        .rposition(|v| v == &Value::Marker)
                        .expect("no marker found");
                    let num_args = self.stack.len() - marker_index - 1;

                    let is_single_return = self.read_u8(ip + 1) == 1;
                    self.push_call_frame(
                        function.name,
                        self.stack.len() - num_args, // frame pointer
                        ip + 2, // return address
                        is_single_return,
                    );

                    ip = function.ip;
                }
                Instruction::Return => {
                    let frame = self.pop_call_frame();
                    let marker_index = self
                        .stack
                        .iter()
                        .rposition(|v| v == &Value::Marker)
                        .expect("no marker found");

                    // Collect the return values
                    let mut return_values: Vec<_> = (marker_index + 1..self.stack.len())
                        .map(|i| self.stack.pop().unwrap())
                        .collect();
                    return_values.reverse();

                    // Pop the marker
                    self.stack.pop();

                    ip = frame.return_address;

                    // Discard all values from the frame
                    for _ in 0..(self.stack.len() - frame.frame_pointer) {
                        self.stack.pop();
                    }

                    // Push the return values back onto the stack
                    if return_values.is_empty() && frame.is_single_return {
                        self.stack.push(Value::Nil);
                    } else {
                        for value in return_values {
                            self.stack.push(value);
                            if frame.is_single_return {
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}
```

### Closures and upvalues

### Error handling

### Native functions

## Conclusion

[^1]:
    Yes, this video is close to 8 hours long. [Welcome to Jon's
    channel][yt-long].

[^2]:
    Well, sort of. Some of these tests use Lua's `load` function, which allows
    loading additional code from a string value. It uses this to make assertions
    about edge cases in the parser. This will only matter later on, though, once
    we start actually executing the code.

[^3]:
    Not without doing some sort of translation, at least. This is what Apple's
    Rosetta 2 does, for example, which came in pretty handy when the M1 Macs
    came out, and all software was still compiled for x64.

[^4]:
    If you're a Lua expert, you may be thinking "but what about `_ENV` and `_G`?
    And you would be totally right that that is something that bit me later on.
    If you're an even bigger expert and think "what about upvalues?", well, hats
    off to you, and we'll get to that later.

[^5]:
    If this makes you wonder how to access variables from outer scopes, you're
    on the right track; we'll handle this in the next section.

[yt-video]: https://www.youtube.com/watch?v=mNOLaw-_Buc
[crafting-interpreters]: https://craftinginterpreters.com/
[yt-long]: https://www.youtube.com/watch?v=KPbrI3xWdCg
[miette]: https://docs.rs/miette
[lua-manual]: https://www.lua.org/manual/5.4/manual.html
[github-lua]: https://github.com/lua/lua
[pratt-parsing]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
