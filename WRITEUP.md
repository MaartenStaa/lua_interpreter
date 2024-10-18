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

### Error handling

### Function calls

### Closures and upvalues

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

[yt-video]: https://www.youtube.com/watch?v=mNOLaw-_Buc
[crafting-interpreters]: https://craftinginterpreters.com/
[yt-long]: https://www.youtube.com/watch?v=KPbrI3xWdCg
[miette]: https://docs.rs/miette
[lua-manual]: https://www.lua.org/manual/5.4/manual.html
[github-lua]: https://github.com/lua/lua
[pratt-parsing]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
