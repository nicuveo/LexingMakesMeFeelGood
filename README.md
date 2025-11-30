# Lexing Makes Me Feel Good

Have you ever read a take so wrong on social media that you have to make a toy language and an interpreter for it just to disprove said bad take? I have. This is it.

There are two wrong claims that this example addresses:
  - lexers can't easily handle nested comments;
  - lexers can't handle expressions within string literals.

This toy language trivially supports both despite using a lexer. Here's an example program:

```
a = 1;
b = 2;
s = "foo { a + b /* /* nested comment */ */ } { "/*" /* nested string evaluation */ }";
print s;
```

This program, when run, prints `foo 3 /*`.

## Grammar

The grammar is as follows:

```
program: statement*
statement: assign | print

assign: IDENTIFIER "=" expression ";"
print: PRINT expression ";"

expression
  : "(" expression ")"
  | IDENTIFIER
  | INT
  | expression "+" expression
  | expression "-" expression
  | expression "*" expression
  | expression "/" expression
  | expression "%" expression
  | QUOTE string_element* QUOTE

string_element
  : "{" expression "}"
  | CHAR+
```

## Lexing

This interpreter uses [Alex](https://haskell-alex.readthedocs.io/en/latest/index.html) and [Happy](https://haskell-happy.readthedocs.io/en/latest/index.html). It makes uses of Alex's "start codes" feature to keep track of the current context: the inner state carries a stack of said start codes. It uses this to restrict some lexing rules to a given context, like so:

```
<0,interpolation> \"  { stringBegin }
<string>          \"  { stringEnd   }
<string>          "{" { interpolationBegin }
<interpolation>   "}" { interpolationEnd   }

<0, comment, interpolation> "/*" { commentBegin }
<0, comment, interpolation> "*/" { commentEnd   }
```

The character `"` is interpreted as the beginning of a string if we are at `0` (the root context) or within a string interpolation, and as the end of a string if we are within a string literal. Within a string, `{` starts an interpolation, and `}` closes it (but `}` isn't accepted at the root). Finally, `/*` pushes a comment context and `*/` pops it, but not within a string, which is why `"/*"` is interpreted correctly.

## Running the interpreter

To run a program, you can use `stack` like so:
```
$ stack build && stack run -- path_to_program_file
```
