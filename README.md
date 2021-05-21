# HLang Compiler

Compiler written in Haskell for the HLang imperative language. HLang is not a real programming language, I am mainly using it to learn how Monadic parser combinators work. See more on [tsoding/haskell-json](https://github.com/tsoding/haskell-json/).

## Language Syntax Grammar

#### Non Left Recursive

<pre>
program     ::= [declaration<b>;</b>]<b>*</b> <b>>>></b> instruction
declaration ::= <b>let</b> function variable <b>=></b> instruction <b>end</b>
instruction ::= expression <b>==</b> expression
              | expression <b><</b> expression
              | expression <b><=</b> expression
              | expression
expression  ::= term <b>+</b> expression
              | term <b>-</b> expression
              | term
term        ::= statement <b>*</b> term
              | statement <b>/</b> term
              | statement
statement   ::= <b>if</b> instruction <b>then</b> instruction <b>else</b> instruction
              | <b>$</b> factor statement
              | factor
factor      ::= variable
              | function
              | literal
              | <b>(</b> instruction <b>)</b>
literal     ::= int
</pre>

## Code Example

```
let square x => if x == 0 then 0 else x * x fi end;
let func x => $ square x + $ square x end;
>>> $ func 5
```