# HLang Compiler

Compiler written in Haskell for the HLang imperative language. HLang is not a real programming language, I am mainly using it to learn how Monadic parser combinators work. See more on [tsoding/haskell-json](https://github.com/tsoding/haskell-json/).

## Language Syntax Grammar

#### Non Left Recursive

<pre>
program     ::= [declaration<b>;</b>]* <b>>>></b> instruction
declaration ::= <b>let</b> name [name]* <b>=></b> instruction <b>end</b>
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
statement   ::= <b>if</b> instruction <b>then</b> instruction <b>else</b> instruction <b>fi</b>
              | <b>$</b> name [statement]*
              | factor
factor      ::= name
              | literal
              | <b>(</b> instruction <b>)</b>
literal     ::= int
              | bool
name        ::= string
</pre>

## Code Example

```
# file.hlang
let square x => if x == 0 then 0 else x * x fi end;
let func x => $ square x + $ square x end;
>>> $ func 5
# output
$ 50
```

## Getting Started

#### Usage

* Download the latest release for your platform or build it from source.

```shell
$ hlang /path/to/file.hlang
```

#### Build

##### Tested using:
* Windows 10
* GHC version 8.6.5
* cabal-install version 3.4.0.0

##### Requirements:
In order to build this project you will need GHC and cabal installed:
* [Haskell](https://www.haskell.org/downloads/)

```shell
$ git clone https://github.com/alexjercan/hlang.git
$ cd hlang
$ cabal init
$ cabal build
```
