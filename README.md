# Polylang : A basic computer algebra system

Polylang is a basic computer algebra system focusted in complex polinomials, and complex numbers. It also doubles as a function proframming language, with a simple monomorph typesystem.

## Getting started

To build polylang yourself, you will need a Haskell complier, and the cabal build system. You can find both at [GHCup](https://www.haskell.org/ghcup/). If you do have them, then follow these steps:

```cmd
git clone https://github.com/IModer/thesis.git poly_lang
cd poly_lang
cabal build
cabal run
```

If you dont want to build it youorself you can check out the [releases](https://github.com/IModer/thesis/releases/) page if there is a build for your machine.

## Usage

```txt
Usage : poly_lang.exe or poly_lang.exe <command>
    Commands:   load <file1> [<file2> ...] - loads the specified files
                help - prints this help
                docs <topic> - Prints help on the specified topic, use `docs topic` to print out the available topics
```

## Overview

Polylang is a REPL where you can execute code written in the poly_lang language, which is a functional language based on the simply typed lambda calculus. This means that each line you write in the REPL gets interpreted, then the resulting answer (normalized program) is show.

- It has the regual lambda syntax : `(\x : Type . Body)`.
- Polynomial variables have to be declared with the `var` keyword, followed by a capital english alphabet letter.
- Useful functions can be found in Prelude.poly, which is automaticly loaded on start.

## Examples

```poly_lang
var X

var Y

(X * Y + 4) * (Y * Y + Y) -- == 1 * X * Y^3 + 1 * X * Y^2 + 4 * Y^2 + 4 * Y

factor ((powP X 4) + (powP X 3) + 4 * (powP X 2) + 4 * X) -- == [1 * X , 1 * X + 1 , 1 * X^2 + 4]
```

More example code can be found under [/examples](/examples)

## Thesis

Polylang is also my Bsc thesis at Eötvös Loránd University. The complete thesis, in hungarian, can be found here : [link](todo).
