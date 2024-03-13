# Szakdolgozat

Témabejelentő:

A szakdolgozat címe:

Egyszerű Komputeralgebra-rendszer megvalósítása

A szakdolgozat témája:

A dolgozat célja egy egyszerű komputeralgebra-rendszer megvalósítása. A rendszer modellezi az egészek, racionálisak, valósak és a mod n gyűrűk körében történő
számolást; lehetővé tesz szimbolikus számolásokat, továbbá a polinomaritmetika is implementálásra kerül. Az alapvető aritmetikai műveleteken túl a polinomokkal
kapcsolatos algoritmusok közül a faktorizálás és a faktorgyűrűkben történő számolás is megjelenik. A rendszer egy egyszerű nyelv implementálásával lehetővé teszi hogy
a felhasználó is képes legyen saját függvények létrehozására ezzel bonyolultabb számítások megkönnyítését.

## Elmélet

Inspiration:

- SageMath
- [SymPy](https://en.wikipedia.org/wiki/SymPy) : **READ**
- [Automated simplification of large symbolic expressions](https://novaprd-lb.newcastle.edu.au/vital/access/%20/manager/Repository/uon:21382?view=list&f0=sm_identifier%3A%22http%3A%2F%2Fhdl.handle.net%2F1959.13%2F1307281%22&sort=sort_ss_title+asc) : Might be useless
- [Understanding Expression Simplification](http://www.cas.mcmaster.ca/~carette/publications/simplification.pdf) : **READ**

## Plan so far

### Scope

- Basic, symbolic and floating point arithmatic in ZZ, QQ, RR
- ZZ, QQ, RR (floating)
- Modular arithmetic over ZZ_n, ZZ[X]_n

- Arithmetic over quotient fields ZZ[x]_n/\<f\> where f is irreducible

- let binding, and functions (maybe lambda functions, so named functions are just let-bound lambdas)

- Polynomial arithmetic over ZZ[x], QQ[x], RR[x]

- Polynomial factoring, solving over ZZ[X], QQ[x], RR[x]

### Types

2 típust különböztetünk meg : Számok, polinómok

Számok :
  Minden complex számok, és a complex számokon definiált műveleteket használják

Polinómok :
  Complex számok listája és a complex polimokon definiált műveleteket használják

Moduló :

  open Zmod x : minden műveletet és literált megcimkéz egy `mod x` művelettel amit majd egy preprocesszor beszúr

  open Pmod f : minden műveletet `mod f` váges el ahol f egy ponimóm

  close : bezár minden megnyitott csomagot

- Numbers:
  - ZZ
  - ZZ mod n
  - QQ floating / QQ symbolic
  - RR floating / RR symbolic

- Polynomials:
  - Poly< R> where R is Number
  - QuotientPoly\<R\>\<f\> where R is Number, f : Poly\<R\>

### Típusok reprezentálása

Int :
  Integer (Végtelen nagy is lehet, már vannak rá megírt függvények)

Rac :
  (Int, Int)

  (Kérdéses a hogy legyen e invariáns [a/b : gcd(a,b) = 1] és ha igen akkor hol enforceoljuk, esetleg lehessen kapcsolni)
  (Geddes könyv, de mindenhol lehet)

Real :
  GHC Float/Double vagy egyből a Real típusosztály

  Algebrai számok, Z[x]/f gyűrűben ahol f a számot reprezentálja

  gyök(2) -(Newton módszer)> [(1.4,1.5),(1.41, 1.42),...]

Comp :
  (Real,Real) vagy (Data.Complex)

### Polinómok

Sima Listák (Vector)

esetleg lista ami tárolj a hosszát (rank) de akkor azt be kell tartani mert Haskellbe (langage mágia nélkül) nem lehet betartatni
amikor létre jön egy akkor autómatikusan szimplifikálódok

### Műveletek

Számok:
  +
  *
  div (egész osztás, vagyis levágja a tizedes jegyet)
  / (rendes complex osztás)
  mod (moduló)
  
Polinómok:

  factor - Polinóm faktorizálása (Kérdés: Nat, Int, ... alatt hogyan, Válasz : komplex alatt megy minden)

  irred - Irredúcibilis-e a polinóm (Ez kérdéses hogy mi alatt irredúcibilis-e)

  Osztály - elég fontos főleg a faktorgyűrűhez, talán fontos hogy gyors legyen

  deriválás

### Core Syntax

```bnf
<term> ::= 
  <name>
  | <term> <term>
  | "\" <name> ":" <ty> "." <term>
  | "let" <name> "=" <term> ";" <opt_tm>
  | <term> <binop> <term>
  | <unop> <term>
  | "true" | "false"
  | <number>
  | <poly>

<opt_tm> ::= ε | <term> 

<binop> ::= "+" | "*" | "&" | "|" | "^"

<unop> ::= "!" | "-"

<number> ::= --this should cover all numbers, even complex

<poly> ::= "[" <name> <binop> <name> "]"

<name> ::= [a-zA-z_][a-zA-z_0-9]* -- (isAlphaNum és nem kulcsszó)

<type> ::= "Int" | "Bool" | "Poly" <type> | <type> "->" <type>

```

-- A `let`-et még kitalálom de biztos lesz:
  
  `let x = y` --(desugar into)--> let x = y; () ahol () az üres term ami nem csinál semmit és tipusa top
  
  vagy magasabb szinten, a repl szinten elkapjuk a `let` el kezdődő kifejezéseket és hozzáadjuk a Contexthez, de lehet ez nehezen kivitelezheető mert a repl hozzá kell hogy férjen a Contexthez

-- Még kell polinómok szintaxisa

let x = [x^2 + x + 2] + [2 + x]; x
--[x^2 + 2x + 4]

factor x
--[(x+2)(x+2)]

## A non-exhausting long list of TODOs

Only exporting the needed functions from modules

Better folder structure (Core\Type.hs, Core\Run.hs, ...)

Code formating (pl.: [ormolu](https://github.com/tweag/ormolu))

Make Repl remember previous commands

Better error messages, both parsing and typing errors

Add timing, and other repl/exe features like loading files

Documentation

### References so far

[Core lang impl](https://github.com/AndrasKovacs/elaboration-zoo/tree/master/01-eval-HOAS-names)

Factor algorithm : [Factor-package](https://hackage.haskell.org/package/factor) and K.O.Geddes et. al.- Algorithms for Computer Algebra
