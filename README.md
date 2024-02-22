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

- Numbers:
  - ZZ
  - ZZ mod n
  - QQ floating / QQ symbolic
  - RR floating / RR symbolic

- Polynomials:
  - Poly< R> where R is Number
  - QuotientPoly\<R\>\<f\> where R is Number, f : Poly\<R\>

### Számok

Esetleges egységes típusosztály (Number where (+), (*), ...)

--Nat - Saját data vagy valami beépített ha van

Ez elhagyahtó mert nem alkotnak még csoportot sem

Int - Integer (Végtelen nagy is lehet, már vannak rá megírt függvények)

Rac - (Int, Int)

(Kérdéses a hogy legyen e invariáns [a/b : gcd(a,b) = 1] és ha igen akkor hol enforceoljuk, esetleg lehessen kapcsolni)
(Geddes könyv, de mindenhol lehet)

magában szimbólikus

Real - GHC Float/Double vagy egyből a Real típusosztály

Szimbólikushoz :

Algebrai számok, Z[x]/f gyűrűben ahol f a számot reprezentálja

gyök(2) -(Newton módszer)> [(1.4,1.5),(1.41, 1.42),...]

Comp - (Real,Real) vagy (Data.Complex)

Z mod p - Típusosztály ahol a p-t vagy könyvtár keresése

class Zmodp where
  p :: Z

majd minden művelet mod p ben végzünk el

Esetleges csomagok - TODO

### Polinómok

Sima Listák (Vector)

esetleg lista ami tárolj a hosszát (rank) de akkor azt be kell tartani mert Haskellbe (langage mágia nélkül) nem lehet betartatni
amikor létre jön egy akkor autómatikusan szimplifikálódok

---
Függvényekként, nagyon if

let f = (λx -> x\*x + x + 2)
és akkor
f 3 = 3 \* 3 + 3 + 2
és a f o g csak
let g = (λx -> x + x^3)
f o g = f . g (tárgynyelvi függvénykomp, metanyelvi függvénykomp. HOAS)

viszont összeadás, kivonás osztás kérdéses. AST szinten??

---

Faktorgyűrű - R\[x\]/f simán polinómgyűrű és mint a Zmodp nél mod f műveletek

itt is lehet valami invariáns, attól függően hogy vannak az osztás megírva

### Polinóm műveletek

Nyelvi beépített elemek lesznek, mint pl.: a függvényképzés
let f = λx -> x + 2 -- LetExpr f LamExpr x (PlusExpr x 2)
factor f            -- App factor f, itt factort kitaláljuk a tárgykódból és a factor függvény implementálva van, majd az App valamhol HOAS-al (factor f) meghívja

factor - Polinóm faktorizálása

Kérdés:
  Nat, Int, ... alatt hogyan

irred - Irredúcibilis-e a polinóm

Osztály - elég fontos főleg a faktorgyűrűhez, talán fontos hogy gyors legyen

Kérdés:
  Még mi?

deriválás

### Core Syntax

```bnf
<term> ::= 
  <name>
  | <term> <term>
  | ("λ" | "lam" | "\") <name> "." <term>
  | "let" <name> "=" <term> "in" <term>
  | <term> <binop> <term>
  | <unop> <term>

<binop> ::= "+" | "*" | "\" | "mod"

<unop> ::= "!" | "-"

<name> ::= [a-zA-z_][a-zA-z_0-9]*
```

-- A `let`-et még kitalálom de biztos lesz:
  vagy (`let x = y in`)
  vagy (ommit the `in`)
  vagy magasabb szinten, a repl szinten elkapjuk a `let` el kezdődő kifejezéseket és hozzáadjuk a Contexthez, de lehet ez nehezen kivitelezheető mert a repl hozzá kell hogy férjen a Contexthez

-- Még kell polinómok szintaxisa

### References so far

Core lang impl: https://github.com/AndrasKovacs/elaboration-zoo/tree/master/01-eval-HOAS-names

Factor algorithm : https://hackage.haskell.org/package/factor and K.O.Geddes et. al.- Algorithms for Computer Algebra
