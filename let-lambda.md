# Hindley-Milner típusrendszeres lambda calculus let-binding-al

## Lambda Calculus Syntax with Let binding

```bnf

<típus>                  ::= <egyszerű típus>
                         |   (forall <típusváltozó> . <típus>)

<egyszerű típus>         ::= <típusváltozó>
                         |   (<egyszerű típus> -> <egyszerű típus>)

<lambda-kifejezés>       ::= <változó>
                         |   (<lambda> <változó> . <lambda-kifejezés>)
                         |   (<lambda-kifejezés> <lambda-kifejezés>)
                         |   <egyszerű let-kifejezés>

<egyszerű let-kifejezés> ::= let <változó> = <lambda-kifejezés> in <lambda-kifejezés>
```

## Kikövetkeztetési szabályok

## Todo

- hozzáadni a típusokhoz a comp-algebra specifikus szabályokat
- hozzáadni a kiköv szabályokhoz a comp-algebra specifikus típusok szabályait
