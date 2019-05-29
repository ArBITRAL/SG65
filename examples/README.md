### A system is specified by using abc (attribute-based communication) calculus

We start from a given abc specification, from which

1. the translator abc2umc generates the umc model for formal verification by relying on UMC model checker

2. the translator abc2abel generates erlang code for actual execution by relying on ABEL runtime environment


### How to write abc specifications in ascii.

The translation tools accept input files written in the following format:

```scala
component Type
   attributes:  a1, a2, ...
   observables: % list of attribute names that will be exposed as state labels
   behaviour:
       let {
          % start a comment
          %% process defintions in abc style
          %% P := nil | <pi>P | action.U | P + P | P|P | K
          %% action := (e1,e2,...)@(pi) | (pi)(x1,x2,...)
          %% e := number | 'literal' | tt | ff | x | a | this.a
          %% pi := true | false | pi and pi | not pi | pi or pi
       }
       init
          %% process expression
end

%% data initializations
C1 : Type (a1 -> v1, a2 -> v2, ...)
...
Cn : Type (...)
```

A specification can have several types, declared in a similar fashion.
