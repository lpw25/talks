# Designing an algebraic effect system for OCaml

Algebraic effects were originally introduced to study the semantics of
computational effects. With the addition of handlers they have become an
exciting new programming construct for implementing such effects.

In this talk, I'll describe ongoing work to design and implement support
for algebraic effects in the OCaml programming language, including
tracking these effects in the type system. I'll focus on two interesting
aspects of the design for such a system:

 - How should effectful operations be matched to effect handlers?
 - How do we implement type inference for an effect system in a language
   like OCaml?
