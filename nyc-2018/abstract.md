# Effective Programming: Adding an Effect System to OCaml

Type systems designed to track the side-effects of expressions have been
around for many years but they have yet to breakthrough into more
mainstream programming languages. This talk will focus on on-going work
to add an effect system to OCaml.

This effect system is primarily motivated by the desire to keep track of
algebraic effects in the OCaml type system. Algebraic effects are an
exciting new programming construct for implementing control effects such
as concurrency, co-routines and generators. Through the Multicore OCaml
project, support for algebraic effects is likely to be included in OCaml
in the near future.

However, the effect system also allows for tracking side-effects more
generally. It distinguishes impure functions, which perform
side-effects, from pure functions, which do not. It also includes a
tracked form of exception to support safe and efficient error handling.

This talk will give an overview of the effect system and demonstrate a
prototype implementation on some practical examples. Time permitting, I
also intend to talk briefly about some of my other work on OCaml,
including modular implicits and macros.
