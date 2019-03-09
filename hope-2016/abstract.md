# Effective programming: bringing algebraic effects and handlers to OCaml

Algebraic effects were originally introduced to study the semantics of
computational effects. With the addition of handlers they have become an
exciting new programming construct for implementing such effects. Languages
such as Eff have demonstrated that handlers can be used as a more composable
alternative to monads for implementing effects in a pure language.

OCaml provides many standard effects, such as mutable state, built into
the language. Those effects not built into the language, for example
concurrency, are traditionally implemented using monads. The first part
of this talk will describe work to implement native algebraic effects
for OCaml. The original motivation for this work was to provide built-in
support for concurrency in OCaml without tying the language to a
particular concurrency implementation. However, algebraic effects
support many interesting examples beyond concurrency.

As with exceptions, algebraic effects risk being performed in a context
where they will not be handled. Type systems designed to track the
side-effects of expressions have been around for many years, and seem
eminently suitable for ensuring all algebraic effects are appropriately
handled.  Recent developments in languages such as Koka have begun to
produce effect systems that are genuinely usable, but they have yet to
breakthrough into a more mainstream language.  The second part of this
talk will describe work to integrate an effect system into OCaml whilst
maintaining backwards compatibility. This system both prevents effects
from going unhandled and turns OCaml into a pure functional language:
successfully tracking the purity of functions through their types.

The talk will discuss the interesting questions and challenges that
still remain before this work is ready for release into OCaml.

This is joint work with Stephen Dolan, Matija Pretnar and KC
Sivaramakrishnan.