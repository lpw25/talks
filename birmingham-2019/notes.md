
# Nominative vs Structural vs Labelled

- Named things in OCaml are abstractable. Abstraction good, but it means that
  we've abstracted away the information about which handler will handle an
  effect -- prevents parameterisation.

- We can instead use a structural notion of equality, and then forbid
  abstraction that hides information about what handler will handle an effect.
  But then we cannot abstract the details of which operations an effect has.

- Neither of those situations is ideal. We want to be able to abstract the
  details of an effect's operations without hiding the information about
  which handler will handle which effect. ....

# Leaking

- Previous issue was about the relationship between the runtime semantics and
  the type system. This issue is really all about runtime semantics, although
  people often talk about it in terms of typing.

- Leaking shows that handlers on their own are not sufficient. We need a way
  to manipulate the relationship between effects and handlers.

- Using shift to explicitly manipulate the relationship between effects
  and handlers is not very satisfactory -- its a pretty blunt instrument.
  Feels a lot like using De Bruijn indices.

- Effect coercions takes the idea even further. It actually starts by
  essentially saying that all effects are equal: they are always handled
  by the nearest enclosing handler. Then they provide a series of
  "effect coercions" to explicitly manipulate which handler on the stack
  will be used.

- This allows them to support abstracted effect labels.

- Then at the end they get back the convenience by having some inference
  that tries to infer implicit coercions when they are safe.

- These manipulations have the feel of a stack machine.

- And the coercions they provide are essentially: shift and swap -- which
  to me look an awful lot like weakening and exchange.

- It seems like what we are really manipulating here is a context of
  free variables. And if we look at effects the right way we can see
  that this is indeed the case.

# Computation trees

- We can view effectful computations as trees.

- This is closely related to the original presentation of algebraic effects,
  and these trees -- essentially algebraic terms -- are where the "algebraic"
  in algebraic effects comes from.

- Handlers are then just a form of substitution

- From this perspective, effectful computations are simply open terms. And
  manipulating the relationship between handlers and effects is really just
  manipulation of open terms

- Manipulating open terms is a well studied problem, so can we use any of the
  well-established approaches for manipulating open terms for managing effects
  and handlers.

- I think we can, and what I've been looking at recently, in collaboration with
  Stephen Dolan, is taking ideas from nominal sets and applying the to these
  issues.
