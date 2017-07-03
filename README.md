# Haskell DDD Playground

This repo contains random pieces of code loosely related to the book [Functional and Reactive Domain Modeling](https://www.manning.com/books/functional-and-reactive-domain-modeling), adapted for Haskell since I personally dislike Scala. It's not supposed to be used as a cohesive application - instead, it's more of a dumping ground for ideas as I read through the book.

### Key Insights

- Use type classes for polymorphism; at first I thought it would be a good idea to use a simple sum type with the commonalities between all data types extracted to a separate product type, but it turns out type classes do a better job at "making impossible states impossible".

- Defining entities in terms of a type class + a concrete type that implements it seems a bit heavy-handed at first, but pays of in the long run if polymorphism suddenly becomes necessary (or if the data type needs to be extended to "behave like" something else). This is the "I" on SOLID applied to FP. Not 100% convinced this is the best way to go as it may increase complexity early on, but it makes sense to at least consider.

- Get well acquainted with the basics of category theory and learn to spot opportunities to use certain common abstractions. These are particularly useful:
  - `Semigroup`: useful when two values of a type have to be combined;
  - `Monoid`: useful when two values of a type have to be combined and an "identity" value is required (ie. as the initial accumulator value in a fold operation);
  - `Functor`: useful when a unary function has to be lifted into a computational context (ie. applying a function of type `Int -> a` to a value of type `Maybe Int` to get a `Maybe a`);
  - `Applicative`: useful when the result of an operation depends on executing several other individual, independent operations that can potentially be parallelized; from a source code perspective, applicatives are ideal candidates when a function of arbitrary arity has to be lifted into a computation context - in that sense, it's like a functor on steroids;
  - `Monad`: useful when the result of an operation depends on executing a chain of interdependent operations (in other words, a sequence of steps where each step needs the result of the previous one to be computed).

- When figuring out which algebra to use, pick the weakest (most generic) possible to maximize reuse opportunities. This is particularly important when picking between using `Applicative` or `Monad` as the parallelizable nature of `Applicative` may also lead to performance improvements.

- Functions that return monads can be composed just like regular functions with Kleisli composition; in Haskell, this is done with the `>=>` and `<=<` operators, ie. `(a -> m b) >=> (b -> m c) -> (a -> m c)`.

- The first step when modeling a domain service is defining its algebra (ie. a group of operations), expressed in terms of functions and types that follow the ubiquitous language. The actual types and concrete instances of the service come later.

- If a domain service is used to represent a business process, its algebra should define functions that reflect the operations contained in that process. Pay special attention to function types so they "align", maximizing opportunities for composition.

- Once the invariants of a service have been defined, use the type system to try and enforce them statically, ie. with phantom types. The ultimate goal is to make any code that would cause a business invariant violation to not even compile.