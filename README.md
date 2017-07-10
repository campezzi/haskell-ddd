# Haskell DDD Playground

This repo contains random pieces of code loosely related to the book [Functional and Reactive Domain Modeling](https://www.manning.com/books/functional-and-reactive-domain-modeling), adapted for Haskell since I personally dislike Scala. It's not supposed to be used as a cohesive application - instead, it's more of a dumping ground for ideas as I read through the book.

### Key Insights

- Use type classes for polymorphism; at first I thought it would be a good idea to use a simple sum type with the commonalities between all data types extracted to a separate product type, but it turns out type classes do a better job at "making impossible states impossible".

- Defining entities in terms of a type class + a concrete type that implements it seems a bit heavy-handed at first, but pays of in the long run if polymorphism suddenly becomes necessary (or if the data type needs to be extended to "behave like" something else). This is the "I" on SOLID applied to FP. Not 100% convinced this is the best way to go as it may increase complexity early on, but it makes sense to at least consider.

- "Smart constructors": hiding the default data constructors for a type and providing custom functions to replace them is good practice as it makes it impossible to create entities/values that do not make sense from a business point of view (ie. a date range where the end date happens before the start date).

- Get well acquainted with the basics of category theory and learn to spot opportunities to use certain common abstractions. These are particularly useful:
  - `Semigroup`: useful when two values of a type have to be combined;
  - `Monoid`: useful when two values of a type have to be combined and an "identity" value is required (ie. as the initial accumulator value in a fold operation);
  - `Functor`: useful when a unary function has to be lifted into a computational context (ie. applying a function of type `Int -> a` to a value of type `Maybe Int` to get a `Maybe a`);
  - `Applicative`: useful when the result of an operation depends on executing several other individual, independent operations that can potentially be parallelized; from a source code perspective, applicatives are ideal candidates when a function of arbitrary arity has to be lifted into a computation context - in that sense, it's like a functor on steroids;
  - `Monad`: useful when the result of an operation depends on executing a chain of interdependent operations (in other words, a sequence of steps where each step needs the result of the previous one to be computed).

- When figuring out which algebra to use, pick the weakest (most generic) possible to maximize reuse opportunities. This is particularly important when picking between using `Applicative` or `Monad` as the parallelizable nature of `Applicative` may also lead to performance improvements.

- Functions that return monads can be composed just like regular functions with Kleisli composition; in Haskell, this is done with the `>=>` and `<=<` operators, ie. `(a -> m b) >=> (b -> m c) -> (a -> m c)`.
  - The Kleisli data type can be used to represent a computation that is waiting for a value to run and produce another value. Since Kleisli is a monad, operations of this type can be composed together in larger operations and then be all triggered at once by passing in the "missing" value;
  - The most common type of Kleisli seen in the wild is the `Reader` monad, which is usually used to inject environment configuration into a group of operations (ie. a database, global configuration parameters, a logger, etc);
  - The `State` monad is also a common specialization of Kleisli, and is used to inject some state (that can be modified) into a group of operations

- The first step when modeling a domain service is defining its algebra (ie. a group of operations), expressed in terms of functions and types that follow the ubiquitous language. The actual types and concrete instances of the service come later.

- If a domain service is used to represent a business process, its algebra should define functions that reflect the operations contained in that process. Pay special attention to function types so they "align", maximizing opportunities for composition.

- Once the invariants of a service have been defined, use the type system to try and enforce them statically, ie. with phantom types. The ultimate goal is to make any code that would cause a business invariant violation to not even compile.

- Separate code in modules and be mindful what they export. Ideally, a module will only export data types and functions that follow the ubiquitous language. Modules can also be used to enforce business invariants (ie. by not exporting default data constructors, providing smart constructors instead).

- The book recommends exporting the algebra of a service in a module and its specific implementations in a separate module to allow for easy swapping of implementations in different contexts (ie. testing). Again, not 100% convinced that type classes should be used this much as it adds heaps of boilerplate and noise to an otherwise simple scenario.

- Another way of separating algebra from implementation is using the `Free Monad` pattern. In that case, the module would expose composable, domain-specific functions that can be used as building blocks to a bigger computation, representing that computation as data (without actually performing it). Then, different interpreters can be written to actually execute the computation in whatever way they choose.
  - Free monads can be used to model operations that would normally be impure in a pure way by pushing the actual impurity down the line, to the interpreter level
  - Free monads are difficult to understand and grasp, so consider that before using them all over the place

- Large applications are composed of multiple bounded contexts that often have to work together to achieve a meaningful purpose. Reactive models accept and embrace that fact by making the interactions between those contexts explicit, aiming for loose coupling, fault-tolerance and resilience. There are several strategies to achieve that, for example:
  - `Future`/`Promise` based APIs
  - Asynchronous messaging through `Message Queues`
  - `Streams` with demand-driven interaction between producers and consumers
  - Lightweight `Actors` with mailboxes and message-passing
