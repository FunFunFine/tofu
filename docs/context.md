---
id: context
title: Context & Examples
---

## Contextual abstractions
There is well-known `Reader` monad, which allows us to depend on some context.
Also, there is its monad-transformer version `ReaderT[F[_], R, A]`, which adds this “dependent” property for some `F`.
How does one abstract over it when using tagless final? Tofu gives a set of specialized abstractions:

* `WithContext[F[_], C]` — for accessing context within `F`
* `WithLocal[F[_], C]` — for local altering of context within `F`
* `WithProvide[F[_],G[_] C]` — for providing context to evaluate `F` to `G`
* `WithRun[F[_],G[_] C]` — for `G` to become contextual just like `F`

There are versions of these abstractions with hidden type parameter for `C` as a part of legacy encoding — `Context[F[_]]`,`Local[F[_]]`,`Provide[F[_]]`,`RunContext[F[_]]`.

## Example
Let's examine simple example web-server in [examples folder](../examples) and see how one can use every one of these abstractions.

### Example server breakdown

This server is a simple backend for Recipe website. The website is not ready yet, but we already know that we want to put recipes in and get them out.

It is implemented with http4s web-server and uses cats.Ref as in-memory storage for recipes.




 
