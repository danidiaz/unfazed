# Unfazed

(Warning: highly experimental, don't know what I'm doing).

Like [foreign-store](https://hackage.haskell.org/package/foreign-store), this library can be used to preserve a value between ghci reloads. 

Unlike [foreign-store](https://hackage.haskell.org/package/foreign-store), this library doesn't use FFI. Instead, it works by storing values in auxiliary threads and communicating with the threads using asynchornous exceptions. 

Inspired by this [Haskell Discourse](https://discourse.haskell.org/t/live-reloading-gui-from-scratch/9569) thread about live reloading in ghci.

# Example of use

```
ghci> import Unfazed
ghci> newStore @Int "foo" 4
ghci> :reload
Ok, one module loaded.
ghci> queryStore @Int "foo"
4
ghci> queryStore @Bool "foo"
*** Exception: StoreNotFound
```