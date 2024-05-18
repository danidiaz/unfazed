# Unfazed

(**Warning**: this is just an experiment, I don't know what I'm doing.)

Like [foreign-store](https://hackage.haskell.org/package/foreign-store), this library can be used to preserve a value between ghci reloads. 

Unlike [foreign-store](https://hackage.haskell.org/package/foreign-store), this library doesn't use FFI. Instead, it works by storing values in auxiliary threads and communicating with the threads by means of asynchronous exceptions. 

Inspired by this [Haskell Discourse](https://discourse.haskell.org/t/live-reloading-gui-from-scratch/9569) thread about live reloading in ghci.

(**Warning#2**: this library might not very efficient when there are lots of
threads, because it performs a linear search across all threads when querying,
in order to locate "storage" threads.)

# Example of use

```
ghci> import Unfazed
ghci> newStore @Int "foo" 4
ghci> queryStore @Int "foo"
4
ghci> :reload
Ok, one module loaded.
ghci> queryStore @Int "foo"
4
ghci> queryStore @Bool "foo"
*** Exception: TypeMismatch {storeName = "foo", queryType = Bool, actualType = Int}
ghci> queryStore @Int "bar"
*** Exception: StoreNotFound {storeName = "bar"}
```