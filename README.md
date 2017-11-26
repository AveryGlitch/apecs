On this branch, `System w a = SystemT w IO a`.
System is now a monad transformer, so it can lift effects into monads other than IO.
This is useful if e.g. your graphics package runs in a monad, and you'd like to embed `System` into it.

Stores have instances per monad, which allows you to e.g. define instances for `STM`.
