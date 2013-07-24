rark
====

A **R**acket language inspired by **Ar**c, by **k**ogir.

This is my plaything, nothing more. In its current form it shouldn't even be considered an Arc fork.

Clone and in the package directory:

```
> raco planet link kogir rark.plt 1 0 `pwd`
> raco planet show
Development links:
  kogir	rark.plt	1 0
    --> /path/to/git/clone
 ```

 Then just

 ```racket
 #lang planet kogir/rark

 (= test (table))
 (= (test 'a) "A")
 ```
 