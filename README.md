rark
====

A **R**acket language inspired by **ar**c, by **k**ogir.

This is my plaything. It's not the future of arc, and in its current form shouldn't even be considered an arc fork. Think of it as an alternate, incomplete and incompatile arc-inspired language.

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
 