# About This Directory

This directory contains the notes I took for *The Reasoned Schemer (1st Edition 2005)*

The book is out of date, and as of time of writing this (2024), the latest version is *2nd Edition (2018)*

You can find the implementations in the books here:
- [1st Edition](https://github.com/miniKanren/TheReasonedSchemer/)
- [2nd Edition](https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd)

## How do I run miniKanren myself?
- Clone/Download the edition of miniKanren
- 1st Edition
  - The implementation contains files like `mk.scm, mkextraforms.scm, ...`
  - At minimum, you will need to run `(load "path/to/mk.scm")` in Scheme REPL, or add `(load "path/to/mk.scm")` at the top of your Scheme file
  - For ease of use, I recommend also run `(load "path/to/mkextraforms.scm")` in REPL or add `(load "path/to/mkextraforms.scm")` in Scheme file. This is because `run*` is defined here
- 2nd Edition
  - Run/Add `(load "path/to/trs-impl.scm")` 
  
## What happens if my miniKanren and book don't match?
If you are reading the 1st Edition, but using implementation closer to the 2nd Edition, you will realize `conde` behaves differently than in the book, and print values in different order. This happens around Chapter 03. By Chapter 06, you will find you are missing forms like `condi, all, alli, ...`.

I haven't read the 2nd Edition as the time of writing this, but I suspect it is unlikely you will encounter similar issue. I started reading while using the [canonical version of miniKanren ported to Racket](https://github.com/miniKanren/Racket-miniKanren), and I realize the `conde` in there secretly does interleaving, which I believe is what the 2nd Edition does.

## My Opinions
I like the implementation of `conde` from the [canonical version](https://github.com/miniKanren/miniKanren) better. Interleaving guarantee discovery. If we think of each branch of `conde` as a thread, interleaving is similar to the `no staring` concept from operating systems.

However, I disagree with replacing `conde` from the 1st Edition. It does make the language much simplier, but it also limit my control over the program behaviour. `conde` also more closely resemble the behaviour of Prolog, which might help get people with Prolog experience onboard
