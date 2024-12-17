#lang racket

(require racket/control)

(+ 10 (reset (+ 20 (shift k (k 30)))))
