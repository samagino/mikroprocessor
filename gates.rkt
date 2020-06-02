;; logic gates
;; γ means gate
#lang racket
(require "mk.rkt")
(require "header.rkt")

(provide
 andγ
 orγ
 xorγ
 invertγ)

(defrel (andγ top bot out)
  (conde
    ((== top hi) (== bot out))
    ((== top lo) (== out lo))))

(defrel (orγ top bot out)
  (conde
    ((== top hi) (== out hi))
    ((== top lo) (== out bot))))

(defrel (xorγ top bot out)
  (conde
    ((== top hi) (invertγ bot out))
    ((== top lo) (== bot out))))

(defrel (invertγ in out)
  (conde
    ((== in hi) (== out lo))
    ((== in lo) (== out hi))))

(defrel (2to1muxγ a b s y)
  (conde
    ((== y s) a)
    ((=/= y s) b)))

(defrel (4to1muxγ d₀ d₁ d₂ d₃ S y)
  (conde
    ((== S `(,lo ,lo)) d₀)
    ((== S `(,lo ,hi)) d₁)
    ((== S `(,hi ,lo)) d₂)
    ((== S `(,hi ,hi)) d₃)
    ))
