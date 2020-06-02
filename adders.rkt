#lang racket
(require "mk.rkt")
(require "header.rkt")
(require "gates.rkt")

(defrel (halfadder a b s c)
  (xorγ a b s)
  (andγ a b c))

(defrel (fulladder a b c₀ s₂ c₃)
  (fresh (c₁ c₂ s₁)
    (halfadder a b s₁ c₁)
    (halfadder s₁ c₀ s₂ c₂)
    (orγ c₁ c₂ c₃)))

;; start at 8 'cause it's like the standard lowest byte-width
(defrel (8addersubtractor A B d c₀ S oflow)
  (fresh (a₀ a₁ a₂ a₃ a₄ a₅ a₆ a₇
          b₀ b₁ b₂ b₃ b₄ b₅ b₆ b₇
          i₀ i₁ i₂ i₃ i₄ i₅ i₆ i₇  ;; intermediaries
          c₁ c₂ c₃ c₄ c₅ c₆ c₇
          s₀ s₁ s₂ s₃ s₄ s₅ s₆ s₇)
    (== `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀) A)
    (== `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀) B)
    (xorγ b₀ d i₀)
    (xorγ b₁ d i₁)
    (xorγ b₂ d i₂)
    (xorγ b₃ d i₃)
    (xorγ b₄ d i₄)
    (xorγ b₅ d i₅)
    (xorγ b₆ d i₆)
    (xorγ b₇ d i₇)
    (== `(,s₇ ,s₆ ,s₅ ,s₄ ,s₃ ,s₂ ,s₁ ,s₀) S)
    (fulladder a₀ i₀ c₀ s₀ c₁)
    (fulladder a₁ i₁ c₁ s₁ c₂)
    (fulladder a₂ i₂ c₂ s₂ c₃)
    (fulladder a₃ i₃ c₃ s₃ c₄)
    (fulladder a₄ i₄ c₄ s₄ c₅)
    (fulladder a₅ i₅ c₅ s₅ c₆)
    (fulladder a₆ i₆ c₆ s₆ c₇)
    (fulladder a₇ i₇ c₇ s₇ oflow)
    ))

;; TODO: figure out how to manage overflow, CPSR, etc.

;; this one uses appendo
;; it stalls when using run*, works with run 1 though
#;
(defrel (add8 A B O oflow)
  (fresh (A₀ A₁ B₀ B₁ O₀ O₁ c₀)
    (appendo A₁ A₀ A)
    (appendo B₁ B₀ B)
    (appendo O₁ O₀ O)
    (4addersubtractoro A₀ B₀ lo lo O₀ c₀)
    (4addersubtractoro A₁ B₁ lo c₀ O₁ oflow)))

;; this one doesn't use appendo, just a bunch of fresh variables
;; it may be faster, but that's a lot of variables...
(defrel (add8 A B O)
  (fresh (a₀ a₁ a₂ a₃ a₄ a₅ a₆ a₇
          b₀ b₁ b₂ b₃ b₄ b₅ b₆ b₇
          o₀ o₁ o₂ o₃ o₄ o₅ o₆ o₇
          oflow)
    (== `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀) A)
    (== `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀) B)
    (== `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀) O)
    (8addersubtractor `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀ )
                       `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀)
                       lo lo
                       `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀)
                       oflow)))

;; I could use add8 for this, but need carry bits, also there may be some other reasons
;; I shouldn't do that that I haven't thought of yet
(defrel (add16 A B O)
  (fresh (a₀ a₁ a₂ a₃ a₄ a₅ a₆ a₇ a₈ a₉ a₁₀ a₁₁ a₁₂ a₁₃ a₁₄ a₁₅
          b₀ b₁ b₂ b₃ b₄ b₅ b₆ b₇ b₈ b₉ b₁₀ b₁₁ b₁₂ b₁₃ b₁₄ b₁₅
          o₀ o₁ o₂ o₃ o₄ o₅ o₆ o₇ o₈ o₉ o₁₀ o₁₁ o₁₂ o₁₃ o₁₄ o₁₅
          c₁ oflow)
    (== `(,a₁₅ ,a₁₄ ,a₁₃ ,a₁₂ ,a₁₁ ,a₁₀ ,a₉ ,a₈ ,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀) A)
    (== `(,b₁₅ ,b₁₄ ,b₁₃ ,b₁₂ ,b₁₁ ,b₁₀ ,b₉ ,b₈ ,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀) B)
    (== `(,o₁₅ ,o₁₄ ,o₁₃ ,o₁₂ ,o₁₁ ,o₁₀ ,o₉ ,o₈ ,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀) O)
    (8addersubtractor `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀)
                      `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀)
                      lo lo
                      `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀)
                      c₁)
    (8addersubtractor `(,a₁₅ ,a₁₄ ,a₁₃ ,a₁₂ ,a₁₁ ,a₁₀ ,a₉ ,a₈)
                      `(,b₁₅ ,b₁₄ ,b₁₃ ,b₁₂ ,b₁₁ ,b₁₀ ,b₉ ,b₈)
                      lo c₁
                      `(,o₁₅ ,o₁₄ ,o₁₃ ,o₁₂ ,o₁₁ ,o₁₀ ,o₉ ,o₈)
                      oflow)
    ))

(defrel (sub8 A B O)
  (fresh (a₀ a₁ a₂ a₃ a₄ a₅ a₆ a₇
          b₀ b₁ b₂ b₃ b₄ b₅ b₆ b₇
          o₀ o₁ o₂ o₃ o₄ o₅ o₆ o₇
          oflow)
    (== `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀) A)
    (== `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀) B)
    (== `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀) O)
    (8addersubtractor `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀)
                      `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀)
                      hi hi
                      `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀)
                      oflow)))

(defrel (sub16 A B O)
  (fresh (a₀ a₁ a₂ a₃ a₄ a₅ a₆ a₇ a₈ a₉ a₁₀ a₁₁ a₁₂ a₁₃ a₁₄ a₁₅
          b₀ b₁ b₂ b₃ b₄ b₅ b₆ b₇ b₈ b₉ b₁₀ b₁₁ b₁₂ b₁₃ b₁₄ b₁₅
          o₀ o₁ o₂ o₃ o₄ o₅ o₆ o₇ o₈ o₉ o₁₀ o₁₁ o₁₂ o₁₃ o₁₄ o₁₅
          c₁ oflow)
    (== `(,a₁₅ ,a₁₄ ,a₁₃ ,a₁₂ ,a₁₁ ,a₁₀ ,a₉ ,a₈ ,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀) A)
    (== `(,b₁₅ ,b₁₄ ,b₁₃ ,b₁₂ ,b₁₁ ,b₁₀ ,b₉ ,b₈ ,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀) B)
    (== `(,o₁₅ ,o₁₄ ,o₁₃ ,o₁₂ ,o₁₁ ,o₁₀ ,o₉ ,o₈ ,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀) O)
    (8addersubtractor `(,a₇ ,a₆ ,a₅ ,a₄ ,a₃ ,a₂ ,a₁ ,a₀)
                      `(,b₇ ,b₆ ,b₅ ,b₄ ,b₃ ,b₂ ,b₁ ,b₀)
                      hi hi
                      `(,o₇ ,o₆ ,o₅ ,o₄ ,o₃ ,o₂ ,o₁ ,o₀)
                      c₁)
    (8addersubtractor `(,a₁₅ ,a₁₄ ,a₁₃ ,a₁₂ ,a₁₁ ,a₁₀ ,a₉ ,a₈)
                      `(,b₁₅ ,b₁₄ ,b₁₃ ,b₁₂ ,b₁₁ ,b₁₀ ,b₉ ,b₈)
                      hi c₁
                      `(,o₁₅ ,o₁₄ ,o₁₃ ,o₁₂ ,o₁₁ ,o₁₀ ,o₉ ,o₈)
                      oflow)
    ))
