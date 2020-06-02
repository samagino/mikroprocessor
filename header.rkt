;; header for mk relations I want
#lang racket

(provide
 hi
 lo)

;; Everything is a either
;;   - a signal (a wire) denoted by lowercase letters
;;   - a list of signals (a bus) denoted by uppercase letters
;;
;; A signal is one of
;;   - hi
;;   - lo
;;
;; hi is some symbol distinct from lo
;; lo is some symbol distinct from hi

(define hi '☭) ;; symbol for high voltage
(define lo '☪) ;; symbol for low voltage
