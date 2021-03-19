;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |assignment 0 Drew Solomon|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Drew Solomon
;Part 1: Expressions
;1
(* 3 -11.5)

;2
(define C1 (+ 3 (* 5 6)))
C1

;3
(/ (expt (expt 2 3) 4) 10)

;4
;2x^2 - 3x - 9 = 0
(/ (+ (* -1 -3) (sqrt(- (expt -3 2) (* 4 (* 2 -9))))) (* 2 2))

;Expressions on strings:
;1
(substring "affluent" 2)

;2
(string-append (substring "helicopter" 0 3) (substring "upload" 2 4))

;3
(string-append "Did you know" " " "(= (* 3 4) 12)" " " "?")

;Part 2: Functions

;1
;Number -> Number
;consumes a number and produces triple that number
(check-expect (triple -11.5) (* 3 -11.5))
(check-expect (triple 5) (* 3 5))
(check-expect (triple 0) (* 3 0))

;(define (triple NUMBER) 4)

(define (triple NUMBER)
  (* 3 NUMBER))

;2
;String -> String
;consumes any string and turns it into a simple question
(check-expect (make-question "the sky is blue")
              "Is it true that the sky is blue?")
(check-expect (make-question "(= (+ 5 3) 8)")
              "Is it true that (= (+ 5 3) 8)?")
;(define (make-question QUESTION) "")

(define (make-question QUESTION)
  (string-append "Is it true that" " " QUESTION "?"))


;3
;Numbers -> Number
;takes the coefficients of an equation of the form ax^2 + bx + c = 0, and returns the plus term of the quadratic formula
(check-expect (quadratic-positive 2 -3 -9) 3)
(check-expect (quadratic-positive 1 1 0) 0)


;(define (quadratic-positive a b c) 0)

(define (quadratic-positive a b c)
  (/ (+ (* -1 b) (sqrt(- (expt b 2) (* 4 (* a c))))) (* a 2)))