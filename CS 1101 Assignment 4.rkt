;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |assignment 4 Drew Solomon Lehong Wang|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Drew Solomon and Lehong Wang
;NOTES:
;-Write test cases before/right after writing a new function
;-Write more/better cases than starter file

(define-struct ancestor (name yob yod mother father pets))
;; name of the ancestor, his year of birth, year of death, 
;; mother, father, and pets that he owned
;; we use false to indicate that an ancestor is unknown
(define (fn-for-ancestor ancestor)
  (cond
    [(false? ancestor) (...)]
    [else
     (...
      (ancestor-name ancestor) ; String
      (ancestor-yob ancestor) ; Natural
      (ancestor-yod ancestor) ; Natural
      (fn-for-ancestor (ancestor-mother ancestor)) ; Ancestor
      (fn-for-ancestor (ancestor-father ancestor)) ;Ancestor
      (fn-for-pets (ancestor-pets ancestor)))])) ; ListOfPets
;==============================================================

(define-struct pet (name type cute?))
;; name of the pet, type of animal, and whether it is cute
(define (fn-for-pet pet)
  (...
   (pet-name pet) ;String
   (pet-type pet) ;String
   (pet-cute? pet) ;Boolean
   ))
;==============================================================

(define mom-cat (make-pet "mom-cat" "cat" true))
(define mom-dad-cat1 (make-pet "mom-dad-cat1" "cat" true))
(define mom-dad-cat2 (make-pet "mom-dad-cat2" "cat" true))
(define mom-dad-cat3 (make-pet "mom-dad-cat3" "cat" false))
(define dtr-dog1 (make-pet "dtr-dog1" "dog" true))
(define dtr-dog2 (make-pet "dtr-dog2" "dog" false))
(define son-son-cat1 (make-pet "son-son-cat1" "cat" false))
(define son-son-cat2 (make-pet "son-son-cat2" "cat" true))
(define son-son-dog (make-pet "son-son-dog" "dog" true))

;==============================================================


(define ANCESTOR6 (make-ancestor "Chris's dad's dad" 1840 1940 false false
                                 empty))
(define ANCESTOR5 (make-ancestor "Chris's mom's dad" 1845 1910 false false
                                 (list mom-dad-cat1 mom-dad-cat2 mom-dad-cat3)))
(define ANCESTOR4 (make-ancestor "Chris's mom's mom" 1850 1900 false false
                                 empty))
(define ANCESTOR3 (make-ancestor "Chris's dad" 1860 1930 false ANCESTOR6
                                 empty))
(define ANCESTOR2 (make-ancestor "Chris's mom" 1880 1980 ANCESTOR4 ANCESTOR5
                                 (list mom-cat)))
(define ANCESTOR1 (make-ancestor "Chris" 1900 1970 ANCESTOR2 ANCESTOR3
                                 empty))
(define ANCESTOR7 (make-ancestor "Chris's son" 1925 2000 false ANCESTOR1
                                 empty))
(define ANCESTOR8 (make-ancestor "Chris's daughter" 1930 2020 false ANCESTOR1
                                 (list dtr-dog1 dtr-dog2)))
(define ANCESTOR9 (make-ancestor "Chris's son's son" 1950 2000 false ANCESTOR7
                                 (list son-son-cat1 son-son-cat2 son-son-dog)))

;==============================================================

;Ancestor -> Boolean
;returns true if the second person is an ancestor of the first, false if not
(check-expect (is-ancestor? ANCESTOR1 false) false)
(check-expect (is-ancestor? false ANCESTOR1) false)
(check-expect (is-ancestor? ANCESTOR1 ANCESTOR2) true)
(check-expect (is-ancestor? ANCESTOR1 ANCESTOR1) true)
(check-expect (is-ancestor? ANCESTOR2 ANCESTOR1) false)
(check-expect (is-ancestor? ANCESTOR1 ANCESTOR4) true)
(check-expect (is-ancestor? ANCESTOR1 ANCESTOR5) true)
(check-expect (is-ancestor? ANCESTOR1 ANCESTOR9) false)
(check-expect (is-ancestor? ANCESTOR9 ANCESTOR1) true)
(check-expect (is-ancestor? ANCESTOR7 ANCESTOR8) false)
(check-expect (is-ancestor? ANCESTOR8 ANCESTOR7) false)

;(define (is-ancestor? key find) false)

(define (is-ancestor? key find)
  (cond
    [(false? find) false]
    [(false? key) false]
    [(string=? (ancestor-name key) (ancestor-name find)) true]
        
    [else
     (if (or (is-ancestor? (ancestor-mother key) find) (is-ancestor? (ancestor-father key) find))
         true
         false)]))

;=======================================================

;Ancestor -> ListOfAncestorNames
;Takes an ancestor and returns a list of all the names of its ancestors who lived to be over 60 years old
(check-expect (ancestors-over-60 false) empty)
(check-expect (ancestors-over-60 ANCESTOR1)
              (list
 "Chris"
 "Chris's mom"
 "Chris's mom's dad"
 "Chris's dad"
 "Chris's dad's dad"))
(check-expect (ancestors-over-60 ANCESTOR2)
              (list "Chris's mom" "Chris's mom's dad"))
(check-expect (ancestors-over-60 ANCESTOR7)
              (list
 "Chris's son"
 "Chris"
 "Chris's mom"
 "Chris's mom's dad"
 "Chris's dad"
 "Chris's dad's dad"))

;(define (ancestors-over-60 key) empty)

(define (ancestors-over-60 key)
  (cond
    [(false? key) empty] ;Base case - false -> return empty
    [(over-60? key)
         (cons (ancestor-name key) (append (ancestors-over-60 (ancestor-mother key)) (ancestors-over-60 (ancestor-father key))))]
    [else
     (append (ancestors-over-60 (ancestor-mother key)) (ancestors-over-60 (ancestor-father key)))]
    ))


;Ancestor -> Boolean
;Takes an ancestor and returns true if they lived to be over 60 and false if not
(check-expect (over-60? ANCESTOR1) true)
(check-expect (over-60? ANCESTOR2) true)
(check-expect (over-60? ANCESTOR3) true)
(check-expect (over-60? ANCESTOR4) false)

;(define (over-60? key) false)

(define (over-60? key) ;Helper function to determine if a person lived to be over 60
  (if (>= (- (ancestor-yod key) (ancestor-yob key)) 60)
          true
          false)) 

;=======================================================

;Ancestor -> ListOfAncestorNames
;Takes an ancestor and returns a list of all of their ancestors who had at least 1 cute pet
(check-expect (a-cute-pet false) empty)
(check-expect (a-cute-pet ANCESTOR1) (list "Chris's mom" "Chris's mom's dad"))
(check-expect (a-cute-pet ANCESTOR8) (list "Chris's daughter" "Chris's mom" "Chris's mom's dad"))

;(define (a-cute-pet key) empty)

(define (a-cute-pet key)
  (cond
    [(false? key) empty] ;Base case - false -> return empty
    [(>= (cute-pet-count (ancestor-pets key) 0) 1)
         (cons (ancestor-name key) (append (a-cute-pet (ancestor-mother key)) (a-cute-pet (ancestor-father key))))]
    [else
     (append (a-cute-pet (ancestor-mother key)) (a-cute-pet (ancestor-father key)))]
    ))


;ListOfPets Natural -> Natural
;Takes a list of pets and returns the amount of cute pets in the list
(check-expect (cute-pet-count empty 1) 1)
(check-expect (cute-pet-count (ancestor-pets ANCESTOR1) 0) 0)
(check-expect (cute-pet-count (ancestor-pets ANCESTOR2) 0) 1)
(check-expect (cute-pet-count (ancestor-pets ANCESTOR5) 0) 2)

;(define (cute-pet-count lop count) 0)

(define (cute-pet-count lop count)
(cond
    [(empty? lop) count] ;Base case
    [(pet-cute? (first lop))
         (cute-pet-count (rest lop) (+ count 1))]
    [else
     (cute-pet-count (rest lop) count)]
    ))

;=======================================================

;Ancestor -> ListOfAncestorNames
;Takes an ancestor and returns a list of all of their ancestors who had at least 2 cute pets
(check-expect (two-cute-pets false) empty)
(check-expect (two-cute-pets ANCESTOR1) (list "Chris's mom's dad"))
(check-expect (two-cute-pets ANCESTOR8) (list "Chris's mom's dad"))
(check-expect (two-cute-pets ANCESTOR9) (list "Chris's son's son" "Chris's mom's dad"))

;(define (two-cute-pets key) empty)

(define (two-cute-pets key)
  (cond
    [(false? key) empty] ;Base case - false -> return empty
    [(>= (cute-pet-count (ancestor-pets key) 0) 2)
         (cons (ancestor-name key) (append (two-cute-pets (ancestor-mother key)) (two-cute-pets (ancestor-father key))))]
    [else
     (append (two-cute-pets (ancestor-mother key)) (two-cute-pets (ancestor-father key)))]
    ))

;========================================================

; Ancestor Natural -> ListOfAncestorNames
; Takes an ancestor and returns a list of their ancestors’ names who had more than cutoff pets of the same type
(check-expect (multiple-same-type-pets false 0) empty)
(check-expect (multiple-same-type-pets ANCESTOR1 1) (list "Chris's mom" "Chris's mom's dad"))
(check-expect (multiple-same-type-pets ANCESTOR9 3) (list "Chris's mom's dad"))
(check-expect (multiple-same-type-pets ANCESTOR9 4) empty)

;(define (multiple-same-type-pets key cutoff) empty)

(define (multiple-same-type-pets key cutoff)
  (cond
    [(false? key) empty] ;Base case - false -> return empty
    [(>= (same-pet-count (ancestor-pets key) 0) cutoff)
         (cons (ancestor-name key) (append (multiple-same-type-pets (ancestor-mother key) cutoff) (multiple-same-type-pets (ancestor-father key) cutoff)))]
    [else
     (append (multiple-same-type-pets (ancestor-mother key) cutoff) (multiple-same-type-pets (ancestor-father key) cutoff))]
    ))


;ListOfPets Natural -> Natural
;Takes a list of pets and returns the amount of pets that match the type of the first pet in the list
(check-expect (same-pet-count empty 1) 1)
(check-expect (same-pet-count (list mom-dad-cat1 mom-dad-cat2 mom-dad-cat3) 1) 3)
(check-expect (same-pet-count (list mom-dad-cat1 mom-dad-cat2 mom-dad-cat3) 0) 3)
(check-expect (same-pet-count (list mom-dad-cat2 mom-dad-cat3) 0) 2)
(check-expect (same-pet-count (list mom-dad-cat3) 2) 2)
(check-expect (same-pet-count (list mom-dad-cat1 mom-dad-cat2) 0) 2)
(check-expect (same-pet-count (list son-son-dog son-son-cat1 son-son-cat2) 0) 2)
(check-expect (same-pet-count (list son-son-cat2 son-son-dog son-son-cat1) 0) 2)

;(define (same-pet-count lop count) 0)

(define (same-pet-count lop count)
(cond
    [(empty? lop) count] ;Base case
    [(> (count-pet-type lop 0 (pet-type (first lop))) count)
         (same-pet-count (rest lop) (count-pet-type lop 0 (pet-type (first lop))))]
    [else
     (same-pet-count (rest lop) count)]
    ))


;ListOfPets Natural String -> Natural
;Takes in a list of pets, a starting number for the count, and a type of pet, and searches
;the list of pets for that type of pet and returns how many were found
(check-expect (count-pet-type empty 1 "cat") 1)
(check-expect (count-pet-type (list mom-dad-cat1 mom-dad-cat2 mom-dad-cat3) 0 "dog") 0)
(check-expect (count-pet-type (list mom-dad-cat1 mom-dad-cat2 mom-dad-cat3) 0 "cat") 3)
(check-expect (count-pet-type (list son-son-cat1 son-son-cat2 son-son-dog) 0 "cat") 2)
(check-expect (count-pet-type (list son-son-cat1 son-son-cat2 son-son-dog) 0 "dog") 1)
                              
;(define (count-pet-type lop count petType) 0)

(define (count-pet-type lop count petType)
(cond
    [(empty? lop) count] ;Base case
    [(string=? (pet-type (first lop)) petType)
         (count-pet-type (rest lop) (+ count 1) petType)]
    [else
     (count-pet-type (rest lop) count petType)]
    ))

;==============================================================
;NOTES:
;1.  write data definition and templates.  really
;2.  expand example tree
;3.  have RL and LR cases
;4.  add base case to over-60 function, then rewrite code

;(____ (list 1 2) (list 3)) --> (list 1 2 3)
;Append
;make a base case - add empty when false node
;BASE CASES ARE IMPORTANT - MAKE FIRST










