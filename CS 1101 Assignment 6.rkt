;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |assignment 6 Drew Solomon Haohao Yi|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ----------------------------------------------------------------
;; Part 1
;; ----------------------------------------------------------------

;Operator Number ListOfNumber -> ListOfNumber
;Consumes an operator, a number and a list of numbers, generating a number for each number by entering the operator and the number
(check-expect (do-math + 3 (list -5.5 16 0 2)) (list -2.5 19 3 5))
(check-expect (do-math - 3 (list -5.5 16 0 2)) (list -8.5 13 -3 -1))
(check-expect (do-math * 2 (list 1    6.4 -2)) (list 2 12.8 -4))
(check-expect (do-math / 2 (list 1    6.4 -2)) (list 0.5 3.2 -1))
(check-expect (do-math expt 2 (list 0 1 2 3 4)) (list 0 1 4 9 16))
(check-expect (do-math expt .5 (list 4 9 16 49)) (list 2 3 4 7))

(check-expect (do-math expt 2 empty) empty)

;(define (do-math operator number empty) empty)


(define (do-math operator number listOfNums)
  (map (lambda (currNum) (operator currNum number)) listOfNums))

;; ----------------------------------------------------------------
;; Part 2
;; ----------------------------------------------------------------

; ListOfNumbers -> ListOfNumbers
;Adds numbers in list until a zero in the list is found and adds sum to list,
;and returns list of sums when two zeroes in a row are found

(check-expect (add-machine empty) empty)
(check-expect (add-machine (list 5 0 0)) (list 5))
(check-expect (add-machine (list 6 2 0 5 9)) (list 8)) ; sums until a 0
;; no final 0 so doesn't report sum       ^
;; of 5 and 9

(check-expect (add-machine (list 5 3 0 -5 5 0 16 12 -4 0 3 0 0 5 5 0 3))
              (list  8      0       24      3))
(check-expect (add-machine (list 0 5 2 0 0)) empty)
;; edge case:                    ^ 0 has no prior item to add

(check-expect (add-machine (list 5 4 3 2 1)) empty) ;Edge case #2: no zeroes in list

;; stub
;(define (add-machine lon) empty)

(define (add-machine lon)
  (local [(define (add-until-0 lon accumulator listOfSums)
            (cond
              [(empty? lon) listOfSums]
              [(= (first lon) 0) (sum-collector (rest lon) (append listOfSums (list accumulator)))]
              [else (add-until-0 (rest lon) (+ accumulator (first lon)) listOfSums)]))

          (define (sum-collector lon listOfSums)
            (cond [(or (empty? lon) (= (first lon) 0)) listOfSums]
                  [else
                   (add-until-0 lon 0 listOfSums)]))]
    (sum-collector lon empty)))


;; ----------------------------------------------------------------
;; Part 3
;; ----------------------------------------------------------------

(require racket/string)

(define DIRECTORY-SIZE 50)
(define-struct file (name extension size subdirectory))
;; a file is a (make-file String String Natural (listof File))

;; a ListOfFile is oen of
;;      empty
;;      (cons File ListOfFile)

#;
(define (fn-for-file file)
  (local
    [(define (fn-for-file file)
       (...
        (file-name file)  ;; String
        (file-extension file) ;; String
        (file-size file) ;; Natural
        (fn-for-lof (file-subdirectory file)))) ;; (listof File)
     (define (fn-for-lof lof)
       (cond
         [(empty? lof) (...)]
         [else
          (...
           (fn-for-file (first lof)) ;; File
           (fn-for-lof (rest lof)))]))] ;; (listof File)
    (fn-for-file file)))

;; String -> ListOfString
(define (split-path path)
  (string-split path "/"))
(check-expect (split-path "/D2/D3/D4")
              (list "D2" "D3" "D4"))


(define F5 (make-file "F5" ".txt"  10005 empty))
(define F6 (make-file "F6" ".exe" 99432 empty))
(define F7 (make-file "F7" ".pdf" 258322 empty))
(define D5 (make-file "D5" "" DIRECTORY-SIZE empty))
(define D4 (make-file "D4" "" DIRECTORY-SIZE (list F6 D5)))
(define D3 (make-file "D3" "" DIRECTORY-SIZE (list D4 F7)))
(define D1 (make-file "D1" "" DIRECTORY-SIZE (list F5)))
(define D2 (make-file "D2" "" DIRECTORY-SIZE (list D3)))
(define F3 (make-file "F3" ".rkt" 1104 empty))
(define F4 (make-file "F4" ".rkt" 712043 empty))
(define D0 (make-file "D0" "" DIRECTORY-SIZE (list D1 D2 F3 F4)))

;; File ListOfString -> File
;; consumes a start directory and a list of subdirectories to traverse
;; returns the directory that represents the result of the path
(check-expect (cd D0 (list "D2" "D3")) D3)
(check-expect (cd D0 (list "D11")) false)

(define (cd current lod)
  (cond
    [(empty? lod) current]
    [else
     (local
       [(define (make-filename file)
          (string-append 
           (file-name file)(file-extension file)))
        (define (correct-directory? current)
          (string=? (make-filename current) (first lod)))
        (define try (filter correct-directory? (file-subdirectory current)))]
       (if (empty? try) false
           ;; assumes there is only 1 directory with the same name in a
           ;; subdirectory. seems reasonable, as that is how real file systems
           ;; work
           (cd (first try) (rest lod))))]))

;;;;;;;;;;;;
;; mkdir!
;;;;;;;;;;;;


; test cases
(check-expect (mkdir! D2 "/" "NEW DIRECTORY1")
              (make-file
               "D2"
               ""
               50
               (list
                (make-file "NEW DIRECTORY1" "" 50 '())
                (make-file
                 "D3"
                 ""
                 50
                 (list
                  (make-file "D4" "" 50 (list (make-file "F6" ".exe" 99432 '()) (make-file "D5" "" 50 '())))
                  (make-file "F7" ".pdf" 258322 '()))))))

(check-expect
 (mkdir! D0 "/D2/D3" "NEW DIRECTORY2")
 (make-file
  "D3"
  ""
  50
  (list
   (make-file "NEW DIRECTORY2" "" 50 '())
   (make-file "D4" "" 50 (list (make-file "F6" ".exe" 99432 '()) (make-file "D5" "" 50 '())))
   (make-file "F7" ".pdf" 258322 '()))))

;; if you follow the normal method of adding elements to list, the second directory
;; created here will be the first element of the list
(check-expect (mkdir! D0 "/D2/D3" "NEW DIRECTORY3")
              (make-file
               "D3"
               ""
               50
               (list
                (make-file "NEW DIRECTORY3" "" 50 '())
                (make-file "NEW DIRECTORY2" "" 50 '())
                (make-file "D4" "" 50 (list (make-file "F6" ".exe" 99432 '()) (make-file "D5" "" 50 '())))
                (make-file "F7" ".pdf" 258322 '()))))

(check-expect (mkdir! D0 "/D2/D99" "NOT CREATED") #false)

(check-expect D0
              (make-file
               "D0"
               ""
               50
               (list
                (make-file "D1" "" 50 (list (make-file "F5" ".txt" 10005 '())))
                (make-file
                 "D2"
                 ""
                 50
                 (list
                  (make-file "NEW DIRECTORY1" "" 50 '())
                  (make-file
                   "D3"
                   ""
                   50
                   (list
                    (make-file "NEW DIRECTORY3" "" 50 '())
                    (make-file "NEW DIRECTORY2" "" 50 '())
                    (make-file "D4" "" 50 (list (make-file "F6" ".exe" 99432 '()) (make-file "D5" "" 50 '())))
                    (make-file "F7" ".pdf" 258322 '())))))
                (make-file "F3" ".rkt" 1104 '())
                (make-file "F4" ".rkt" 712043 '()))))

; stub
;(define (mkdir! startDirectory path newDirectoryName) false)

(define (mkdir! startDirectory path newDirectoryName)
  (cond
    [(false? (cd startDirectory (split-path path))) false]
    [else
     (begin ;; execute several statements
       (set-file-subdirectory! (cd startDirectory (split-path path))
                               (append (list (make-file
                                              newDirectoryName
                                              ""
                                              DIRECTORY-SIZE
                                              empty))
                                       (file-subdirectory (cd startDirectory (split-path path)))
                                       )) ;; update file's subdirectory
       (cd startDirectory (split-path path)));Return parent file after update
     ]))

;cd returns false if path not found
;cd returns last file in path input as parameter
;need to use split-path on path parameter before giving to cd

;mkdir! returns false if location input parameter does not exist
;mkdir! returns file corresponding to the parent of where it created the subdirectory if successful

;=============================================================================================================
;  ;;;;;;;;;;;;;;;;;;;;;;;;
;  ;; extra credit:  rmdir!
;  ;;;;;;;;;;;;;;;;;;;;;;;;
;
;  ; stub
;  (define (rmdir! file string string2) false)
;
;  ; test cases
;  (check-expect (rmdir! D0 "/D99" "REMOVE") false)
;  (check-expect (mkdir! D0 "/D1" "TO DELETE")
;                (make-file "D1" "" 50 (list (make-file "TO DELETE" "" 50 '()) (make-file "F5" ".txt" 10005 '()))))
;
;  (check-expect (rmdir! D0 "/D1" "TO DELETE")
;                (make-file "D1" "" 50 (list (make-file "F5" ".txt" 10005 '()))))
;
;  (check-expect (rmdir! D0 "/D1" "TO DELETE") false)


