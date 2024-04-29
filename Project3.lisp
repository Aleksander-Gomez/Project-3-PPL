;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project 3: Functional Programming, CS3210                        ;;
;; Authors: Aleks Gomez, Fin Martinez, Jordan Snow, and Kyle Sterner;;
;; Additional coding assistance provided by ChatGPT                 ;;
;; Project framework provided by ThienNgo Nguyen Le                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return T if item is a member of set.
(defun set-member (set item)
  "Check if item is a member of set."
  (cond
    ((null set) nil)                 
    ((equal (car set) item) t)       
    (t (set-member (cdr set) item))))

;;Test set-member
(defun set-member-test ()
  (if (set-member '(1 2 3) 1)
    (format t "Test passed: (set-member '(1 2 3) 1) => T~%")
    (format t "Test failed: (set-member '(1 2 3) 1) => T~%")))

;; Load tests with C-c C-k (don't know if this'll work)

;;Run tests
(set-member-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the union of set-1 and set-2.

(defun set-union (set-1 set-2)
  (COND
    ((null set-1) set-2)
    ((null set-2) set-1)
    ((member (car set-1) set-2)
     (set-union (cdr set-1) set-2))
    (t (cons (car set-1) (set-union (cdr set-1) set-2)))))

;;---->)

;; Test cases

(defun set-union-test ()
  (let ((set-1 '(1 2 3 4))
	(set-2 '(3 4 5 6)))
    (assert (equal (set-union set-1 set-2) '(1 2 3 4 5 6)))

    (let ((set-1 '(1 2 3)
	  (set-2 '(4 5 6)))

	  (assert (equal (set-union set-1 set-2) '(1 2 3 4 5 6))))

      (let ((set-1 '(1 2 3))
	    ((set-2 '(1 2 3)))
	  (assert (equal (set-union set-1 set-2) '(1 2 3))))))

;Run tests
 (set-union-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the intersection of set-1 and set-2.

(defun set-intersection (set-1 set-2)
  (COND
    ((null set-1) '())
    ((member (car set-1) set-2)
     (cons (car set-1) (set-intersection (cdr set-1) set-2)))
    (t (set-intersection (cdr set-1) set-2))))
    
;;---->)

(defun set-intersection-test()
  (let ((set-1 '(1 2 3 4))
	(set-2 '(2 3 4 5 6)))
    (assert (equal (set-union set-1 set-2) '(2 3 4)))

    (let ((set-1 '(1 2 3))
	  (set-2 '(4 5 6)))
      (assert (equal (set-union set-1 set-2) '())))

;Run tests
(set-intersection-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the difference of set-1 and set-2.
(defun set-diff (set-1 set-2)
  "Check the difference between two sets"
  (cond ((null set-1) nil)
    ((member (car set-1) set-2) (set-difference (cdr set-1) set-2))
    (t (cons (car set-1) (set-difference (cdr set-1) set-2)))))

;;Test set-diff
(defun set-diff-test ()
  (format t "Test 1: ")
  (let ((result (set-diff '(1 2 3 4) '(3 4 5))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))

  (format t "Test 2: ")
  (let ((result (set-diff '(1 2 3) '(1 2 3))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))

  (format t "Test 3: ")
  (let ((result (set-diff '() '(1 2 3))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))

  (format t "Test 4: ")
  (let ((result (set-diff '(1 2 3) '())))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))
  
  (format t "Test 5: ")
  (let ((result (set-diff '(1 1 2 2 3 3) '(2 3 4))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))
  
  (format t "Test 6: ")
  (let ((result (set-diff '(a b c) '(c d e))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result)))
  
  (format t "Test 4: ")
  (let ((result (set-diff '(1 2 a b) '(b c 1))))
    (if (null result)
      (format t "No difference found. ~%")
      (format t "Difference found: ~a~%" result))))

;;Run Tests
(set-diff-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the exclusive or of a and b
(defun boolean-xor (a b)
  "Compute the XOR of two boolean values 'a' and 'b'."
  (cond ((and a b) nil)    ;Both true => false
    ((or a b) t)           ;Either true => true
    (t nil)))              ;Both false => false

;;Test boolean-xor
(defun boolean-xor-test ()
  (format t "Test with both inputs true:")
  (let ((result (boolean-xor t t)))
    (if result
      (format t "(boolean-xor t t) => t~%")
      (format t "(boolean-xor t t) => nil~%")))

  (format t "Test with first input 'true' & second input 'nil':")
  (let ((result (boolean-xor t nil)))
    (if result
      (format t "(boolean-xor t nil) => t~%")
      (format t "(boolean-xor t nil) => nil~%")))
  
  (format t "Test with first input 'nil' & second input 'true':")
  (let ((result (boolean-xor nil t)))
    (if result
      (format t "(boolean-xor nil t) => t~%")
      (format t "(boolean-xor nil t) => nil~%")))

  (format t "Test with both inputs false:")
  (let ((result (boolean-xor nil nil)))
    (if result
      (format t "(boolean-xor nil nil) => t~%")
      (format t "(boolean-xor nil nil) => nil~&"))))
  
;;Run tests
(boolean-xor-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the implication of a and b
;; (defun boolean-implies (a b)

;;<Your implementation go here >

(defun boolean-implies (a b)
;; Check if 'a' implies 'b'
  (if (and a (not b))           ; If 'a' is true and 'b' is false
      nil                       ; Return false, as 'a' does not imply 'b'
      t))                       ; Otherwise, return true, as 'a' implies 'b'



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if both 'a' and 'b' are true or both are false
(defun boolean-iff (a b)
  (if (and a b)                    ; If both 'a' and 'b' are true
     t                             ; Return true, as both conditions are met for if and only if
      (if (and (not a) (not b))    ; If both 'a' and 'b' are false     
	  t                              ; Return true, as both conditions are met for if and only if
	  nil)))                         ; Otherwise, return false


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate a boolean expression.
(defun boolean-eval (exp)
  (cond ((eq exp 't) t)
        ((eq exp 'nil) nil)
        ((eq (car exp) 'not) (not (boolean-eval (cadr exp)))) ; NOT function in LISP 
        ((eq (car exp) 'and) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp)))) ; AND function in LISP
        ((eq (car exp) 'or) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp)))) ; OR function in LISP
        ((eq (car exp) 'xor) (xor (boolean-eval (cadr exp)) (boolean-eval (cddr exp)))) ; XOR defined
        ((eq (car exp) 'implies) (implies (boolean-eval (cadr exp)) (boolean-eval (cddr exp)))) ; IMPLIES defined 
        ((eq (car exp) 'iff) (iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp)))))) ; IFF defined

(defun xor (a b)
  (and (or a b) (not (and a b))))

(defun implies (a b)
  (or (not a) b))

(defun iff (a b)
  (and (implies a b) (implies b a)))
