;;;; 2012-03-08 12:20:06
;;;; This is your lisp file. May it serve you well.

(in-package :differentiate)

(defun cpp (n k) 
    (if (= k 0) 1 
        (* (/ (+ (- n k) 1) k) (cpp n (- k 1)))))

(DEFUN bk (n k) 
    (COND ((< k 0) 
            (print '(error1))) 
        ((< n 0) 
            (print '(error2))) 
        ((< n k) 
            (print '(error3))) 
        ((<= k n) 
            (SET 'b (LIST (cpp n k)) ))))



(defun part_of_simple (n a max_degree) (cond 
    ((= (- max_degree n) 0) (set 'y (list 1)))
    ((not (= (- max_degree n) 0)) (set 'y (list (expt a (- max_degree n)))))
  ))

; производная от sin
(DEFUN funv (n a max_degree) (COND  
        ((= (REM (+ n (rem max_degree 2)) 2) 1) (SET 'v (part_of_simple n a max_degree)) (append v  (list '*) '(cos) (list (list a 'x)) ))  
        ((= (REM (+ n (rem max_degree 2)) 2) 0) (SET 'v (part_of_simple n a max_degree)) (append v  (list '*) '(sin) (list (list a 'x)) ))
        ;((= (REM n 4) 2) (SET 'v (part_of_simple n a)) (append v (list '*) '(sin) (list (list a 'x)) ))
        ;((= (REM n 4) 3) (SET 'v (part_of_simple n a)) (append v (list '*) '(sin) (list (list a 'x)) ))
                  ))

(defun sign_for_funv (n max_degree) (COND
                                      ((= (REM (+ n (* max_degree 3)) 4) 0) (SET 'v '(+)))  
                                      ((= (REM (+ n (* max_degree 3)) 4) 1) (SET 'v '(+)))
                                      ((= (REM (+ n (* max_degree 3)) 4) 2) (SET 'v '(-)))
                                      ((= (REM (+ n (* max_degree 3)) 4) 3) (SET 'v '(-)))           
                                      ))
                           

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun simplify (fir sec)
  (prog nil (set 'sd ())
        ;(print (car fir))
        ;(print (car sec))
        (set 'sd (* (car fir) (car sec)))
        (return (list sd))
  ))

(DEFUN funu (n) (COND 
                  ((= n 0) (SET 'u (list '1 'x^4))) 
                  ((= n 1) (SET 'u (list '4 'x^3))) 
                  ((= n 2) (SET 'u (list '12 'x^2))) 
                  ((= n 3) (SET 'u (list '24 'x)))
                  ((= n 4) (SET 'u (list '24 '1)))
                  ((> n 4) (SET 'u (list '0 ())))))

(DEFUN proizv (n) 
  (prog NIL (SET 'pr ())
        (print "type A")
        (set 'a (read))
        (print n)
        ;(set 'n (- n 1))
        (print n)
        (loop for i from 0 TO n DO 
              (set 'pr 
                   (if (not (eq (car (funu i)) 0))
                       (append pr 
                               (append (sign_for_funv i (- n 1))) 
                               (append 
                                (simplify 
                                 (simplify (bk n i) (funu i)) 
                                 (funv i a n)))
                               (append '(*))
                               (append (cdr (funu i)))
                               (append (cdr (funv i a n)))))))
        ;(set 'pr (cdr pr))
        ;(print (write-to-string pr)) 
        (set 'res_str (write-to-string pr)) ; list in string
        (set 'res_str1 (replace-all res_str " " ""))
        (set 'res_str1 (replace-all res_str1 "DIFFERENTIATE::" ""))
        ;(princ res_str1)
        (return res_str1)
        ))


