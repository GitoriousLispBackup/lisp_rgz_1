;;;; 2012-03-08 12:20:06
;;;; This is your lisp file. May it serve you well.

(in-package :differentiate)

(defun cpp_a (n k) 
    (if (= k 0) 1 
        (* (/ (+ (- n k) 1) k) (cpp_a n (- k 1)))))

(DEFUN bk_a (n k) 
    (COND ((< k 0) 
            (print '(error1))) 
        ((< n 0) 
            (print '(error2))) 
        ((< n k) 
            (print '(error3))) 
        ((<= k n) 
            (SET 'b (LIST (cpp_a n k)) ))))

(DEFUN funu_a (i) (COND 
                  ((= i 0) (SET 'u (list '1 'x^4))) 
                  ((= i 1) (SET 'u (list '4 'x^3))) 
                  ((= i 2) (SET 'u (list '12 'x^2))) 
                  ((= i 3) (SET 'u (list '24 'x))) 
                  ((= i 4) (SET 'u (list '24 1)))
                  ((> i 4) (SET 'u (list '0)))))



;(defun part_of_simple_a (n) (cond 
;    ((= n 0) (set 'y ()))
;    ((not (= n 0)) (set 'y (list 'a^ n '*)))
;  ))


(defun part_of_simple_a (n max_degree) (cond 
    ((= (- max_degree n) 0) (set 'y ()))
    ((not (= (- max_degree n) 0)) (set 'y (list 'a^ (- max_degree n) '*)))
  ))


; производная от sin
(DEFUN funv_a (n max_degree) (COND   
        ((= (REM (+ n (rem max_degree 2)) 2) 1) (SET 'v (part_of_simple_a n max_degree)) (append v '(cos(ax))))  
        ((= (REM (+ n (rem max_degree 2)) 2) 0) (SET 'v (part_of_simple_a n max_degree)) (append v '(sin(ax))))
        ;((= (REM n 4) 2) (SET 'v (part_of_simple_a n)) (append v (list '(- sin(ax)))))
        ;((= (REM n 4) 3) (SET 'v (part_of_simple_a n)) (append v (list '(- cos(ax)))))
                  ))

(defun sign_for_funv_a (n max_degree) (COND
                                      ((= (REM (+ n (* max_degree 3)) 4) 0) (SET 'v '(+)))  
                                      ((= (REM (+ n (* max_degree 3)) 4) 1) (SET 'v '(+)))
                                      ((= (REM (+ n (* max_degree 3)) 4) 2) (SET 'v '(-)))
                                      ((= (REM (+ n (* max_degree 3)) 4) 3) (SET 'v '(-)))           
                                      ))
                           

(defun replace-all_a (string part replacement &key (test #'char=))
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

(defun simplify_a (fir sec)
  (prog nil (set 'sd ())
        ;(print (car fir))
        ;(print (car sec))
        (set 'sd (* (car fir) (car sec)))
        (if (= sd 1) 
            (return '()))
        (return (list sd '*))
  ))

(DEFUN proizv_with_a (n) 
  (prog NIL (SET 'pr ()) 
        (loop for i from 0 TO n DO 
              
                   (if (not (eq (car (funu_a i)) 0))
                       (set 'pr (append pr 
                                        (append (sign_for_funv_a i (- n 1))) 
                                        (append (simplify_a (bk_a n i) (funu_a i)))
                                        ;(append '(*))
                                        (append (cdr (funu i)))
                                        (append '(*))
                                        (append (funv_a i n))))))
        ;(set 'pr (cdr pr))
        ;(print pr) 
        (set 'res_str (write-to-string pr :escape nil :right-margin 1000)) ; list in string
        ;(set 'res_str1 (replace-all_a res_str "\n" ""))
        (set 'res_str1 (replace-all_a res_str "^ " "^"))
        (set 'res_str1 (replace-all_a res_str1 " * " "*"))
        (set 'res_str1 (replace-all_a res_str1 " *" "*"))
        (set 'res_str1 (replace-all_a res_str1 " (" "("))
        (set 'res_str1 (replace-all_a res_str1 "(- " "-"))
        (set 'res_str1 (replace-all_a res_str1 "(+ " ""))
        (set 'res_str1 (replace-all_a res_str1 "))" ")"))
        (set 'res_str1 (replace-all_a res_str1 "*1" ""))
        (set 'res_str1 (replace-all_a res_str1 "DIFFERENTIATE::" ""))
        ;(princ res_str1)
        (return res_str1)
        ))
