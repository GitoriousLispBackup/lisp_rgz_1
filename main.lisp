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

(DEFUN funu (n) (COND ((= n 0) (SET 'u '(x^4))) 
        ((= n 1) (SET 'u '(4x^3))) 
        ((= n 2) (SET 'u '(12x^2))) 
        ((= n 3) (SET 'u '(24x))) 
        ((= n 4) (SET 'u '(24)))
        ((> n 4) (SET 'u '(0)))))

; производная от sin
(DEFUN funv (n) (COND   
        ((= (REM n 4) 0) (SET 'v (LIST 'a^ n '(sin(ax)))))  
        ((= (REM n 4) 1) (SET 'v (LIST 'a^ n '(cos(ax)))))
        ((= (REM n 4) 2) (SET 'v (LIST 'a^ n '(- sin(ax)))))
        ((= (REM n 4) 3) (SET 'v (LIST 'a^ n '(- cos(ax)))))))

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

(DEFUN proizv (n) 
  (prog NIL (SET 'pr ()) 
        (loop for i from 0 TO n DO 
              (set 'pr 
                   (if (not (eq (car (funu (- n i))) 0))
                       (append pr 
                               (append '(+)) 
                               (bk n (- n i)) 
                               (append '(*)) 
                               (append (funu (- n i)) 
                                       (append '(*)) 
                                       (append (funv (- n i))))))))
        (set 'pr (cdr pr))
        ;(print (write-to-string pr)) 
        (set 'res_str (write-to-string pr)) ; list in string
        (set 'res_str1 (replace-all res_str " " ""))
        (set 'res_str1 (replace-all res_str1 "DIFFERENTIATE::" ""))
        (princ res_str1)
        ;(return pr)
        ))
