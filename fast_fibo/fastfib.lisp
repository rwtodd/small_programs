;; translated from the Haskell at:
;; https://www.nayuki.io/page/fast-fibonacci-algorithms
(defun fibonacci (n)
  (labels ((fib (n)
		(if (= n 0)
		    (values 0 1)
		  (multiple-value-bind (a b) (fib (floor n 2))
				       (let ((c (* a (- (* b 2) a)))
					     (d (+ (* a a) (* b b))))
					 (if (evenp n)
					     (values c d)
					   (values d (+ c d))))))))
	  (values (fib n))))

		     
