;; C A S T G E O
;; Create a geomantic shield
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(setf *random-state* (make-random-state t))  ; new random nums each time loaded

(defpackage :com.waywardcode.geomancy
  (:use :common-lisp)
  (:export #:cast-geo))

(in-package :com.waywardcode.geomancy)

;; Create a random set of moms, along with
;; the transpositions as daughters.
(defun moms-daughters () 
  (let ((ans (make-array '(8 4))))
    (loop for i from 7 downto 4 do
	  (dotimes (j 4)
	    (let ((v (random 2)))
	      (setf (aref ans    i       j   ) v
		    (aref ans (- 3 j) (- 7 i)) v)))
	  finally (return ans))))

;; Combine adjacent geomantic figures, returning
;; an array 1/2 the size of the input.
(defun combine (src)
  (let ((ans (make-array (list (/ (array-dimension src 0) 2)  
                               4))))
    (dotimes (i (array-dimension ans 0) ans) 
      (dotimes (j 4)
	(setf (aref ans i j) 
	      (boole boole-xor (aref src (* i 2)       j)
		     (aref src (+ (* i 2) 1) j)))))))

(defconstant +LINES+ (vector "*   *"  "  *  "))
(defconstant +SPACES+ "                                         ")

;; Displays a line of geomantic figures FIGS, separated
;; by MSP spaces, with ISP initial spaces.
(defun displine (isp msp figs)
  (let ((ispaces (subseq +SPACES+ 0 isp))
	(mspaces (subseq +SPACES+ 0 msp)))
    (dotimes (j 4)
      (princ ispaces)
      (dotimes (i (array-dimension figs 0))
	(princ (aref +LINES+ (aref figs i j)))
	(princ mspaces))
      (princ #\Newline))
    (princ #\Newline)))

;; The main function.
(defun cast-geo () 
  (let* ((line1     (moms-daughters))
         (nieces    (combine line1))
         (witnesses (combine nieces))
         (judge     (combine witnesses)))
    (displine 2   5 line1)
    (displine 7  15 nieces)
    (displine 17 35 witnesses)
    (displine 37  0 judge)))

