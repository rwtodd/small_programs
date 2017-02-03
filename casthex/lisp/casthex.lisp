;; C A S T H E X
;; Cast an I-Ching Hexagram

(defpackage :com.waywardcode.casthex
  (:use :common-lisp)
  (:export #:cast-hex))

(in-package :com.waywardcode.casthex)

;; A list of the hexagrams, in numeric (lines) order
(defconstant +hex-names+
  #("02. K'un -- Earth"
    "24. Fu -- Return"
    "07. Shih -- The Army"
    "19. Lin -- Overseeing"
    "15. Ch'ien -- Humility"
    "36. Ming I -- Concealment of Illumination"
    "46. Sheng -- Rising"
    "11. T'ai -- Tranquility"
    "16. Yu -- Enthusiasm"
    "51. Chen -- Thunder"
    "40. Hsieh -- Liberation"
    "54. Kuei Mei -- Making a Young Girl Marry"
    "62. Hsiao Kuo -- Predominance of the Small"
    "55. Feng -- Richness"
    "32. Heng -- Constancy"
    "34. Ta Chuang -- Great Power"
    "08. Pi -- Accord"
    "03. Chun -- Difficulty"
    "29. K'an -- Mastering Pitfalls"
    "60. Chieh -- Discipline"
    "39. Chien -- Halting"
    "63. Chi Chi -- Settled"
    "48. Ching -- The Well"
    "05. Hsu  -- Waiting"
    "45. Ts'ui -- Gathering"
    "17. Sui -- Following"
    "47. K'un -- Exhaustion"
    "58. Tui -- Joy"
    "31. Hsien -- Sensitivity"
    "49. Ko -- Revolution"
    "28. Ta Kuo -- Excess of the Great"
    "43. Kuai -- Parting"
    "23. Po -- Stripping Away"
    "27. I -- Lower Jaw (Nourishment)"
    "04. Meng -- Darkness"
    "41. Sun -- Reduction"
    "52. Ken -- Mountain"
    "22. Pi -- Adornment"
    "18. Ku -- Degeneration"
    "26. Ta Ch'u -- Nurturance of the Great"
    "35. Chin -- Advance"
    "21. Shih Ho -- Biting Through"
    "64. Wei Chi -- Unsettled"
    "38. K'uei -- Disharmony"
    "56. Lu -- Travel"
    "30. Li -- Fire"
    "50. Ting -- The Cauldron"
    "14. Ta Yu -- Great Possession"
    "20. Kuan -- Observation"
    "42. I -- Increase"
    "59. Huan -- Dispersal"
    "61. Chung Fu -- Faithfulness in the Center"
    "53. Chien -- Gradual Progress"
    "37. Chia Jen -- People in the Home"
    "57. Sun -- Wind"
    "09. Hsiao Ch'u -- Nurturance by the Small"
    "12. Pi -- Obstruction"
    "25. Wu Wang -- Fidelity (No Error)"
    "06. Sung -- Contention"
    "10. Lu -- Treading"
    "33. Tun -- Withdrawal"
    "13. T'ung Je^n -- Sameness with People"
    "44. Kou -- Meeting"
    "01. Chi'en -- Heaven"))

;; Methods of casting... MTD is an expression returning a number to be added to #\6
;; for each of the 6 lines of a casting.
(defmacro casting-of (mtd) `
  (map-into (make-string 6) #'(lambda () (code-char (+ (char-code #\6) ,mtd)))))

(defun cast-coins () (casting-of (+ (random 2) (random 2) (random 2))))
(defun cast-static () (casting-of (+ (random 2) 1)))	      
(defun cast-stalks ()
  (casting-of
     (let ((val (random 16)))
       (cond ((= val 0)  0)
	     ((< val 6)  1)
	     ((< val 13) 2)
	     (t          3)))))

;; Decode a hexagram into King Wen numbers, and string representations of each line
(defun decode-hex (str)
  (let ((w1 0)      ; king wen number 1
	(w2 0)      ; king wen number 2
	(reps nil)) ; string representations
    (loop for i from 5 downto 0 do
	  (setf w1 (* w1 2)
		w2 (* w2 2)
		reps (cons (case (aref str i)
				 (#\6 (incf w2)           "  ---   ---  =>  ---------")
				 (#\7 (incf w1) (incf w2) "  ---------      ---------")
				 (#\8                     "  ---   ---      ---   ---")
				 (#\9 (incf w1)           "  ---------  =>  ---   ---"))
			   reps)))
    (values w1 w2 (nreverse reps))))

;; Display hexagrams with names to the user
(defun display-hex (str)
  (multiple-value-bind (w1 w2 reps) (decode-hex str)
		       (let ((changed (/= w1 w2)))
			 (format t "Casting: ~s~%~%" str)
			 (mapc #'(lambda (r) (princ (if changed r (subseq r 0 11))) (terpri)) reps)
			 (format t "~%~a~%" (aref +hex-names+ w1))
			 (if changed
			     (format t " = Changing To =>~%~a~%" (aref +hex-names+ w2))))))

;; This is the exported, top-level function of the program. It lets the user
;; specify which casting method to use.
(defun cast-hex (&optional (method "coins"))
  (display-hex (cond
		((equal method "coins") (cast-coins))
		((equal method "stalks") (cast-stalks))
		((equal method "static") (cast-static))
		((and (stringp method)
		      (= (length method) 6)
		      (every #'(lambda (ch) (char<= #\6 ch #\9)) method))
		 method)
		(t    (error "Usage: display-hex [\"coins\"|\"static\"|\"stalks\"|<casting>]")))))


