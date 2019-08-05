;; global variables describing the bounds of the guess
(defparameter *low* 1)
(defparameter *high* 100)

;; computes a whole number between low and high
;; ash - arithmetic shift
(defun guess-my-number ()
  (ash (+ *low* *high*) -1))

;; sets the upper bound to be one fewer than the previous guess
;; and guesses the number again.
(defun smaller ()
  (setf *high* (- (guess-my-number) 1))
  (guess-my-number))

;; sets the lower bound to be one more than the previous guess
;; and guesses the number again.
(defun larger ()
  (setf *low* (+ (guess-my-number) 1))
  (guess-my-number))

;; resets the values of the bounds and guesses again.
;; used to restart the program when the computer correctly guesses.
(defun start-over ()
  (setf *low* 1)
  (setf *high* 100)
  (guess-my-number))

;;; global variables:
;;  (defparameter variable value) :   define *mutable* variable. convention to have *earmuffs*
;;  (setf variable value)         :   change the value of a *mutable* variable

;;; functions:
;;  (defun name (args) body)      :   define a global function
