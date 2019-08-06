(defun 5-1 (p)
  (let ((result (+ p 5)))
    (list 'result 'is result)))


;;; Keyboard exercise 5-6 (Craps)

;;; a
(defun throw-die ()
  "Returns a random value between 1 and 6."
  (+ 1 (random 6)))

;;; b
(defun throw-dice ()
  "Returns the result of two individual throws as a list."
  (list (throw-die) (throw-die)))

;;; c
(defun snake-eyes-p (dice-throw)
  "Returns whether you have rolled snake eyes."
  (equal dice-throw '(1 1)))

(defun box-cars-p (dice-throw)
  "Returns whether you have rolled box cars."
  (equal dice-throw '(6 6)))

;;; d
(defun throw-value (dice-throw)
  "Helper function that adds the two throws."
  (+ (first dice-throw) (second dice-throw)))


(defun instant-win-p (dice-throw)
  "Returns whether you have won instantly (7 or 11)."
  (member (throw-value dice-throw) '(7 11)))

(defun instant-loss-p (dice-throw)
  "Returns whether you have lost instantly (2, 3, or 12)."
  (member (throw-value dice-throw) '(2 3 12)))

;;; e
(defun say-throw (dice-throw)
  "Returns the name of the throw or the sum if it has none."
  (cond ((snake-eyes-p dice-throw) 'snake-eyes)
        ((box-cars-p dice-throw) 'box-cars)
        (t (throw-value dice-throw))))

;;; f
(defun craps ()
  "Throw the dice. If you don't win or lose, establish a point."
  (let ((dice-throw (throw-dice)))
     (append (list 'throw (first dice-throw) ; THROW X and Y --
                   'and (second dice-throw)
                   '--)
             (cond ((instant-loss-p dice-throw)
                    (list (say-throw dice-throw) '-- 'you 'lose)) ; SNAKE-EYES/BOX-CARS -- YOU LOSE
                   ((instant-win-p dice-throw)
                    (list (say-throw dice-throw) '-- 'you 'win)) ; 7/11 -- YOU WIN
                   (t (list 'your 'point 'is (say-throw dice-throw))))))) ; YOUR POINT IS Z

;;; g
(defun try-for-point (point)
  "Continue throwing the dice until you make the point (win) or roll a 7 (lose)."
  (let* ((dice-throw (throw-dice))
         (value (throw-value dice-throw)))
    (append (list 'throw (first dice-throw) ; THROW X AND Y --
                  'and (second dice-throw) '--)
            (cond ((equal value point)
                   (list value '-- 'you 'win)) ; VALUE -- YOU WIN
                  ((equal value 7)
                   (list value '-- 'you 'lose)) ; VALUE -- YOU LOSE
                  (t (list value '-- 'throw 'again)))))) ; VALUE -- THROW AGAIN
