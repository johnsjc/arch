;;; 9 -- I/O

(defun 9-1 ()
  "Prints some text using carriage return ~&"
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")  
  (format t "~&but there are no old, bold pilots."))

(defun 9-2 (n)
  "Draws a line N asterisks long."
  (labels ((draw-star (n) ; helper function to draw an asterisk
             (cond ((zerop n) nil)
                   (t (format t "*")
                      (draw-star (- n 1))))))
    (format t "~&") ;carriage return to begin the line
    (draw-star n))) ; call helper function

(defun 9-3 (width height)
  "Draws a box of asterisks with dimensions WIDTH x HEIGHT"
  (cond ((zerop height) nil)
        (t (9-2 width)
           (9-3 width (- height 1)))))

(defun 9-4 (n-bottles)
  "Sings N bottles of beer on the wall"
  (cond ((zerop n-bottles) nil) ; no more bottles, return
        (t (format t "~&~S bottles of beer on the wall," n-bottles)
            (format t "~&~S bottles of beer!" n-bottles)
            (format t "~&Take one down,")
            (format t "~&Pass it around,")
            (format t "~&~S bottles of beer on the wall.~%~%" ; 2 newlines to separate verses
                    (- n-bottles 1))
            (9-4 (- n-bottles 1)))))

(defun 9-5 (board)
  "Pretty prints a Tic-Tac-Toe board.
   Board is a list where each element is X, O or nil"
  ;; helper function to print a line of the board
  (labels ((print-line (line)
             (format t "~& ~A | ~A | ~A"
                     (first line)
                     (second line)
                     (third line))))
    (let ((separator "-----------") ; separator between lines
          (board (sublis '((nil . " ")) ; replace nils with spaces
                         board)))
      (print-line board) ; print the first 3 elements
      (format t "~&~A" separator)
      (print-line (nthcdr 3 board)) ; print the second 3 elements
      (format t "~&~A" separator)
      (print-line (nthcdr 6 board))))) ; print the last 3 elements

(defun 9-6 ()
  "Computes an hourly worker's gross pay.
   Inputs are prompted from the user."
  (format t "~&Enter the hourly rate: ")
  (let ((hourly-rate (read)))
    (format t "Enter the number of hours worked: ")
    (let ((hours-worked (read)))
      (format t "Pay: ~S"
              (* hourly-rate hours-worked)))))

(defun 9-7 ()
  "The cookie monster will prompt for a cookie until you give one"
  (format t "~&Give me cookie!!!")
  (format t "~&Cookie? ")
  (let ((input (read)))
    (cond ((equal input 'cookie)
           (format t "Thank you!"))
          (t (format t "No want ~S...~%~%" input)
             (9-7)))))

;;; Keyboard exercise -- graph

;; a
(defun space-over (n)
  "Moves the cursor to the right by printing N spaces"
  (cond ((plusp n)
         (format t " ")
         (space-over (- n 1)))
        ((zerop n) nil)
        (t (format t "~&Error!"))))

;; b
(defun plot-one-point (plotting-string y-val)
  "Prints PLOTTING STRING in column Y-VAL"
  (let ((width (length plotting-string)))
    (space-over (* width y-val))
    (format t "~A~%" plotting-string)))

;; c
(defun plot-points (plotting-string y-values)
  "Plots a list of y values POINTS"
  (mapcar #'
   (lambda (y) (plot-one-point plotting-string y))
   y-values))

;; d
(defun generate (x y)
  "Generates a list of integers from X to Y inclusive"
  (cond ((equalp x y) (list y))
        (t (cons x
                 (generate (+ x 1) y)))))

;; e
(defun prompt (message)
  "Prompts the user for input"
  (format t "~A" message)
  (read))

(defun make-graph ()
  "Prompts the user for the graph values and plots the graph."
  (let* ((func (prompt "Function to graph? (available: square) "))
         (start (prompt "Starting x value? "))
         (end (prompt "Ending x value? "))
         (plotting-string (prompt "Plotting string (include quotes)? "))
         (y-values (mapcar func (generate start end))))

    (plot-points plotting-string y-values)))

;; f
(defun square (n)
  (* n n))


;;; Serialization and De-serialization

;;; Serializes Lisp objects to a file:
;;;
;;; ("This is a lisp object" ((one . uno) (two . dos)) (a b c (d e (f))));; "This is a Lisp object"
;;;
;;; =>
;;;
;;; "This is a Lisp object"
;;; ((one . uno)
;;;  (two . dos))
;;; (a b c (d e (f)))
(defun serialize (objects path)
  (labels ((write-all-objects (stream objects)
             (cond ((listp objects)
                    (cond ((null objects) nil)
                          (t (format stream "~S~%" (first objects))
                             (write-all-objects stream (rest objects)))))
                   (t (format stream "~S~%" objects)))))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-all-objects stream objects))))


;;; De-serializes a file like the following:
;;;
;;; "This is a Lisp object"
;;; ((one . uno)
;;;  (two . dos))
;;; (a b c (d e (f)))
;;;
;;; Into a list:
;;; ("This is a lisp object" ((one . uno) (two . dos)) (a b c (d e (f))));; "This is a Lisp object"
(defun deserialize (path)
  (labels ((read-all-objects (stream eof-indicator)
             ;; by supplying nil and eof-indicator to read
             ;; when it reaches the end of the file it will return the indicator
             ;; instead of throwing an error
             (let ((result (read stream nil eof-indicator)))
               (if (eq result eof-indicator)
                   nil
                   (cons result
                         (read-all-objects stream eof-indicator))))))
    
    (with-open-file (stream path)
      (let ((contents
              (read-all-objects stream (list '$eof$))))
        (format t "~&Read ~S objects from file" (length contents))
        contents))))

(defun 9-11 (my-list)
  "Prints MY-LIST in dot notation"
  (cond ((atom my-list)
         (format t "~S" my-list))
        (t
         (format t "(")
         (9-11 (car my-list))
         (format t " . ")
         (9-11 (cdr my-list))
         (format t ")"))))

(defun 9-15 (my-list)
  "Prints MY-LIST in hybrid notation"
  (labels ((hybrid-print-car (car-piece)
             (format t "(")
             (9-15 car-piece))

           (hybrid-print-cdr (cdr-piece)
             (cond ((null cdr-piece)
                    (format t ")"))
                   ((atom cdr-piece)
                    (format t " . ~S)" cdr-piece))
                   (t (format t " ")
                      (9-15 (first cdr-piece))
                      (hybrid-print-cdr (rest cdr-piece))))))
    
    (cond ((atom my-list)
           (format t "~S" my-list))
          (t (hybrid-print-car (car my-list))
             (hybrid-print-cdr (cdr my-list))))))
  
