(defparameter *total-glasses* 0)

(defun 10-2 (n)
  (incf *total-glasses* n)
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))


(defparameter *friends* nil)
(defparameter *n-already-met* 0)

(defun 10-3 (person)
  (cond ((equal person (first *friends*))
         'we-just-met)
        ((member person *friends*)
         (incf *n-already-met*)
         'we-know-each-other)
        (t (push person *friends*)
           'pleased-to-meet-you)))

(defun 10-4 (person)
  (cond ((member person *friends*)
         (setf *friends* (remove person *friends*))
         'forgotten)
        (t (list 'i 'dont 'know person))))


;;; Tic-tac-toe

(defun make-board ()
  "Create an empty tic-tac-toe board.
   0  - empty
   1  - filled by O
   10 - filled by X
   nth can be used to get the nth square."
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  "Converts the value v to the corresponding letter.
   0  - <space>
   1  - O
   10 - X"
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  "Print a single row of the board."
  (format t
          "~&   ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  "Print the tic-tac-toe board."
  (let ((separator "-----------"))
    (format t "~%")
    (print-row (nth 1 board)
               (nth 2 board)
               (nth 3 board))
    (format t "~&  ~A" separator)
    (print-row (nth 4 board)
               (nth 5 board)
               (nth 6 board))
    (format t "~&  ~A" separator)
    (print-row (nth 7 board)
               (nth 8 board)
               (nth 9 board))
    (format t "~%~%")))

(defparameter +computer+ 10)
(defparameter +player+ 1)

(defun make-move (player position board)
  "Sets the POSITION on BOARD equal to the PLAYER"
  (setf (nth position board) player) board)

(defparameter +triplets+
  '((1 2 3) (4 5 6) (7 8 9) ; horizontal triplets
    (1 4 7) (2 5 8) (3 6 9) ; vertical triplets
    (1 5 9) (3 5 7))) ; diagonal triplets

(defun sum-triplet (board triplet)
  "Determines the sum of the TRIPLET on BOARD"
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet)  board)))

(defun compute-sums (board)
  "Returns a list of all triplet sums on BOARD"
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          +triplets+))

(defun winnerp (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 +computer+) sums)
        (member (* 3 +player+) sums))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun play-one-game ()
  (let ((board (make-board)))
    (print-board board)
    (if (y-or-n-p "Would you like to go first? ")
        (player-move board)
        (computer-move board)))

(defun player-move (board))
  (let* ((position (read-a-legal-move board))
         (new-board (make-move +player+ position board)))
    (print-board new-board)
    (cond ((winnerp new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((position (read)))
    (cond ((not (and (integerp position)
                     (<= 1 position 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth position board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t position))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (position (first best-move))
         (strategy (second best-move))
         (new-board (make-move +computer+ position board)))
    (format t "~&My move: ~S" position)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winnerp new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (player-move new-board)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  "Choose a random empty space on the board"
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((position (+ 1 (random 9))))
    (if (zerop (nth position board))
        position
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((position (win-or-block board
                                (* 2 +computer+))))
    (and position (list position "make 3 in a row"))))

(defun block-opponent-win (board)
  (let ((position (win-or-block board
                                (* 2 +player+))))
    (and position (list position "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (triplet)
                      (equal (sum-triplet board triplet)
                             target-sum))
                  +triplets+)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (position)
               (zerop (nth position board)))
           squares))

;;; End of tic-tac-toe

(defun 10-5 (x y)
  (let* ((largest (max x y))
         (average (/ (+ x y) 2.0))
         (percentage (* 100 (/ average largest))))
    (list 'average average 'is percentage 'percent 'of 'max  largest)))
