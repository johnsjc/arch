;;; Global constants
(defparameter +player+ 1) ; X

(defparameter +computer+ 10) ; O

(defparameter +diagonals+ '((1 5 9) (3 5 7))) ; diagonal triplets

(defparameter +triplets+
  (append '((1 2 3) (4 5 6) (7 8 9) ; horizontal triplets
            (1 4 7) (2 5 8) (3 6 9)) ; vertical triplets
          +diagonals+))

(defparameter +corners+ '(1 3 7 9)) ; corner spaces

(defparameter +sides+ '(2 4 6 8)) ; side spaces


(defun make-board ()
  "Create an empty tic-tac-toe board.
   0  - empty
   1  - filled by X (player)
   10 - filled by O (computer)
   nth can be used to get the nth square."
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  "Converts the value v to the corresponding letter.
   0  - <space>
   1  - X
   10 - O"
  (cond ((equal v 1) "X")
        ((equal v 10) "O")
        (t " ")))

(defun print-row (x y z)
  "Prints a single row of the board."
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

(defun make-move (player position board)
  "Sets the POSITION on BOARD equal to PLAYER (X or O)"
  (setf (nth position board) player) board)

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
  "Returns T if BOARD is in a winning state"
  (let ((sums (compute-sums board)))
    (or (member (* 3 +computer+) sums)
        (member (* 3 +player+) sums))))

(defun board-full-p (board)
  "Returns T if BOARD is full"
  (not (member 0 board)))

(defun play-one-game ()
  "Sets up a tic-tac-toe game"
  (let ((board (make-board)))
    (print-board board)
    (if (y-or-n-p "Would you like to go first? ")
        (player-move board)
        (computer-move board))))

(defun player-move (board)
  "Execute the player's move"
  (let* ((position (read-a-legal-move board))
         (new-board (make-move +player+ position board)))
    (print-board new-board)
    (cond ((winnerp new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  "Reads until a legal move is entered by the player"
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
  "Executes the computer's move"
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
  "Chooses the best available move on the board"
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (exploit-two-on-one board)
      (squeeze-play-attack board)
      (two-on-one-setup board)
      (random-move board)))

(defun random-move (board)
  "Strategy to choose a random move"
  (labels ((choose-random-space (board)
             (let ((position (+ 1 (random 9))))
               (if (zerop (nth position board))
                   position
                   (pick-random-empty-position board)))))    
    (list (choose-random-space board)
          "random move")))

(defun make-three-in-a-row (board)
  "Strategy to make 3 in a row and win"
  (let ((position (win-or-block board
                                (* 2 +computer+))))
    (and position (list position "make 3 in a row"))))

(defun block-opponent-win (board)
  "Strategy to block the player from making 3 in a row"
  (let ((position (win-or-block board
                                (* 2 +player+))))
    (and position (list position "block opponent"))))

(defun win-or-block (board target-sum)
  "Helper function to find the empty space for a win or block"
  (let ((triplet (find-if
                  #'(lambda (triplet)
                      (equal (sum-triplet board triplet)
                             target-sum))
                  +triplets+)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  "Find an empty position in a list of board positions"
  (find-if #'(lambda (position)
               (zerop (nth position board)))
           squares))


;;; Advanced strategies

(defun check-diagonals (board center-player target-sum)
  "Helper function to check diagonal state on the board"
  (and (equal center-player (nth 5 board))
       (find-if #'(lambda (diagonal)
                    (equal target-sum (sum-triplet board diagonal)))
                +diagonals+)))

(defun squeeze-play-p (board)
  "Returns T if there is a squeeze play on BOARD
   e.g. an X-O-X pattern on a diagonal"
  (check-diagonals board +computer+ 12))

(defun two-on-one-p (board)
  "Returns T if there is a two-on-one on BOARD
   e.g. a X-X-O or O-X-X pattern on a diagonal"
  (check-diagonals board +player+ 12))

(defun block-squeeze-play (board)
  "Strategy to defend against a squeeze play"
  (when (squeeze-play-p board)
    (let ((position (find-empty-position board +sides+)))
      (and position (list position "block squeeze play")))))


(defun block-two-on-one (board)
  "Strategy to defend against a two-on-one"
  (when (two-on-one-p board)
    (let ((position (find-empty-position board +corners+)))
      (and position (list position "block two-on-one")))))

(defun find-opposite-corner (board c1 c2 player)
  "Find the opposite corner of the one currently
   occupied by PLAYER"
  (when (member player (list (nth c1 board)
                             (nth c2 board)))
    (find-empty-position board (list c1 c2))))

(defun squeeze-play-attack-p (board)
  "Returns T if there is an opportunity for a squeeze play
   on BOARD (for the computer)"
  (check-diagonals board +player+ 11))

(defun two-on-one-setup-p (board)
  "Returns T if there is an opportunity for a two on one
   on BOARD (for the computer)"
  (or (check-diagonals board +computer+ 11)
      (check-diagonals board 0 11)))

(defun squeeze-play-attack (board)
  "Attempts to set up a squeeze play if one exists."
  (when (squeeze-play-attack-p board)
    (let ((position (or (find-opposite-corner board 1 9 +computer+)
                        (find-opposite-corner board 3 7 +computer+))))
      (and position (list position "set up squeeze play")))))

(defun two-on-one-setup (board)
  "Attempts to set up a two-on-one attack if it exists."
  (when (two-on-one-setup-p board)
    (let ((position (or (find-opposite-corner board 1 9 +player+)
                        (find-opposite-corner board 3 7 +player+)
                        5)))
      (and position (list position "set up two-on-one")))))

(defun exploit-two-on-one (board)
  "Exploits a two-on-one attack after it has been set up."
  (when (equal (nth 5 board) +computer+)
    (or (two-on-one board 1 2 4 3 7)
        (two-on-one board 3 2 6 1 9)
        (two-on-one board 7 4 8 1 9)
        (two-on-one board 9 6 8 3 7))))

(defun two-on-one (board position side1 side2 corner1 corner2)
  "Helper function to determine if the given position is a successful
   two-on-one exploitation"
  (and (equal (sum-triplet board
                           (list corner1 5 corner2)) 21)
       (zerop (nth position board))
       (zerop (nth side1 board))
       (zerop (nth side2 board))
       (list position "exploit two-on-one")))
