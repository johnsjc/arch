(defun 7-1 ()
  "Uses mapcar to add one to every element of a list."
  (flet ((add1 (n)
           (+ n 1)))
    (mapcar #'add1 '(1 3 5 7 9))))

(defun 7-2 ()
  "Uses mapcar to extract social security numbers from a table."
  (let ((daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                        (kent clark 089-52-6787 reporter)
                        (lane lois 951-26-1438 reporter)
                        (white perry 355-16-7439 editor))))
    (flet ((extract-ssn (entry)
             (third entry)))
      (mapcar #'extract-ssn daily-planet))))

(defun 7-3 ()
  "Apply the zerop predicate to a list."
  (mapcar #'zerop '(2 0 3 4 0 -5 -6)))

(defun 7-4 ()
  "Apply a predicate to a list that determines if n > 5"
  ;; mapcar takes only one input so we define a helper function
  (flet ((greater-than-fivep (n)
           (> n 5)))
    (mapcar #'greater-than-fivep '(2 0 3 4 0 -5 -6))))

(defun 7-5 (x)
  "A lambda expression that subtracts 7 from a number."
  ((lambda (n) (- n 7)) x))

(defun 7-6 (x)
  "A lambda expression that returns T if the input is T or NIL, NIL otherwise."
  ((lambda (n)
     (if (or (equal n t)
             (equal n nil))
         t nil)) x))

(defun 7-7 ()
  "Flips UP to DOWN and vice versa."
  (let ((directions '(UP DOWN UP UP)))
    (mapcar #'(lambda (dir)
                (if (equal dir 'UP) 'DOWN 'UP)) directions)))

(defun 7-8 (x k)
  "Return the number in the list x that is roughly equal to k."
  (find-if #'(lambda (n)
               (and (not (> n (+ k 10)))
                   (not (< n (- k 10))))) x))

(defun 7-9 (x)
  "Return the first element in the list that is a non nil list"
  (find-if #'(lambda (l)
               (and (listp l)
                    (not (null l)))) x))

;;; Mini keyboard exercise - transpose

;;; a
(setf note-table '((c 1)
                   (c-sharp 2)
                   (d 3)
                   (d-sharp 4)
                   (e 5)
                   (f 6)
                   (f-sharp 7)
                   (g 8)
                   (g-sharp 9)
                   (a 10)
                   (a-sharp 11)
                   (b 12)))
;;; b
(defun numbers (notes)
  "Returns a list of numbers that correspond to the list of notes."
  (mapcar #'(lambda (note)
              (cadr (assoc note note-table))) notes))

;;; c
(defun get-note (number)
  "Retrieve the note from the table given a number."
  (find-if #'(lambda (entry)
               (equal number entry)) note-table))

(defun notes (numbers)
  "Returns a list of notes that correspond to the list of numbers."
  (mapcar #'(lambda (number)
              (first (get-note number))) numbers))

;;; d
(defun inverse-check ()
  "Make sure the notes and numbers functions are mutual inverses."
  (let ((notes-list '(E D C D E E E))
        (numbers-list '(5 3 1 3 5 5 5)))
    (and (equal notes-list (notes (numbers notes-list)))
         (equal numbers-list (numbers (notes numbers-list))))))

;;; e
(defun raise (steps numbers)
  "Transposes notes (as numbers) up by the value of steps."
  (mapcar #'(lambda (num)
              (+ num steps)) numbers))

;;; f
(defun normalize (numbers)
  "Normalizes notes (as numbers) to be between 1 and 12."
  (mapcar #'(lambda (number)
              (cond ((< number 1) (+ number 12))
                    ((> number 12) (- number 12))
                    (t number)))
          numbers))

;;; g
(defun transpose (n song)
  "Transposes a list of notes by n steps."
  (notes (normalize (raise n (numbers song)))))

;;; End of mini keyboard exercise

(defun 7-11 ()
  "Pick out the elements that are between 1 and 5."
  (let ((mylist '(1 2 3 4 5 6)))
    (remove-if-not #'(lambda (n)
                       (and (< n 5)
                            (> n 1)))
                   mylist)))

(defun 7-12 ()
  "Count how many times 'the' appears in a sentence."
  (let ((sentence '(the quick brown fox jumped over the lazy dogs)))
    (length (remove-if-not #'(lambda (word)
                               (equal word 'the)) sentence))))

(defun 7-13 ()
  "Picks the lists of length two from a list of lists."
  (let ((mylist '(1 2 (3 4) (5 6) 7)))
    (remove-if-not #'(lambda (l)
                       (and (listp l)
                            (equal (length l) 2)))
                   mylist)))

(defun 7-14-union ()
  "Union written with remove-if or remove-if-not"
  (let ((list-a '(1 2 3 4))
        (list-b '(2 3 5 6 7)))
    ;; union = (1 2 3 4 5 6 7)
    (append list-a (remove-if #'(lambda (el)
                                  (member el list-a))
                              list-b))))

(defun 7-14-intersection ()
  "Intersection written with remove-if or remove-if-not"
  (let ((list-a '(1 2 3 4))
        (list-b '(2 3 5 6 7)))
    ;; intersection = (2 3)
    (remove-if-not #'(lambda (el)
                       (member el list-b))
                   list-a)))

;;; Mini keyboard exercise -- cards

;;; a
(defun rank (card)
  "Return the rank of a card, e.g. (2 CLUBS) => 2"
  (first card))

(defun suit (card)
  "Return the suit of a card, e.g. (2 CLUBS) => CLUBS"
  (second card))

;;; b
(setf my-hand '((3 hearts)
                (5 clubs)
                (2 diamonds)
                (4 diamonds)
                (ace spades)))

(defun count-suit (suit hand)
  "Counts the numbers of cards in HAND that are of type SUIT"
  (length (remove-if-not #'(lambda (card)
                             (equal suit (suit card)))
                         hand)))

;;; c
(setf colors '((clubs black)
               (diamonds red)
               (hearts red)
               (spades black)))

(defun color-of (card)
  "Returns the color (red/black) of CARD"
  (let ((card-suit (suit card)))
    (second (assoc card-suit colors))))

;;; d
(defun first-red (hand)
  "Returns the first card in HAND that is red."
  (find-if #'(lambda (card)
               (equal 'red (color-of card)))
           hand))

;;; e
(defun black-cards (hand)
  "Returns all black cards in HAND."
  (remove-if-not #'(lambda (card)
                     (equal 'black (color-of card)))
                 hand))

;;; f
(defun what-ranks (suit hand)
  "Returns the ranks of all cards in HAND belonging to SUIT
   e.g. (what-ranks 'diamonds my-hand) => (2 4)"
  (flet ((extract-suit (suit)
           (remove-if-not #'(lambda (card)
                              (equal (suit card) suit))
                          hand)))
    (mapcar #'rank (extract-suit suit))))

;;; g
(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
  "Returns true if card1 has a higher rank than card2"
  (member (rank card1) (member (rank card2) all-ranks)))

;;; h
(defun high-card (hand)
  "Returns the highest rank card in HAND"
  ;; uses find-if to find the highest rank present in the hand.
  (let ((highest-rank-in-hand
          (find-if
           #'(lambda (rank)
               (assoc rank hand))
           all-ranks :from-end t)))
    ;; after finding the highest rank, extract the card.
    (assoc highest-rank-in-hand hand)))

(defun high-card-reduce (hand)
  "Returns the highest ranked card in HAND"
  ;; repeatedly choose the highest card of each pair.
  (reduce
   #'(lambda (card1 card2)
       (if (higher-rank-p card1 card2)
           card1
           card2))
   hand))

;;; End of mini keyboard exercise

(defun 7-17 ()
  "Returns the total length of all sublists in a list of lists."
  (let ((my-list '((A B C) (D E) (F) (G H))))
    (reduce #'+ (mapcar #'length my-list))))

(defun 7-19 (my-list)
  "Returns T if every element of a list is odd."
  (every #'oddp my-list))

(defun 7-20 (my-list)
  "Returns T if every element of a list is not odd."
  (every #'evenp my-list))

(defun 7-21 (my-list)
  "Returns T if not every element of a list is odd."
  (find-if #'evenp my-list))

(defun 7-22 (my-list)
  "Returns T if it is not the case that a list has no odd elements."
  (find-if #'oddp my-list))

(defun 7-26 (predicate my-list)
  "Find-if implemented using remove-if-not"
  (first (remove-if-not predicate my-list)))

(defun 7-27 (predicate my-list)
  "Every implemented using remove-if"
  (null (remove-if predicate my-list)))

;;; Keyboard exercise -- block world!

(setf database
      '((b1 shape brick)
        (b1 color green)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)

        (b2 shape brick)
        (b2 color red)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)

        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of-b2)

        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)

        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)

        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))

;;; a
(defun match-element (sym1 sym2)
  "Returns T if sym1 and sym2 are the same symbol or if sym2 is ?"
  (or (eq sym1 sym2)
      (equal sym2 '?)))

;;; b
(defun match-triple (assertion pattern)
  "Returns T if the assertion matches the pattern.
   e.g. (b2 color red) matches (b2 color ?)"
  (every #'match-element assertion pattern))

;;; c
(defun fetch (pattern)
  "Returns all assertions in the database that match PATTERN"
  (remove-if-not
   #'(lambda (assertion)
       (match-triple assertion pattern)) database))

;;; d
(defun questions ()
  "Using fetch and patterns to answer some questions about block world"
  (let* ((q1 '(what shape is block b4?))
         (q2 '(which blocks are bricks?))
         (q3 '(what relation is block b2 to b3?))
         (q4 '(list the color of every block))
         (q5 '(what facts are known about block 4?))

         (answers `(
                    ,q1 ,(cddar (fetch '(b4 shape ?)))
                    ,q2 ,(mapcar #'first (fetch '(? shape brick)))
                    ,q3 ,(first (fetch '(b2 ? b3)))
                    ,q4 ,(mapcar
                          #'(lambda (assertion)
                              (list (first assertion) (third assertion)))
                          (fetch '(? color ?)))
                    ,q5 ,(mapcar
                          #'(lambda (assertion)
                              (rest assertion))
                          (fetch '(b4 ? ?))))))
    answers))

;;; e
(defun color-of (block-name)
  "Given a block name, return the pattern for its color."
  (list block-name 'color '?))

;;; f
(defun supporters (block-name)
  "Given a block name, return a list of supporters."
  (let ((pattern (list '? 'supports block-name)))
    (mapcar #'first (fetch pattern))))

;;; g
(defun supp-cube (block-name)
  "Returns true if the given block is supported by a cube."
  (let ((pattern '(? shape cube)))
    (find-if
     #'(lambda (supporter)
         (assoc supporter (fetch pattern)))
     (supporters block-name))))

;;; h
(defun desc1 (block-name)
  "Helper function for description - gets all assertions"
  (let ((pattern (list block-name '? '?)))
    (fetch pattern)))

;;; i
(defun desc2 (block-name)
  "Helper function for description - strips block name from assertions"
  (mapcar #'rest (desc1 block-name)))

;;; j
(defun description (block-name)
  "Describes a block"
  (reduce #'append (desc2 block-name)))

;;; k
(defun descriptions-of-blocks ()
  "Descriptions of blocks b1 and b4."
  `(b1 ,(description 'b1)
       b4 ,(description 'b4)))

;;; l
(defun add-materials ()
  "Add material information to the database"
  (setf database (append database
                         '((b1 material wood)
                           (b2 material plastic)))))

;;; End of keyboard exercise

(defun 7-30 ()
  "Given a table of english and french words and a list of spanish words
   output a trilingual dictionary."
  (let ((en-fr-dict '((one un)
                      (two deux)
                      (three trois)
                      (four quatre)
                      (five cinq)))
        (es-words '(uno dos tres quatro cinco)))
    (mapcar
     #'(lambda (x y)
         (append x (list y)))
     en-fr-dict es-words)))
