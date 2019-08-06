(defun 6-5 ()
  "Set the global variable line to ROSES ARE RED"
  (setf line '(roses are red)))

(defun 6-6-last (l)
  "Get the last element of a list using last."
  (first (last l)))

(defun 6-6-reverse (l)
  "Get the last element of a list using reverse."
  (first (reverse l)))

(defun 6-6-nth (l)
  "Get the last element of a list using nth and length."
  (let ((index (- (length l) 1)))
    (nth index l)))

(defun 6-7-reverse (l)
  "Get the penultimate element of a list using reverse."
  (second (reverse l)))

(defun 6-7-nth (l)
  "Get the penultimate element of a list using nth."
  (let ((len (length l)))
    (if (< len 2)
        nil
        (nth (- len 2) l))))

(defun 6-8 (l)
  "Get all elements of a list except the last one."
  (reverse (cdr (reverse l))))

(defun 6-10 (l)
  "Returns whether the list is a palindrome."
  (equal (reverse l) l))

(defun 6-11 (l)
  "Makes a palindrome from the input."
  (append l (reverse l)))

(defun 6-15-intersection (l)
  "Returns whether the list contains an article."
  (intersection l '(the an a)))

(defun 6-15-member (l)
  "Returns whether the list contains an article."
  (or (member 'the l)
      (member 'an l)
      (member 'a l)))

(defun 6-15-member-and (l)
  "Returns whether the list contains an article."
  (not (and (not (member 'the l))
            (not (member 'an l))
            (not (member 'a l)))))

(defun 6-18 (l)
  "Adds the vowels to the given list."
  (union l '(a e i o u)))

(defun 6-21 (x y)
  "Returns whether x is a subset of y."
  (equal '() (set-difference x y)))

(defun 6-24 (x y)
  "Returns whether two sets are equal."
  (and (equal '() (set-difference x y))
       (equal '() (set-difference y x))))

(defun 6-25 (x y)
  "Returns whether x is a proper subset of y."
  (and (equal '() (set-difference x y))
       (not (equal (length x) (length y)))))


;;; Mini keyboard exercise 6.26 -- compare

;;; a
(defun right-side (comparison)
  "Returns the right side of a comparison such as '(red ball -vs- blue ball)"
  (nthcdr 1 (member '-vs- comparison)))

;;; b
(defun left-side (comparison)
  "Returns the left side of a comparison."
  (reverse (right-side (reverse comparison))))

;;; c
(defun count-common (comparison)
  "Counts the number of common features in a comparison."
  (length (intersection (left-side comparison) (right-side comparison))))

;;; d
(defun compare (comparison)
  "Returns the number of common features between two descriptions."
  (list (count-common comparison) 'common 'features))

;;; end of mini keyboard exercise

(defun 6-30 ()
  "Create an a-list of books and their authors."
  '((war-and-peace leo-tolstoy)
   (don-quixote miguel-de-cervantes)
   (to-kill-a-mockingbird harper-lee)
   (1984 george-orwell)
   (the-hobbit jrr-tolkien)))

(defun 6-31 (book)
  "Return the author who wrote the given book."
  (cdr (assoc book (6-30))))

(defun 6-33 (author)
  "Return the book written by the given author. Atomic required for rassoc."
  (flet ((books ()
           '((war-and-peace . leo-tolstoy)
             (don-quixote . miguel-de-cervantes)
             (to-kill-a-mockingbird . harper-lee)
             (1984 . george-orwell)
             (the-hobbit . jrr-tolkien))))
    (first (rassoc author (books)))))

(defun 6-34 (state)
  "Return the cities that are in a given state."
  (flet ((cities ()
           '((pennsylvania pittsburgh johnstown)
             (new-jersey newark princeton trenton)
             (ohio columbus))))
    (rest (assoc state (cities)))))

;;; Mini keyboard exercise 6.35
;;; Nerdus Americanus

;;; a
;; represents the cyclical pattern of states as (state, successor) pairs.
(setf nerd-states
      '((sleeping eating)
        (eating waiting-for-a-computer)
        (waiting-for-a-computer programming)
        (programming debugging)
        (debugging sleeping)))

;;; b
(defun nerdus (state)
  (second (assoc state nerd-states)))

;;; d
(defun sleepless-nerd (state)
  (let ((next-state (nerdus state)))
    (if (equal 'sleeping next-state)
        'eating
        next-state)))

;;; e
(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))


;;; end of keyboard exercise


(defun 6-36 (l)
  "Swap the first and last elements of a list."
  (let* ((a (reverse (rest l))) ; YOU CANT BUY LOVE => LOVE BUY CANT
         (b (reverse (rest a)))) ; CANT BUY
    (append (cons (first a) b) (list (first l))))) ; LOVE CANT BUY YOU

(defun 6-37-left (l)
  "Rotates a list to the left, e.g. (A B C D) => (B C D A)"
  (append (rest l) (list (first l))))

(defun 6-37-right (l)
  "Rotates a list to the right, e.g. (A B C D) => (D A B C)"
  (let ((but-last (reverse (rest (reverse l)))))
    (append (last l) but-last)))

(defun 6-40 ()
  "Define a table so (member list) and (assoc table) are equivalent."
  (let ((lst '(a b c d))
        (tbl '((a b c d)
             (b c d)
             (c d)
             (d))))
    (equal (member 'b lst) (assoc 'b tbl))))

;;; Keyboard exercise -- Robbie

;; Defines a room followed by (direction, destination) pairs of possible exits.
(setf rooms
      '((living-room
         (north front-stairs)
         (south dining-room)
         (east kitchen))
        (upstairs-bedroom
         (west library)
         (south front-stairs))
        (dining-room
         (north living-room)
         (east pantry)
         (west downstairs-bedroom))
        (kitchen
         (west living-room)
         (south pantry))
        (pantry
         (north kitchen)
         (west dining-room))
        (downstairs-bedroom
         (north back-stairs)
         (east dining-room))
        (back-stairs
         (south downstairs-bedroom)
         (north library))
        (front-stairs
         (north upstairs-bedroom)
         (south living-room))
        (library
         (east upstairs-bedroom)
         (south back-stairs))))

;;; a
(defun choices (room)
  "Returns the possible choices Robbie can make to exit a room."
  (rest (assoc room rooms)))

;;; b
(defun look (direction room)
  "Given a direction and a room, return the destination if the direction is taken."
  (second (assoc direction (choices room))))

;;; c
(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the global variable LOC"
  (setf *loc* place))

;;; d
(defun how-many-choices ()
  "Returns how many choices Robbie has."
  (length (choices *loc*)))

;;; e
(defun upstairsp (room)
  "Returns whether the room is upstairs."
  (member room '(library upstairs-bedroom)))

(defun onstairsp (room)
  "Returns whether the room is on stairs."
  (member room '(front-stairs back-stairs)))

;;; f
(defun where ()
  "Tells where Robbie is."
  (let ((msg '(robbie is)))
    (append msg
            (cond ((upstairsp *loc*) '(upstairs in the))
                  ((onstairsp *loc*) '(on the))
                  (t '(downstairs in the)))
            (list *loc*))))

;;; g
(defun move (direction)
  "Moves Robbie in the given direction if possible."
  (let ((new-location (look direction *loc*)))
    (cond ((null new-location)
           '(ouch! robbie hit a wall))
          (t (set-robbie-location new-location)
             (where)))))

;;; End of keyboard exercise

(defun 6-42 (sentence)
  "Changes a sentence in first person to use the royal we."
  (subst 'we 'i sentence))

(defun madlib ()
  "Madlib example using sublis"
  (let ((template '(NAME IS TOO COOL FOR NOUN CLASS. INSTEAD HE WILL BE ATTENDING EVENT))
        (substitutions '((name . Jc)
                         (noun . computer)
                         (event . blackhat))))
    (sublis substitutions template)))

