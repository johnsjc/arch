;;; 8 -- Recursion

(defun 8-2 (my-list)
  "Return T if any elements in MY-LIST are odd.
   Write using IF instead of CONS."
  (if (null my-list)
      nil
      (if (oddp (first my-list))
          t
          (8-2 (rest my-list)))))

(defun 8-4 (n)
  "Returns a list containing N 'HA's"
  (cond ((< n 1) nil)
        (t (cons 'ha (8-4 (- n 1))))))

(defun 8-5 (my-list)
  "Adds all of the elements in MY-LIST together."
  (cond ((equal my-list '()) 0)
        (t (+ (first my-list) (8-5 (rest my-list))))))

(defun 8-6 (my-list)
  "Returns T if all elements in MY-LIST are odd."
  (cond ((null my-list) t)
        ((evenp (first my-list)) nil)
        (t (8-6 (rest my-list)))))

(defun 8-7 (x my-list)
  "Recursive definition of the built in member function."
  (cond ((equal my-list '()) nil)
        ((equal (first my-list) x) my-list)
        (t (8-7 x (rest my-list)))))

(defun 8-8 (item alist)
  "Recursive definition of the built in assoc function."
  (cond ((equal alist nil) nil)
        ((equal (caar alist) item) (car alist))
        (t (8-8 item (rest alist)))))

(defun 8-9 (n my-list)
  "Recursive definition of the built in nth function."
  (cond ((equal my-list '()) nil)
        ((zerop n) (first my-list))
        (t (8-9 (- n 1) (rest my-list)))))

(defun 8-10-add1 (n) (+ n 1))
(defun 8-10-sub1 (n) (- n 1))

(defun 8-10 (x y)
  "Recursive definition for adding positive numbers."
  (cond ((zerop y) x)
        (t (8-10 (8-10-add1 x) (8-10-sub1 y)))))

(defun 8-11 (n)
  "Returns the NTH fibonacci number."
  (cond ((or (zerop n)
             (equal n 1)) 1)
        (t (+ (8-11 (- n 2))
              (8-11 (- n 1))))))

(defun 8-14 ()
  "Shortest infinite recursion function"
  (8-14))

;; Recursion template 1 -- Double-Test Tail Recursion
;; Used when there is a possibility the function might not find what its looking for.
;; (defun func (x)
;;   (cond (end-test-1 end-value-1)
;;         (end-test-2 end-value-2)
;;         (t (func reduced-x))))

(defun 8-17 (my-list)
  "Returns the first odd number in MY-LIST or NIL if none exists.
   Double-Test Tail Recursion form"
  (cond ((equal my-list '()) nil)
        ((oddp (first my-list)) (first my-list))
        (t (8-17 (rest my-list)))))

;; Recursion template 2 -- Single-Test Tail Recursion
;; Used when we know the function will eventually find what its looking for.
;; (defun func (x)
;;   (cond (end-test end-value)
;;         (t (func reduced-x))))

(defun 8-18 (my-list)
  "Returns the final element of MY-LIST.
   Single-Test Tail Recursion form"
  (cond ((atom (cdr my-list))
         (car my-list))
        (t (8-18 (rest my-list)))))

;; Recursion template 3 -- Single-Test Augmenting Recursion
;; Not tail recursive!
;; (defun func (x)
;;   (cond (end-test end-value)
;;         (t (aug-fun aug-val
;;           (func reduced-x)))))

(defun 8-21 (n)
  "Adds the numbers N, N-1, N-2 .. 0
   Single-Test Augmenting Recursion form"
  (cond ((zerop n) 0)
        ((+ n (8-21 (- n 1))))))

(defun 8-22 (my-list)
  "Returns T if all elements of a list are the same."
  (cond ((< (length my-list) 2) t)
        ((not (equal (first my-list) (second my-list))) nil)
        (t (8-22 (rest my-list)))))


;; List-consing recursion (special case of template 3)
;; (defun func (x)
;;   (cond (end-test end-value)
;;         (t (cons x (func reduced-x)))))

(defun 8-24 (n)
  "Counts down from N to 1.
   List consing recursion"
  (cond ((< n 1) nil)
        (t (cons n (8-24 (- n 1))))))

(defun 8-25 (n)
  "Applicative version of factorial using the countdown function (8-24)"
  (reduce #'* (8-24 n)))

(defun 8-26-a (n)
  "Countdown from N to 0 instead of N to 1
   Changes the end-test"
  (cond ((< n 0) nil)
        (t (cons n (8-26 (- n 1))))))

(defun 8-26-b (n)
  "Countdown from N to 0 instead of N to 1
   Appends 0"
  (let ((countdown (8-24 n)))
    (cond ((equal n 0) '(0))
          ((null countdown) nil)
          (t (append countdown (list 0))))))

(defun 8-27 (my-list)
  "Returns a list containing the squares of each value in MY-LIST"
  (cond ((null my-list) nil)
        (t (let ((n (first my-list)))
             (cons (* n n) (8-27 (rest my-list)))))))

;; Simultaneous recursion -- (special case of templates 1,2)
;; (defun func (n x)
;;   (cond (end-test end-value)
;;         .. optional additional end-test
;;         (t (func reduced-n reduced-x))))

(defun 8-28 (n my-list)
  "Recursive definition of nth
   Simultaneous recursion on multiple variables"
  (cond ((null my-list) nil)
        ((zerop n) (first my-list))
        (t (8-28 (- n 1) (rest my-list)))))

(defun 8-29 (x my-list)
  "Recursive definition of member"
  (cond ((null my-list) nil)
        ((equal (first my-list) x) my-list)
        (t (8-29 x (rest my-list)))))

(defun 8-30 (item alist)
  "Recursive definition of assoc"
  (cond ((null alist) nil)
        ((equal (caar alist) item) (car alist))
        (t (8-30 item (rest alist)))))

(defun 8-31 (list-a list-b)
  "Determines which of two lists is longer"
  (cond ((and (null list-a)
              (null list-b))
         'same-length)
        ((null list-a)
         'second-is-longer)
        ((null list-b)
         'first-is-longer)
        (t (8-31 (rest list-a) (rest list-b)))))

;; Conditional augmentation (special case of template 3)
;; (defun func (x)
;;   (cond (end-test end-value)
;;         (aug-test (aug-func aug-value (func reduced-x)))
;;         (t (func reduced-x))))

(defun 8-32 (my-list)
  "Sum only the numeric elements of MY-LIST"
  (cond ((null my-list) 0)
        ((numberp (first my-list))
         (+ (first my-list) (8-32 (rest my-list))))
        (t (8-32 (rest my-list)))))

(defun 8-33 (e my-list)
  "Recursive definition of remove"
  (cond ((null my-list) nil)
        ((not (equal e (first my-list)))
         (cons (first my-list) (8-33 e (rest my-list))))
        (t (8-33 e (rest my-list)))))

(defun 8-34 (list-a list-b)
  "Recursive definition of intersection"
  (cond ((null list-a) nil)
        ((member (first list-a) list-b)
         (cons (first list-a) (8-34 (rest list-a) list-b)))
        (t (8-34 (rest list-a) list-b))))

(defun 8-35 (list-a list-b)
  "Recursive definition of set-difference"
  (cond ((null list-a) nil)
        ((not (member (first list-a) list-b))
         (cons (first list-a) (8-35 (rest list-a) list-b)))
        (t (8-35 (rest list-a) list-b))))

(defun 8-36-a (my-list)
  "Counts the odd elements in MY-LIST"
  (cond ((null my-list) 0)
        ((oddp (first my-list))
         (+ 1 (8-36-a (rest my-list))))
        (t (8-36-a (rest my-list)))))

(defun 8-36-b (my-list)
  "Counts the odd elements in MY-LIST"
  (cond ((null my-list) 0)
        (t (+ (if (oddp (first my-list))
                  1
                  0)
              (8-36-b (rest my-list))))))

;; Multiple recursion (like fibonacci)
;; (defun func (n)
;;   (cond (end-test-1 end-value-1)
;;         ... additional end tests
;;         (t (combiner (func first-reduced-n)
;;                      (func second-reduced-n)))))

(defun 8-37-combine (x y)
  (+ x y))

(defun 8-37 (n)
  "Compute the NTH fibonacci number."
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (8-37-combine (8-37 (- n 2))
                         (8-37 (- n 1))))))

;; Trees and CAR/CDR recursion
;; Used to search trees (OR combiner) or build trees (cons combiner)
;; (defun func (x)
;;   (cond (end-test-1 end-value-1)
;;         ... additional end tests
;;         (t (combiner (func (car x))
;;                      (func (cdr x))

(defun 8-39 (tree)
  "Counts the number of atoms in a tree"
  (cond ((atom tree) 1)
        ((+ (8-39 (car tree))
            (8-39 (cdr tree))))))

(defun 8-40 (tree)
  "Counts the number of cons cells in a tree"
  (cond ((atom tree) 0)
        ((+ 1 (8-40 (car tree))
            (8-40 (cdr tree))))))

(defun 8-41 (tree)
  "Sums the numeric values in a tree"
  (cond ((numberp tree) tree)
        ((atom tree) 0)
        (t (+ (8-41 (car tree))
              (8-41 (cdr tree))))))

(defun 8-42 (new old tree)
  "Recursive implementation of subst"
  (cond ((equal tree old) new)
        ((atom tree) tree)
        (t (cons (8-42 new old (car tree))
                 (8-42 new old (cdr tree))))))

(defun 8-43 (tree)
  "Flattens a tree into a list"
  (cond ((equal tree nil) nil)
        ((atom tree) (list tree))
        (t (append (8-43 (car tree))
                   (8-43 (cdr tree))))))

(defun 8-44 (tree)
  "Returns the maximum depth of a tree"
  (cond ((atom tree) 0)
        (t (+ 1 (max
                 (8-44 (car tree))
                 (8-44 (cdr tree)))))))

(defun 8-45 (my-list)
  "Returns the maximum depth of nested parentheses in MY-LIST"
  (cond ((atom my-list) 0)
        (t (max (+ 1 (8-45 (car my-list)))
                (8-45 (cdr my-list))))))

(defun 8-46 (n)
  "Counts up from 1 to N"
  (cond ((zerop n) nil)
        (t (append (8-46 (- n 1)) (list n)))))

(defun 8-47 (n)
  "Makes a list of size n"
  (if (zerop n)
      nil
      (append (8-47 (- n 1)) '(X))))

(defun 8-48 (x n)
  "Buries X under N levels of parentheses"
  (cond ((zerop n) x)
        (t (list (8-48 x (- n 1))))))

(defun 8-49 (list-a list-b)
  "Pairs elements together from both lists."
  (cond ((null list-a) nil)
        (t (cons (list (first list-a) (first list-b))
                 (8-49 (rest list-a) (rest list-b))))))

(defun 8-50 (my-list)
  "Returns successive sublists of MY-LIST"
  (cond ((null my-list) nil)
        (t (cons my-list (8-50 (rest my-list))))))

(defun 8-51 (my-list)
  "Recursive implementation of reverse - sets up call"
  (8-51-dowork my-list nil))

(defun 8-51-dowork (x y)
  "Implementation of reverse"
  (cond ((null x) y) ; y is our constructed list, return after processing x
        (t (8-51-dowork
            (rest x)
            (cons (first x) y))))) ; build up y one element at a times

(defun 8-52 (list-a list-b)
  "Recursive union appends list-a to the result of dowork
   which are the elements in list-b that don't appear in list-a"
  (append list-a (8-52-dowork list-a list-b)))

(defun 8-52-dowork (list-a list-b)
  "Recursive implementation of union"
  (cond ((null list-b) nil) ; done processing list-b
        ((member (first list-b) list-a)
         (8-52-dowork list-a (rest list-b))) ; element exists in list-a, skip
        (t (cons (first list-b) ; else add element to constructed list and recurse
                 (8-52-dowork list-a (rest list-b))))))

(defun 8-53 (my-list)
  "Returns the largest even number in MY-LIST"
  (cond ((null my-list) 0)
        ((oddp (first my-list))
         (8-53 (rest my-list)))
        (t (max (first my-list)
                (8-53 (rest my-list))))))

(defun 8-54 (n)
  "Raises N to the Nth power (recursively)"
  (8-54-dowork n n))

(defun 8-54-dowork (cnt base)
  (cond ((zerop cnt) 1)
        (t (* (8-54-dowork (- cnt 1) base) base))))

(defun 8-55 (my-list)
  "Returns every other element in MY-LIST"
  (cond ((null my-list) nil)
        (t (cons (first my-list)
                 ( 8-55 (rest (rest my-list)))))))

(defun 8-56 (my-list)
  "Returns the first half of a list."
  ;; recursively constructs a list based on a counter c
  (labels ((dowork (c my-list)
             ;; (minusp c) would not work since it returns nil for c=0
             ;; (not (plusp c) for c=0 returns T
             (cond ((not (plusp c)) nil)
                   (t (cons (first my-list)
                            (dowork (- c 1) (rest my-list)))))))
    ;; sets up the recursive call
    (dowork (/ (length my-list) 2)
            my-list)))

(defun 8-57 (list-a list-b)
  "Merge two sorted lists together"
  (cond ((null list-a) list-b) ; if list-a is empty, return list-b
        ((null list-b) list-a) ; if list-b is empty, return list-a
        ((< (first list-a) (first list-b)) ; if element in a is smaller, construct using it
         (cons (first list-a)
               (8-57 (rest list-a) list-b))) ; else construct using element in b
        (t (cons (first list-b)
                 (8-57 list-a (rest list-b))))))


;;; Keyboard exercise -- family tree

(setf family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)

        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)

        (george frank linda)
        (hillary frank linda)

        (andre nil nil)

        (tamara bruce suzanne)
        (vincent bruce suzanne)

        (wanda nil nil)

        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)

        (nigel andre hillary)

        (frederick nil tamara)

        (zelda vincent wanda)
        (joshua ivan wanda)

        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)

        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

;;; a
(defun father (person)
  "Returns the father of PERSON"
  (second (assoc person family)))

(defun mother (person)
  "Returns the mother of PERSON"
  (third (assoc person family)))

(defun parents (person)
  "Returns the parents of PERSON"
  ;; the and statements return NIL if a parent does not exist or (parent)
  ;; (union nil nil) => nil
  (union
   (and (father person) (list (father person)))
   (and (mother person) (list (mother person)))))

(defun children (person)
  "Returns the children of PERSON"
  (and
   person
   (mapcar #'first
           (remove-if-not
            #'(lambda (entry)
                (member person (rest entry)))
            family))))

;;; b
(defun siblings (person)
  "Returns a person's siblings (including half-siblings)"
  (and
   person ; return nil if person is nil
   ;; all of the children of my parents except for me are siblings.
   (set-difference
    (union (children (father person))
           (children (mother person)))
    (list person))))

;;; c
(defun mapunion (fn my-list)
  "Applies fn on my-list and then computes the union of the result."
  (and
   my-list
   (reduce #'union (mapcar fn my-list))))

;;; d
(defun grandparents (person)
  "Returns a person's grandparents"
  (and
   person ; return nil if person is nil
   ;; my grandparents are the parents of my parents.
   (mapunion #'parents (parents person))))

;;; e
(defun cousins (person)
  "Returns a person's cousins"
  (and
   person
   ;; my cousins are the children of my parent's siblings.
   (mapunion #'children
             (mapunion #'siblings (parents person)))))

;;; f
(defun descended-from-p (person ancestor)
  "Returns T if the person is descended from ancestor."
  ;; I am descended from someone if they are my parents
  ;; or the parents of my parents
  (cond ((null person) nil)
        ((member ancestor (parents person)) t)
        (t 
         (or (descended-from-p (father person) ancestor)
             (descended-from-p (mother person) ancestor)))))

;;; g
(defun ancestors (person)
  "Returns the ancestors of PERSON"
  (cond ((null person) nil)
        (t
         (union (parents person)
                (mapunion #'ancestors (parents person))))))

;;; h
(defun generation-gap (person ancestor)
  "Returns the number of generations between PERSON and ANCESTOR."
  (labels ((dowork (person ancestor n)
             (cond ((null person) nil)
                   ((equal person ancestor) n)
                   (t
                    (or (dowork (father person) ancestor (+ n 1))
                        (dowork (mother person) ancestor (+ n 1)))))))
    (dowork person ancestor 0)))

;;; i

(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t
         (append (flatten (car tree))
                 (flatten (cdr tree))))))

(defun questions ()
  
  (let* ((q1 '(is robert descended from deirdre?))
         (q2 '(who are yvettes ancestors?))
         (q3 '(what is the generation gap between olivia and frank?))
         (q4 '(who are peters cousins?))
         (q5 '(who are olivias grandparents?))

         (answers
           (mapcar
            #'(lambda (qa-pair)
                (flatten (list (first qa-pair) '-- (second qa-pair))))
            `(
              (,q1 ,(if (descended-from-p 'robert 'deidre) 'yes 'no))
              (,q2 ,(ancestors 'yvette))
              (,q3 ,(generation-gap 'olivia 'frank))
              (,q4 ,(cousins 'peter))
              (,q5 ,(grandparents 'olivia))
              ))))
    answers))
