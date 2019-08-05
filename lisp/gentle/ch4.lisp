(defun 4-1 (n)
  (if (evenp n)
      n
      (+ 1 n)))

(defun 4-2 (n)
  (if (< n 0)
      (- n 1)
      (+ n 1)))

(defun 4-3 (value)
  (if value
      nil
      t))

(defun 4-4 (x y)
  (if (< x y)
      (list x y)
      (list y x)))

(defun 4-6 (n)
  (if (< n 0)
      (- n)
      n))

(defun 4-8 (phrase)
  (cond ((equal (first phrase) 'good)
         (cons 'great (rest phrase)))
        ((equal (first phrase) 'bad)
         (cons 'awful (rest phrase)))
        (t
         (cons 'very phrase))))

(defun 4-9 (x)
  (cond ((not (oddp x))
         (+ x 1))
        (t x)))

(defun 4-10-cond (x min max)
  (cond ((< x min)
         min)
        ((> x max)
         max)
        (t
         x)))

(defun 4-10-if (x min max)
  (if (< x min)
      min
      (if (> x max)
          max
          x)))

(defun 4-11 (nums)
  (cond ((zerop (first nums))
         'first)
        ((zerop (second nums))
         'second)
        ((zerop (third nums))
         'third)
        (t
         'none)))

(defun 4-12 (n)
  (cond ((equal 99 n)
         1)
        (t
         (+ n 1))))

(defun 4-13 (op1 op2 result)
  (cond ((equal result (+ op1 op2))
         'sum-of)
        ((equal result (* op1 op2))
         'product-of)
        (t
         'beats-me)
))

(defun 4-15 (x y)
  (or (equal x y)
      (> x y)))

(defun 4-16 (n)
  (cond ((and (oddp n) (> n 0))
         (* n n))
        ((and (oddp n) (< n 0))
         (* n 2))
        (t (/ n 2))))

(defun 4-17 (x y)
  (or
   (and
    (or (equal x 'boy) (equal x 'girl))
    (equal y 'child))
   (and
    (or (equal x 'man) (equal x 'woman))
    (equal y 'adult))
   ))

(defun 4-18 (player1 player2)
  (cond ((equal player1 player2)
         'tie)
        ((or
          (and
           (equal player1 'rock)
           (equal player2 'scissors))
          (and
           (equal player1 'paper)
           (equal player2 'rock))
          (and
           (equal player1 'scissors)
           (equal player2 'paper)))
         'first-wins)
        (t
         'second-wins)))

(defun 4-19-cond (x y z w)
  (cond ((not x)
         nil)
        ((not y)
         nil)
        ((not z)
         nil)
        (t
         w)))

(defun 4-19-if (x y z w)
  (if x
      (if y
          (if z
              w))))

(defun 4-20-if (x y)
  (if (equal x y)
      'equal
      (if (< x y)
          'first-smaller
          'first-bigger)))

(defun 4-20-and-or (x y)
  (or
   (and (equal x y)
        'equal)
   (and (< x y)
        'first-smaller)
   'first-bigger))

(defun 4-21-if (x y)
  (if (> x y)
      t
      (if (zerop x)
          t
          (zerop y))))

(defun 4-21-cond (x y)
  (cond ((> x y)
         t)
        ((zerop x)
         t)
        (t (zerop y))))

(defun 4-22-cond (temp scale)
  (cond ((equal scale 'fahrenheit)
         (> temp 212))
        ((equal scale 'celcius)
         (> temp 100))))

(defun 4-22-if (temp scale)
  (if (equal scale 'fahrenheit)
      (> temp 212)
      (if (equal scale 'celcius)
          (> temp 100))))

(defun 4-22-and-or (temp scale)
  (or
   (and
    (equal scale 'fahrenheit)
    (> temp 212))
   (and
    (equal scale 'celcius)
    (> temp 100))))
