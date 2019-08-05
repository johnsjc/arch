(defun make-even (n)
  (if (evenp n)
      n
      (+ 1 n)))

(defun further (n)
  (if (< n 0)
      (- n 1)
      (+ n 1)))

(defun my-not (value)
  (if value
      nil
      t))

(defun ordered (x y)
  (if (< x y)
      (list x y)
      (list y x)))

(defun my-abs (n)
  (if (< n 0)
      (- n)
      n))

(defun emphasize3 (phrase)
  (cond ((equal (first phrase) 'good)
         (cons 'great (rest phrase)))
        ((equal (first phrase) 'bad)
         (cons 'awful (rest phrase)))
        (t
         (cons 'very phrase))))

(defun make-odd (x)
  (cond ((not (oddp x))
         (+ x 1))
        (t x)))

(defun constrain-cond (x min max)
  (cond ((< x min)
         min)
        ((> x max)
         max)
        (t
         x)))

(defun constrain-if (x min max)
  (if (< x min)
      min
      (if (> x max)
          max
          x)))

(defun firstzero (nums)
  (cond ((zerop (first nums))
         'first)
        ((zerop (second nums))
         'second)
        ((zerop (third nums))
         'third)
        (t
         'none)))

(defun cycle (n)
  (cond ((equal 99 n)
         1)
        (t
         (+ n 1))))

(defun how-compute (op1 op2 result)
  (cond ((equal result (+ op1 op2))
         'sum-of)
        ((equal result (* op1 op2))
         'product-of)
        (t
         'beats-me)
))
