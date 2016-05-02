;; 1.1
(defparameter *name-suffix*
  '(MD Jr.)
  "A list of suffixes that can apper at the last of a name")

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (if (member (first (last name)) *name-suffix*)
      (last-name (reverse (rest (reverse name))))
      (first (last name))))

;; 1.2
(defun power (x n)
  "Power raises x to the nth power. n must be an integer >= 0. This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))

;; 1.3
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression, counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))

;; 1.4
(defun count-anywhere (exp1 exp2)
  "Count the times exp1 appears anywhere within exp2."
  (cond ((atom exp2) (if (equal exp1 exp2) 1 0))
        ((equal exp1 exp2) 1)
        (t (+ (count-anywhere exp1 (first exp2))
              (count-anywhere exp1 (rest exp2))))))

;; 1.5
(defun dot-product (lis1 lis2)
  "Return the dot product of lis1 and lis2."
  (if (or (null lis1) (null lis2))
      0
      (+ (* (first lis1) (first lis2))
         (dot-product (rest lis1) (rest lis2)))))
