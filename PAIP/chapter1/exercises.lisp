#!/opt/local/bin/sbcl --script


(load "../utils.lisp")



;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        1.1        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setf names '((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper)
              (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier)
              (Miss Scarlet) (Rex Morgan MD) (Morton Downey Jr)))

(defparameter *titles* '(Ms Mr Mrs Miss Sir Madam Dr Admiral Major General))

(defparameter *post-fixes* '(MD PhD Jr Sr I II III IV V VI))

(defun last-name-naive (a-list)
  (car (last a-list)))

(defun last-name-2 (a-list)
  (let ((reved (reverse a-list)))
    (labels ((inner (another)
                (if (member (car another) *post-fixes*)
                    (inner (cdr another))
                    (car another))))
      (inner reved))))

(defun last-name-3 (a-list)
  (car (last (remove-if (lambda (x) (member x *post-fixes*)) a-list))))

(mapcar #' last-name-naive names)
(mapcar #' last-name-2 names)
(mapcar #' last-name-3 names)

(fmap-time (list #'last-name-2 #'last-name-3) 900 names)


;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        1.2        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

; good
(defun power-dotimes (x n)
  (let ((acc 1))
      (dotimes (i n)
        (setf acc (* acc x)))
      acc))

(defun power-dotimes2 (x n)
  (let ((acc 1))
      (dotimes (i n acc)
        (setf acc (* acc x)))))

(defun power-dolist (x n)
  (let ((acc 1))
    (dolist (el (make-list n :initial-element x))
      (setf acc (* acc el)))
    acc))

; good
(defun power-do (x n)
  (do ((acc 1 (* acc x))
       (i 0 (+ i 1)))
       ((= i n) acc)))

(defun power-reduce (x n)
  (reduce #'* (make-list n :initial-element x)))

(defun power-reduce-vector (x n)
  (reduce #'* (make-sequence 'vector n :initial-element x)))

(defun power-recurse (x n)
  (if (eq n 1)
    x
    (* x (power-recurse x (- n 1)))))

(defun power-recurse-case (x n)
  (case n
    (1 x)
    (t (* x (power-recurse x (- n 1))))))

; good 7
(defun power-recurse-tail (x n &optional (acc 1))
  (if (eq n 0)
    acc
    (power-recurse-tail x (- n 1) (* x acc))))

(setf *all-power-fns*
  (list #'power-dotimes
        #'power-dotimes2
        #'power-dolist
        #'power-do
        #'power-reduce
        #'power-reduce-vector
        #'power-recurse
        #'power-recurse-case
        #'power-recurse-tail))

(fmap-time *all-power-fns* 999999 2 300)




;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        1.3        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun count-atoms-1 (sexpr)
  (if sexpr
    (typecase sexpr
      (list (+ (count-atoms-1 (car sexpr)) (count-atoms-1 (cdr sexpr))))
      (atom 1))
    0))

(defun count-atoms-2 (sexpr)
  (if (null sexpr)
    0
    (if (listp sexpr)
      (+ (count-atoms-2 (car sexpr)) (count-atoms-2 (cdr sexpr)))
      1)))

(defun count-atoms-3 (sexpr)
  (cond
    ((null sexpr)  0)
    ((atom sexpr) 1)
    ((listp sexpr) (+ (count-atoms-3 (car sexpr))
                      (count-atoms-3 (cdr sexpr))))))

; the best
(defun count-atoms-4 (sexpr)
  (cond
    ((null sexpr)  0)
    ((atom sexpr) 1)
    (t (+ (count-atoms-3 (car sexpr)) (count-atoms-3 (cdr sexpr))))))




;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        1.4        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun count-anywhere (the-atom sexpr)
  (cond
    ((null sexpr)        0)
    ((listp sexpr)       (+ (count-anywhere the-atom (car sexpr))
                            (count-anywhere the-atom (cdr sexpr))))
    ((eq the-atom sexpr) 1)
    (t                   0)))

(count-anywhere 'a '(a ((a) b) a))




;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        1.5        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun dot-product-1 (lis1 lis2)
  (reduce #'+ (mapcar #'* lis1 lis2)))

; best
(defun dot-product-2 (lis1 lis2)
  (let ((acc 0))
    (dotimes (i (length lis1))
      (setf acc (+ acc (* (nth i lis1) (nth i lis2)))))
    acc))

(defun dot-product-3 (lis1 lis2)
  (let ((len (length lis1))
        (acc 0))
    (if (not (eql len (length lis2)))
      (error "not the same length")
      (dotimes (i len acc)
        (setf acc (+ acc (* (nth i lis1) (nth i lis2))))))))


(dot-product-1 '(10 20) '(3 4))
