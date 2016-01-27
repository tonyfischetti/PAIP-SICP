#!/opt/local/bin/sbcl --script


;;;;;;;;;
; utils

(defun println (something &optional (pfn #'print))
  (funcall pfn something)
  (format t "~%"))

(defun fmap (fns &rest args)
  (mapcar (lambda (fn) (apply fn args)) fns))

; (defun fmap-time (fns &rest args &key (times 100))
(defun fmap-time (fns times &rest args)
  (mapcar (lambda (fn)
            (println fn)
            (time (dotimes (x times) (apply fn args))))
          fns))

; or using 'labels
(defun fmap-time2 (fns times &rest args)
  (labels ((tmp (fn)
                (println fn)
                (time (dotimes (x times) (apply fn args)))))
  (mapcar #'tmp fns)))

;;;;;;;;;


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


; do with no body
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


(defun power-recurse-tail (x n &optional (acc 1))
  (if (eq n 0)
    acc
    (power-recurse-tail x (- n 1) (* x acc))))



(defvar *all-fns*
  (list #'power-dotimes
        #'power-dotimes2
        #'power-dolist
        #'power-do
        #'power-reduce
        #'power-reduce-vector
        #'power-recurse
        #'power-recurse-case
        #'power-recurse-tail))



; (mapc #'println (fmap *all-fns* 10 3))

; (fmap-time *all-fns* 999 2 300)

(println (power-do 10 3))











;;; dope fibonacci using 'do
(defun fib (lim)
  (do
    ; variable definitions form   in the form of (var init-form step-form)
    ((n 0 (+ 1 n))
     (cur 0 next)
     (next 1 (+ cur next)))
    (
     ; test form
     (= lim n)
     ; result form
     cur)))

