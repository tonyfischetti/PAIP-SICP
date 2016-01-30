
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



































