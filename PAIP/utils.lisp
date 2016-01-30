
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
