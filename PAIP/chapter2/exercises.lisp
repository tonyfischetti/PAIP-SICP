#!/opt/local/bin/sbcl --script


(load "../utils.lisp")



;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        2.1        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun mappend (fn the-list)
  (if (null the-list)
    nil
    (append (funcall fn (first the-list))
            (mappend fn (rest the-list)))))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammers.")

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-2 (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        (t (let ((this (rewrites phrase)))
             (cond ((null this) (list phrase))
                   (t (generate-2 (random-elt this))))))))

(generate 'sentence)
(generate-2 'sentence)





;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;                   ;;
;;        2.3        ;;
;;                   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *wine-review-grammar*
  '((review -> 



