(in-package #:cl-memcached)


;;;
;;; queue implementation from http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp
;;;

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue ()
  (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    nil))


(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (let ((items (list items)))
    (cond ((null items) nil)
	  ((or (null (q-last q)) (null (q-elements q)))
	   (setf (q-last q) (last items)
		 (q-elements q) (nconc (q-elements q) items)))
	  (t (setf (cdr (q-last q)) items
		   (q-last q) (last items))))))

;; the wrappers

(defun make-queue ()
  ""
  #+allegro (make-instance 'mp:queue)
  #-allegro (make-empty-queue))

(defmacro enqueue (queue what)
  ""
  #+allegro `(mp:enqueue ,queue ,what)
  #-allegro `(enqueue-at-end ,queue ,what))

(defmacro dequeue (queue)
  ""
  #+allegro `(mp:dequeue ,queue)
  #-allegro `(remove-front ,queue))

(defmacro queue-empty-p (queue)
  ""
  #+allegro `(mp:queue-empty-p ,queue)
  #-allegro `(empty-queue? ,queue))
