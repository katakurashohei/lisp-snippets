(ql:quickload "vecto")

(defpackage #:hilbert-curve
  (:use #:cl #:vecto))

(in-package #:hilbert-curve)

(defparameter *dx* #(1 0 -1 0))
(defparameter *dy* #(0 1 0 -1))

(defun l-system (order axiom rules)
  (dotimes (_ order)
    (let ((next ""))
      (loop for c across axiom do
        (let ((replacement (assoc c rules)))
          (setf next (concatenate 'string next
                                  (if replacement
                                      (cdr replacement)
                                      (string c))))))
      (setf axiom next)))
  axiom)

(defun lindenmayer-hilbert (order)
  (let ((axiom "A")
        (rules (list (cons #\A "-BF+AFA+FB-")
                     (cons #\B "+AF-BFB-FA+"))))
    (l-system order axiom rules)))

(defun turtle-drawing (size order)
  (let* ((instr (coerce order 'list))
         (step (/ size (expt 2 order)))
         (x 0)
         (y 0)
         (dir 0)
         (points (list (list x y))))
    (dolist (c instr (nreverse points))
      (case c
        (#\+ (setf dir (mod (+ dir 1) 4)))
        (#\- (setf dir (mod (+ dir 3) 4)))
        (#\F
         (incf x (aref *dx* dir))
         (incf y (aref *dy* dir))
         (push (list (* x step) (* y step)) points))))))

(defun draw-hilbert-png (size order file)
  (with-canvas (:width size :height size)
    (let ((points (turtle-drawing size (lindenmayer-hilbert order))))
      (stroke)
      (translate 0 (- size 1))
      (move-to 0 0)
      (mapcar (lambda (pt) (apply #'line-to pt))points)
      (stroke)
      (save-png file))))
