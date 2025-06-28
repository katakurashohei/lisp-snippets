(ql:quickload "vecto")

(defpackage #:1d-subsymmetry
  (:use #:cl #:vecto))

(in-package #:1d-subsymmetry)

(defparameter *canvas-width* 800)
(defparameter *canvas-height* 400)

(defun flip-y (y)
  (- *canvas-height* y))

(defun draw-rectangle-array (x y filled-indices)
  (let ((rect-width 10)
        (rect-height 10)
        (spacing 0)
        (fy (flip-y y)))
    (dotimes (i 7)
      (let ((rx (+ x (* i (+ rect-width spacing)))))
        (when (member i filled-indices)
          (set-rgb-fill  0 0 0)
          (rectangle rx (- fy rect-height) rect-width rect-height)
          (fill-path))
        (set-rgb-stroke 0 0 0)
        (set-line-width 1.2)
        (rectangle rx (- fy rect-height) rect-width rect-height)
        (stroke)))))


(defun pattern->filled-indices (n)
  (loop for i from 0 below 7
        when (logbitp i n)
        collect i))


(defun draw-array-rectangles (filename)
  (with-canvas (:width *canvas-width* :height *canvas-height*)
    (set-rgb-fill 1 1 1)
    (clear-canvas)

    (let ((columns 8)
          (x-margin 10)
          (y-margin 10)
          (x-spacing 100)
          (y-spacing 20))
      (dotimes (n 128)
        (let* ((row (floor n columns))
               (col (mod n columns))
               (x (+ x-margin (* col x-spacing)))
               (y (+ y-margin (* row y-spacing)))
               (indices (pattern->filled-indices n)))
          (draw-rectangle-array x y indices))))

    (save-png filename)))


(draw-array-rectangles "1d-subsymmetry.png")
