(ql:quickload "vecto")

(defpackage #:1d-subsymmetry
  (:use #:cl #:vecto))

(in-package #:1d-subsymmetry)

(defun digit-to-bits (n bits)
  (loop for i from 0 below bits
        collect (if (logbitp i n) 1 0)))

(defun is-mirror-symmetric (bits)
  (equal bits (reverse bits)))

(defun is-run-symmetric (bits)
  (let* ((len (length bits))
         (half (floor len 2)))
    (and (= (* 2 half) len)
         (or (and (every #'(lambda (b) (= b 1)) (subseq bits 0 half))
                  (every #'(lambda (b) (= b 0)) (subseq bits half)))
             (and (every #'(lambda (b) (= b 0)) (subseq bits 0 half))
                  (every #'(lambda (b) (= b 1)) (subseq bits half)))))))

(defun rotate-left (lst n)
  (append (subseq lst n) (subseq lst 0 n)))

(defun is-rotation-symmetric (bits)
  (let ((len (length bits)))
    (loop for i from 1 below len
          thereis (equal bits (rotate-left bits i)))))

(defun bit-complement (bits)
  (mapcar (lambda (b) (if (= b 1) 0 1)) bits))

(defun is-reverse-complement-symmetric (bits)
  (equal bits (bit-complement (reverse bits))))

(defun find-symmetric-windows (bits window-size &key (types '(:mirror :run-length :rotation :reverse-complement)))
  (let ((len (length bits))
        (results '())
        (count 0))
    (loop for i from 0 to (- len window-size) do
      (let* ((window (subseq bits i (+ i window-size)))
             (matched-types
               (remove nil
                       (mapcar (lambda (type)
                                 (case type
                                   (:mirror (and (is-mirror-symmetric window) :mirror))
                                   (:run-length (and (is-run-symmetric window) :run-length))
                                   (:rotation (and (is-rotation-symmetric window) :rotation))
                                   (:reverse-complement (and (is-reverse-complement-symmetric window) :reverse-complement))
                                   (t nil)))
                               types))))
        (when matched-types
          (incf count)
          (push (list :start i :window window :types matched-types) results))))
    (values count (nreverse results))))

(defparameter *canvas-width* 800)
(defparameter *canvas-height* 400)
(defparameter *beads-num* 7)

(defun pattern->filled-indices (n bits)
  (loop for i from 0 below bits
        when (logbitp i n)
          collect i))

(defun symmetry-count-for-pattern (n &optional (window-size 3))
  (multiple-value-bind (count _) (find-symmetric-windows
                                   (digit-to-bits n *beads-num*)
                                   window-size)
    count))

(defun total-symmetry-count (n window-sizes)
  (let ((bits (digit-to-bits n *beads-num*)))
    (loop for w in window-sizes
          sum (multiple-value-bind (count _) (find-symmetric-windows bits w)
                count))))

(defun flip-y (y)
  (- *canvas-height* y))

(defun draw-rectangle-array (x y filled-indices)
  (let ((rect-width 10)
        (rect-height 10)
        (spacing 0))
    (set-rgb-stroke 0 0 0)
    (set-line-width 1.5)
    (rectangle x y (* rect-width *beads-num*) rect-height)
    (stroke)
    (dotimes (i *beads-num*)
      (let ((rx (+ x (* i (+ rect-width spacing)))))
        (when (member i filled-indices)
          (set-rgb-fill 0 0 0)
          (rectangle rx y rect-width rect-height)
          (fill-path))
        (set-rgb-stroke 0 0 0)
        (set-line-width 0)
        (rectangle rx y rect-width rect-height)
        (stroke)))))


(defun draw-array-rectangles-sorted (filename &key (window-sizes '(3 5 7)) (columns 8))
  (let* ((patterns
           (loop for n from 0 below 128
                 for bits = (digit-to-bits n *beads-num*)
                 for matches = (loop for w in window-sizes
                                     append (multiple-value-bind (_ results)
                                                 (find-symmetric-windows bits w)
                                               results))
                 for symmetry-types = (remove-duplicates (mapcan (lambda (r) (getf r :types)) matches))
                 for total = (length matches)
                 collect (list n total symmetry-types (pattern->filled-indices n *beads-num*))))
         (sorted (sort patterns #'> :key #'second))
         (cell-width 150)
         (cell-height 50)
         (rows (ceiling (length sorted) columns))
         (canvas-width (+ 20 (* columns cell-width)))
         (canvas-height (+ 20 (* rows cell-height))))
    
    (with-canvas (:width canvas-width :height canvas-height)
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (let ((font (get-font "/System/Library/Fonts/Supplemental/Arial.ttf")))
        (set-font font 10)
        (dotimes (i (length sorted))
          (let* ((entry (nth i sorted))
                 (n (first entry))
                 (score (second entry))
                 (types (third entry))
                 (indices (fourth entry))
                 (col (floor i rows))
                 (row (mod i rows))
                 (x (+ 30 (* col cell-width)))
                 (y (+ *canvas-height*  (flip-y (+ 10 (* row cell-height))))))
            (draw-rectangle-array x y indices)
            (set-rgb-fill 0 0 0)
            (draw-string (+ x (* *beads-num* 10) 2.5)
                         (+ y 2.5)
                         (format nil "~A" score)))))
      (save-png filename))))

(draw-array-rectangles-sorted "symmetry-types.png" :window-sizes '( 3  5  7) :columns 8)
