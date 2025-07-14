(defun euclidean-distance (p1 p2)
  (sqrt (reduce #'+ (mapcar (lambda (a b) (expt (- a b) 2)) p1 p2))))

(defun random-centroids (points k)
  (loop repeat k collect (nth (random (length points)) points)))

(defun closest-centroid (point centroids)
  (car (reduce (lambda (a b)
                 (if (< (euclidean-distance point (cdr a))
                        (euclidean-distance point (cdr b)))
                     a b))
               (mapcar (lambda (c) (cons c c)) centroids))))

(defun mean-point (points)
  (let ((n (length points)))
    (if (zerop n)
        (make-list (length (first points)) :initial-element 0)
        (mapcar (lambda (dim-values) (/ (reduce #'+ dim-values) n))
                (apply #'mapcar #'list points)))))

(defun k-means (points k max-iterations)
  (let ((centroids (random-centroids points k)))
    (loop repeat max-iterations do
      (let ((clusters (make-hash-table :test 'equal)))
        (dolist (p points)
          (let ((c (closest-centroid p centroids)))
            (push p (gethash c clusters))))
        (setf centroids
              (mapcar (lambda (c)
                        (mean-point (gethash c clusters)))
                      centroids))))
    centroids))

;;=== EXAMPLE ===;;
;;Input
;;(defparameter *points* '((1 2 3) (2 3 4) (10 11 12) (11 12 13) (5 5 5)))
;;Output
;;(k-means *points* 2 10)

