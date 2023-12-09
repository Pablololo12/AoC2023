(defun getLines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun firstN (line)
  (if (digit-char-p (car line))
    (digit-char-p (car line))
    (firstN (cdr line))))

(defun getN (line)
  (+ (* (firstN line) 10) (firstN (reverse line))))

(defun getNumbers (lines)
  (if lines
    (cons (getN (coerce (car lines) 'list)) (getNumbers (cdr lines)))
    '()
    ))

(write (reduce #'+ (getNumbers (getLines "input.txt"))))
