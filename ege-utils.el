;;; ege-utils.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:coordinate-in-rect-p (coordinate rect)
  "Check if COORDINATE is found inside RECT.

COORDINATE can be a 2 element list, or a return value from ege:mouse-position.
RECT is a 4 element list with (col row width height)."
  (let ((col (nth 0 rect))
	(row (nth 1 rect))
	(width (nth 2 rect))
	(height (nth 3 rect))
	(point-x (car coordinate))
	(point-y (cdr coordinate)))
    (and (>= point-x col)
	 (<= point-x (+ col width))
	 (>= point-y row)
	 (<= point-y (+ row height)))))


(defun ege:rect-pressed-p (rect mouse-button-number)
  "Check if mouse is pressing over RECT.
Pressed mouse button is compared to MOUSE-BUTTON-NUMBER."
  (and (eql ege:mouse-pressed mouse-button-number)
       (ege:coordinate-in-rect-p (ege:mouse-position)
				 rect)))

(provide 'ege-utils)
;;; ege-utils.el ends here
