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

(defun ege:place-string-at-area (col row str &optional attributes)
  "Places at COL and ROW a given STR.
&optional ATTRIBUTES is the face attribute to use for the string.
Can accept a multiline string.

This is a wrapper function to the coordinate.el function."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area col row str attributes))))

(defun ege:place-char-at (col row char &optional attributes)
  "Place char at COL and ROW coordinates.
CHAR is the character to place.
&optional ATTRIBUTES is the face attribute to use for the character.
Coordinates use a starting index of 0.

This is a wrapper function to the coordinate.el function."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at col row char attributes))))

(provide 'ege-utils)
;;; ege-utils.el ends here
