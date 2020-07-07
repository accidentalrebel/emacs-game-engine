;;; ege-draw.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:draw-rect(col row width height char &optional attributes)
  "Draw a rectangle at COL and ROW with WIDTH and HEIGHT.

Displays CHAR with optional ATTRIBUTES.
This is just a wrapper for 'coordinate-place-char-at-area'."
  (when (string= (buffer-name) ege:buffer-name)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at-area col row width height char attributes))))

(defun ege:draw-text(str col row &optional attributes)
  "Draws the STR at COL and ROW with optional ATTRIBUTES."
  (when (string= (buffer-name) ege:buffer-name)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area col row str attributes)
      )))

;;; ege-draw.el ends here
