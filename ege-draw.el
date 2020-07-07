;;; ege-draw.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:draw_rect(col row width height char &optional attributes)
  "Draw a rectangle at COL and ROW with WIDTH and HEIGHT.

Displays CHAR with optional ATTRIBUTES.
This is just a wrapper for 'coordinate-place-char-at-area'."
  (when (string= (buffer-name) ege:buffer-name)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at-area col row width height char attributes))))

;;; ege-draw.el ends here
