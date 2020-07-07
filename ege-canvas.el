;;; ege-canvas.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun in-game-buffer-p ()
  "Check if the current buffer is the game buffer."
  (string= (buffer-name) ege:buffer-name))

;;; ege-canvas.el ends here
