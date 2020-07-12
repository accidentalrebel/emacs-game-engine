;;; ege-canvas.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:in-game-buffer-p ()
  "Check if the current buffer is the game buffer."
  (string= (buffer-name) ege:buffer-name))

(provide 'ege-canvas)
;;; ege-canvas.el ends here
