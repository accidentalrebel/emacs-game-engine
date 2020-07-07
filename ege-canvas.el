;;; ege-canvas.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun in_game_buffer_p ()
  "Check if the current buffer is the game buffer."
  (string= (buffer-name) ege:buffer-name))

;;; ege-canvas.el ends here
