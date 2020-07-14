;;; ege-canvas.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:in-game-buffer-p ()
  "Check if the current buffer is the game buffer."
  (string= (buffer-name) ege:buffer-name))

(defun ege:clear-buffer ()
  "Clears the buffer and its canvas so it can be rewritten to again."
  (pop-to-buffer ege:buffer-name)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(coordinate-initialize-view-area ege:buffer-cols ege:buffer-rows "-")))

(provide 'ege-canvas)
;;; ege-canvas.el ends here
