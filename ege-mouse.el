;;; ege-mouse.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defun ege:_setup-mouse-events ()
  (local-set-key [mouse-1] 'ege:_on-mouse-pressed))

(defun ege:mouse-init ()
  (ege:_setup-mouse-events)
  )

(defun ege:_on-mouse-pressed ()
  (interactive)
  (message "Clicked")
  )

(defun ege:mouse-position ()
  (mouse-position)
  )

(provide 'ege-mouse)
;;; ege-mouse.el ends here
