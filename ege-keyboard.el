;;; ege-canvas.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defvar ege:key-pressed nil)

(defmacro ege:_set-key (key)
  "Macro for easy setting up of KEY in ege:setup-control-config."
  `(local-set-key (kbd ,key) '(lambda () (interactive) (ege:_on-key-pressed ,key))))

(defun ege:setup-control-config ()
  "Initial config for setting of controls."
  (local-set-key (kbd "<escape>") 'ege:exit)
  (ege:_set-key "w")
  (ege:_set-key "a")
  (ege:_set-key "s")
  (ege:_set-key "d")
  (ege:_set-key "<up>")
  (ege:_set-key "<left>")
  (ege:_set-key "<down>")
  (ege:_set-key "<right>"))

(add-hook 'ege:ege-mode-hook 'ege:setup-control-config)

(defun ege:_keyboard-late-update ()
  "Late update function for the keyboard."
  (setq ege:key-pressed nil)
  )

(defun ege:_on-key-pressed (key)
  "Internal function that is called when a KEY is pressed."
  (setq ege:key-pressed key)
  )

(provide 'ege-keyboard)
;;; ege-keyboard.el ends here
