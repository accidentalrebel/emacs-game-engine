;; Load the game engine

(when (file-exists-p "emacs-game-engine.el")
  (load-file "./emacs-game-engine.el"))

(require 'emacs-game-engine)

(ege:init)
(ege:init_buffer "Example Game" 80 24)
