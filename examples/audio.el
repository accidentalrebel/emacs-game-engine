;;; drawing.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)
	       
(ege:play-sound "./data/audio/coin.wav" 0.5)

;; Plays music asynchronously and then stops it after 3 seconds
(ege:play-sound-async "./data/audio/cantina.wav" 0.5)
(sleep-for 3)
(ege:stop-sound "./data/audio/cantina.wav") ;; Stopping sound by specifying filename

;; Plays music asynchronously and then stops it after 3 seconds
(setq current-sound (ege:play-sound-async "./data/audio/cantina.wav" 0.5))
(sleep-for 3)
(ege:stop-sound current-sound)) ;; Stopping sound by specifying process

