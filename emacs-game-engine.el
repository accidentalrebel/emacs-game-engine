;;; emacs-game-engine.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;; Copyright (C) 2020  AccidentalRebel

;; Author: Karlo Licudine <accidentalrebel@gmail.com>
;; Keywords: games
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;;; Code:

;; Load coordinate.el, a separate library I made that provides convenience
;; functions for editing the buffer through col and row coordinates.
(require 'coordinate)
(require 'ege-draw)
(require 'ege-canvas)
(require 'ege-keyboard)
(require 'ege-audio)
(require 'ege-mouse)
(require 'ege-utils)

;; Variables
;; =========
(defvar ege:buffer-name "Emacs Game Engine" "The name of the game buffer.")

(defvar ege:buffer-cols 0 "The number of columns for the buffer")
(defvar ege:buffer-rows 0 "The number of rows for the buffer")

(defvar ege:_update-func nil "The update function registered by the game")
(defvar ege:_update-timer nil "The returned timer function to be used for cancelling the timer.")

;; Modes
;; =====
;; Note: Since ege:ege-mode is derived from special-mode. The buffer is read-only.
;;       To write to the buffer, "inhibit-read-only" must be set to "t".
(define-derived-mode ege:ege-mode special-mode "ege-mode")

;; FUNCTIONS
;; =========
(defun ege:init (buffer-name cols rows)
  "Initialize the engine and the buffer canvas.

BUFFER-NAME is the name of the buffer
COLS is the number of columns (in characters)
ROWS is the number of rows (in characters)"
  (setq ege:buffer-name buffer-name)
  (setq ege:buffer-cols cols)
  (setq ege:buffer-rows rows)

  (select-window (nth 1 (window-list)))
  (switch-to-buffer ege:buffer-name)
  (ege:ege-mode)
  (ege:mouse-init)
  (ege:clear-buffer))

(defun ege:exit()
  "Exits the currently running game.
Handles the cancelation of update timer and other cleaning up processes."
  (interactive)
  (when ege:_update-timer
    (cancel-timer ege:_update-timer))
  (message (concat "Game " ege:buffer-name " exited")))

(defun ege:_update()
  "Internal update function used by the engine.
Handles the calling of an update function if there one is registered."
  (when
      (and ege:_update-func
	   (string= (buffer-name) ege:buffer-name))
    (funcall ege:_update-func))
  (ege:_keyboard-late-update)
  (ege:_mouse-late-update))

(defun ege:register-update (update-func fps)
  "Registers an update function UPDATE-FUNC as a game loop.

FPS the frames per second."
  (setq ege:_update-func update-func)
  (let ((delay (/ 1.0 fps)))
    (setq ege:_update-timer
	  (run-with-timer delay delay 'ege:_update))))

(defun ege:clear-buffer ()
  "Clears the buffer and its canvas so it can be rewritten to again."
  (pop-to-buffer ege:buffer-name)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(coordinate-initialize-view-area ege:buffer-cols ege:buffer-rows "-")))

(defun ege:place-string-at-area (col row str &optional attributes)
  "Places at COL and ROW a given STR.
&optional ATTRIBUTES is the face attribute to use for the string.
Can accept a multiline string.

This is a wrapper function to the coordinate.el function."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area col row str attributes))))

(defun ege:place-char-at (col row char &optional attributes)
  "Place char at COL and ROW coordinates.
CHAR is the character to place.
&optional ATTRIBUTES is the face attribute to use for the character.
Coordinates use a starting index of 0.

This is a wrapper function to the coordinate.el function."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at col row char attributes))))

(provide 'emacs-game-engine)
;;; emacs-game-engine.el ends here

