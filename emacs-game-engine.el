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

(defun ege:init ()
  "Initialize the engine."
  (message "Emacs game engine initialized"))

(defun ege:init_buffer (buffer-name cols rows)
  "Initialize the buffer to use for the game.

BUFFER-NAME is the name of the buffer
COLS is the number of columns (in characters)
ROWS is the number of rows (in characters)"
  (pop-to-buffer buffer-name)
  (coordinate-initialize-view-area cols rows "-"))

(provide 'emacs-game-engine)
;;; emacs-game-engine.el ends here
