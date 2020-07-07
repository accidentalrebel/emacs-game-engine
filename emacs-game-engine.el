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

(defvar ege:buffer-cols 0 "The number of columns for the buffer")
(defvar ege:buffer-rows 0 "The number of rows for the buffer")

(defun ege:init (buffer-name cols rows)
  "Initialize the engine and the buffer canvas.

BUFFER-NAME is the name of the buffer
COLS is the number of columns (in characters)
ROWS is the number of rows (in characters)"
  (setq ege:buffer-cols cols)
  (setq ege:buffer-rows rows)
  
  (pop-to-buffer buffer-name)
  (ege:clear_window))

(defun ege:clear_window ()
  (erase-buffer)
  (coordinate-initialize-view-area ege:buffer-cols ege:buffer-rows "-"))

(provide 'emacs-game-engine)
;;; emacs-game-engine.el ends here
