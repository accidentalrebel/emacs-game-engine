;;; ege-audio.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defvar ege:audio-player "play" "Audio player to use for playing async audio files")

(defun ege:play-sound (path volume)
  "Plays the sound from relative PATH with VOLUME between 0 to 1.
IMPORTANT: This is a blocking process.
Use ege:play-sound-async for asynchronous audio playback."
  (let* ((directory (file-name-directory path))
	 (filename (file-name-nondirectory path))
	 (data-directory (expand-file-name directory)))
    (play-sound `(sound :file ,filename
			:volume ,volume)))
  )

;; TODO: An alternative way to play async with overhead https://github.com/marcinkoziej/org-pomodoro/issues/41#issuecomment-113898387
(defun ege:play-sound-async (path volume)
  "Plays the sound asynchronously from relative PATH with VOLUME between 0 to 1.
Tries to look for specific audio players found in the OS."
  (let ((data-directory (expand-file-name path)))
    (start-process-shell-command (concat "ege:play-sound:" path)
				 nil
				 (concat ege:audio-player " " data-directory))))

(defun ege:stop-sound (process)
  "Stops the given sound PROCESS.
Process can be a returned object from ege:play-sound-async, it can also be the path to the file."
  (if (processp process)
      (kill-process process)
    (kill-process (concat "ege:play-sound:" process))))

(provide 'ege-audio)
;;; ege-audio.el ends here
