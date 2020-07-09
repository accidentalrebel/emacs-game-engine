
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
    (start-process-shell-command "ege:play-sound-async" nil (concat "play " data-directory)))
  )
	       
(ege:play-sound "./data/audio/coin.wav" 0.5)
(ege:play-sound-async "./data/audio/cantina.wav" 0.5)
