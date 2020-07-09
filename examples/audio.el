
(defun ege:play-sound (path volume)
  (let* ((directory (file-name-directory path))
	 (filename (file-name-nondirectory path))
	 (data-directory (expand-file-name directory)))
    (play-sound `(sound :file ,filename
			:volume ,volume)))
  )
	       
(ege:play-sound "./data/audio/coin.wav" 0.5)
