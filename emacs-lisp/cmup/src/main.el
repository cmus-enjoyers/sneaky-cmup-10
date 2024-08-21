(require 'cl-lib)

(cl-defstruct cmup-playlist
  name
  path
  content)

(defconst cmup-music-path
  (expand-file-name "~/music"))

(defconst cmup-playlist-path
  (expand-file-name "~/.config/cmus/playlists"))

(defun ls (path)
  (cl-remove-if (lambda (str)
                  (string-prefix-p "." str))
                (directory-files path)))

(defun make-playlist-path (path)
  (concat cmup-playlist-path "/" path))

(defun create-playlist (name)
  (let ((path (make-playlist-path name)))
    (make-cmup-playlist :name name :path path :content nil)))

(defun get-cmup-music ()
 (ls cmup-music-path))

(print (mapcar (lambda (x)
          (create-playlist x))
        (get-cmup-music)))
