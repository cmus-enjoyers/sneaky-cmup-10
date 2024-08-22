(require 'cl-lib)

(cl-defstruct cmup-playlist
  name
  path
  content)

(defconst cmup-music-path
  (expand-file-name "~/music"))

(defconst cmup-playlist-path
  (expand-file-name "~/.config/cmus/playlists"))

(defun ls (path &optional filter-fn)
  (cl-remove-if (lambda (str)
                  (or (string-prefix-p "." str)
                      (and filter-fn
                           (funcall filter-fn str))))
                (directory-files path)))

(defun make-playlist-path (path)
  (concat cmup-playlist-path "/" path))

(defun make-music-path (path)
  (concat cmup-music-path "/" path))

(defun music? (name)
  (string-suffix-p ".mp3" name))

(defun create-playlist (name)
  (let ((path (make-playlist-path name))
        (music-path (make-music-path name)))
    (make-cmup-playlist
     :name name
     :path path
     :content (ls music-path (lambda (str)
                               (not (music? str)))))))

(defun write-playlist (playlist)
  (let ((content (string-join (cmup-playlist-content playlist) "\n")))
    (let ((content-length (length content)))
      (when (> content-length 0)
        (print content-length)))))

(defun write-playlists (playlists)
  (dolist (x playlists)
    (write-playlist x)))

(defun print-playlist (playlist)
  (princ (concat "Playlist " (cmup-playlist-name playlist)
                 " on path \n" (cmup-playlist-path playlist)))
  (let (value)
    (dolist (x (cmup-playlist-content playlist) value)
      (princ (concat "   " x "\n")))))

(defun print-playlists (playlists)
  (dolist (x playlists)
    (print-playlist x)))

(defun get-cmup-music ()
  (ls cmup-music-path))

(defvar cmup-playlists (mapcar (lambda (x)
                 (create-playlist x))
               (get-cmup-music)))

(write-playlists cmup-playlists)
