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
  "List non-hidden files in path. Optionally filter with filter-fn"
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
    (make-cmup-playlist :name name :path path :content
                                      (ls music-path (lambda (str)
                                        (not (music? str)))))))

(defun write-playlist (playlist)
  nil)

(defun get-cmup-music ()
 (ls cmup-music-path))

(print (mapcar (lambda (x)
          (create-playlist x))
        (get-cmup-music)))
