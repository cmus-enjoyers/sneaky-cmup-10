(require 'cl-lib)

(defconst cmup-music-path 
          (expand-file-name "~/music"))

(defun get-cmup-music ()
 (cl-remove-if (lambda (str)
                 (string-prefix-p "." str))
               (directory-files cmup-music-path)))

(print (get-cmup-music))
