(defun c-basic-offset-from-style (style)
  (cdr (assoc 'c-basic-offset (cdr (assoc style c-style-alist)))))

(defun c-style-from-default-style ()
  (let ((c_mode (assoc 'c-mode c-default-style)))
    (cdr (if (eq c_mode nil) (assoc 'other c-default-style) c_mode))))

(defun indentation-width-c ()
  (if (eq c-basic-offset 'set-from-style)
      (c-basic-offset-from-style (c-style-from-default-style))
    c-basic-offset))

(defun instantiate-template-c-like (name directory filename body)
  (let ((project (concat directory name)))
    (unless (file-exists-p project)
      (make-directory project))

    (let ((source_file (concat (file-name-as-directory project) filename))
          (indentation_width (make-string (indentation-width-c) ?\ )))
      (with-temp-buffer
        (insert (replace-regexp-in-string "\t" indentation_width body))
        (write-file source_file))
      (find-file source_file))

    (with-temp-buffer
      (insert (format "%s:\n" name))
      (write-file (concat (file-name-as-directory project) "Makefile")))))

(defun instantiate-template-c (name directory)
  "Create a minimal C program and Makefile"
  (interactive "sName: \nDDirectory: ")
  (instantiate-template-c-like name directory (format "%s.c" name)
                               (concat "#include <stdio.h>\n\n"
                                       "int main(int argc, char* argv[]) {\n"
                                       (format "\tprintf(\"%s\\n\");\n" name)
                                       "\treturn 0;\n"
                                       "}\n")))

(defun instantiate-template-cc (name directory)
  "Create a minimal C++ program and Makefile"
  (interactive "sName: \nDDirectory: ")
  (instantiate-template-c-like
   name
   directory
   (format "%s.cc" name)
   (concat "#include <iostream>\n\n"
           "int main(int argc, char* argv[]) {\n"
           (format "\tstd::cout << \"%s\\n\";\n" name)
           "\treturn 0;\n"
           "}\n")))

(provide 'templates)
