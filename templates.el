(defun c-basic-offset-from-style (style)
  (cdr (assoc 'c-basic-offset (cdr (assoc style c-style-alist)))))

(defun c-style-from-default-style ()
  (let ((c_mode (assoc 'c-mode c-default-style)))
    (cdr (if (eq c_mode nil) (assoc 'other c-default-style) c_mode))))

(defun indentation-width-c ()
  (if (eq c-basic-offset 'set-from-style)
      (c-basic-offset-from-style (c-style-from-default-style))
    c-basic-offset))

(defun instantiate-template-c (name directory)
  "Create a minimal C program and Makefile"
  (interactive "sName: \nDDirectory: ")

  (let ((project (concat directory name)))
    (unless (file-exists-p project)
      (make-directory project))

    (let ((main_c (concat (file-name-as-directory project)
                          (format "%s.c" name)))
          (indentation_width (make-string (indentation-width-c) ?\ )))
      (with-temp-buffer
        (insert (concat "#include <stdio.h>\n\n"
                        "int main(int argc, char* argv[]) {\n"
                        indentation_width
                        (format "printf(\"%s\\n\");\n" name)
                        indentation_width
                        "return 0;\n}\n"))
        (write-file main_c))
      (find-file main_c))

    (with-temp-buffer
      (insert (format "%s:\n" name))
      (write-file (concat (file-name-as-directory project) "Makefile")))))

(provide 'templates)
