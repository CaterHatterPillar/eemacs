(defun c-basic-offset-from-style (style)
  (cdr (assoc 'c-basic-offset (cdr (assoc style c-style-alist)))))

(defun c-style-from-default-style ()
  (let ((c_mode (assoc 'c-mode c-default-style)))
    (cdr (if (eq c_mode nil) (assoc 'other c-default-style) c_mode))))

(defun indentation-width-c ()
  (if (eq c-basic-offset 'set-from-style)
      (c-basic-offset-from-style (c-style-from-default-style))
    c-basic-offset))

(defun create-and-open-source-file (directory file_name body)
  (let ((file_path (concat (file-name-as-directory project) file_name))
        (indentation_width (make-string (indentation-width-c) ?\ )))
    (with-temp-buffer
      (insert (replace-regexp-in-string "\t" indentation_width body))
      (write-file file_path))
    (find-file file_path)))

(defun create-implicit-makefile (name directory)
  (with-temp-buffer
    (insert (format "%s:\n" name))
    (write-file (concat (file-name-as-directory directory) "Makefile"))))

(defun formalize-template (name directory file_name body)
  (let ((project (concat directory name)))
    (unless (file-exists-p project)
      (make-directory project))

    (create-and-open-source-file directory file_name body)
    (create-implicit-makefile name project)))

(defun template-formalize-c (name directory)
  "Create a minimal C program and Makefile"
  (interactive "sName: \nDDirectory: ")
  (let ((c_file_name (format "%s.c" name))
        (c_body (concat "#include <stdio.h>\n\n"
                        "int main(int argc, char* argv[]) {\n"
                        (format "\tprintf(\"%s\\n\");\n" name)
                        "\treturn 0;\n"
                        "}\n")))
    (formalize-template name directory c_file_name c_body)))

(defun template-formalize-cc (name directory)
  "Create a minimal C++ program and Makefile"
  (interactive "sName: \nDDirectory: ")
  (let ((cc_file_name (format "%s.cc" name))
        (cc_body (concat "#include <iostream>\n\n"
                         "int main(int argc, char* argv[]) {\n"
                         (format "\tstd::cout << \"%s\\n\";\n" name)
                         "\treturn 0;\n"
                         "}\n")))
    (formalize-template name directory cc_file_name cc_body)))

(provide 'template)
