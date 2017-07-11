(defun instantiate-template-c (name directory)
  "Create a minimal C program and Makefile"
  (interactive "sName: \nDDirectory: ")

  (let ((project (concat directory name)))
    (unless (file-exists-p project)
      (make-directory project))

    (let ((main_c (concat (file-name-as-directory project)
                          (format "%s.c" name))))
      (with-temp-buffer
        (insert "int main(int argc, char* argv[]) {\n")
        (insert (make-string (if (equal c-basic-offset 'set-from-style)
                                 4 c-default-style)
                             ?\ ))
        (insert "return 0;\n}\n")
        (write-file main_c))
      (find-file main_c))

    (with-temp-buffer
      (insert (format "%s:\n" name))
      (write-file (concat (file-name-as-directory project) "Makefile")))))

(provide 'templates)
