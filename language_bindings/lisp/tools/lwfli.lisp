
;;
;; Invokes the LispWorks foreign parser
;;
(require          "foreign-parser")
(require          "asdf")
(asdf:load-system :audio-engine)
(in-package       :misc)

(defun print-usage (stream)
  (format stream "~%")
  (format stream "Usage: lwfli [-options] input output~%")
  (format stream "~%")
  (format stream "where options include~%")
  (format stream "    -I      C preprocessor include path~%")
  (format stream "    -p      package in which the generated code will be included~%")
  (format stream "    -c      case sensitive~%")
  nil)

(defun main ()
  (let* ((out  *standard-output*)
         (args (command-line-args)))
    (unless args
      (progn
        (print-usage out)
        (return-from main))) 
    (with-parsed-args args
      (let* ((include-path   (expand-path (find-option :i)))
             (to-package     (find-option :p))
             (case-sensitive (find-option :c))
             (cpp-options    (split-sequence #\; (find-option :x)))
             (input          (expand-path (first *arguments*)))
             (output         (expand-path (second *arguments*))))
;        (print (command-line-args))
      (setf *preprocessor-include-path* include-path)
      (foreign-parser:process-foreign-file input
           :dff output
           :case-sensitive case-sensitive
           :package (make-package to-package)
           :preprocessor-options cpp-options)
      (format out "~%Generated '~s'~%~%" output)))))

#-deliver (main)
