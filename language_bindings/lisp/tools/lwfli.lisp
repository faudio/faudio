
;;
;; Invokes the LispWorks foreign parser
;;
(require "foreign-parser") 

(require          :asdf)
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
  (let* ( 
          (out  *standard-output*)
          (args (command-line-args))
        )   
    (if (not args) 
      (progn
        (print-usage out)
        (return-from main))
      (with-parsed-args args
        (let* (
                (include-path   (find-option :i)) 
                (to-package     (find-option :p)) 
                (case-sensitive (find-option :c)) 
                (cpp-options    (find-option :x)) 
                (input          (first *arguments*))
                (output         (second *arguments*))
              )
        ; (format out "include: ~s~%" include-path)
        ; (format out "include: ~s~%" to-package) 
        ; (format out "include: ~s~%" case-sensitive)
        ; (format out "include: ~s~%" cpp-options)
        ; (format out "include: ~s~%" input)
        ; (format out "include: ~s~%" output)
        (setf *preprocessor-include-path* include-path)

        (foreign-parser:process-foreign-file input 
             :dff output 
             :case-sensitive case-sensitive 
             :package (make-package to-package)
             :preprocessor-options cpp-options)

; TODO not portable!
;        (system:call-system-showing-output (format nil "sed -e \"s/\\\"~s\\\"/:~s\/\" -i \"\" ~s" to-package to-package output)) 

;sed -e "s/\"my-package\"/:my-package/" -i "" output

        (format out "~%Generated '~s'~%~%" output)

       )
     )
   )
 )
) 
      

#-deliver (main)
