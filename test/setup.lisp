
(progn
  (asdf:load-system :audio-engine))

(progn
  (push "/Users/hans/audio/build/Frameworks/" cffi:*darwin-framework-directories*)
  (setf *foreign-lib* (cffi:load-foreign-library '(:framework "DoReMIRAudio")))  
    (audio-engine::audioengine-set-log-file "/Users/hans/Library/Logs/DoReMIRAudio.log")
    (audio-engine::audioengine-initialize)
    (audio-engine::plot-use-gnu))
