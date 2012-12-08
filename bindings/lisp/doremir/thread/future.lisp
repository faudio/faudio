(defctype Future :long)

(defctype Value :long)

(defcfun "create" :Doremir.Thread.Future.Future ())

(defcfun "destroy" :void (:Doremir.Thread.Future.Future))

(defcfun "isDone" :bool (:Doremir.Thread.Future.Future))

(defcfun "wait" :void (:Doremir.Thread.Future.Future))

(defcfun "get" :Doremir.Thread.Future.Value (:Doremir.Thread.Future.Future))