(defctype Doremir.Future :pointer)

(defctype Doremir.Thread.Future.Value :pointer)

(defcfun "Doremir.Thread.Future.create" :Doremir.Future ())

(defcfun "Doremir.Thread.Future.destroy" :void (:Doremir.Future))

(defcfun "Doremir.Thread.Future.isDone" :boolean (:Doremir.Future))

(defcfun "Doremir.Thread.Future.wait" :void (:Doremir.Future))

(defcfun "Doremir.Thread.Future.get" :Doremir.Thread.Future.Value (:Doremir.Future))