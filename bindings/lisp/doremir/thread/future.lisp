(defctype Doremir.Thread.Future :pointer)

(defctype Doremir.Thread.Future.Value :Doremir.Ptr)

(defcfun "Doremir.Thread.Future.create" :Doremir.Thread.Future (:Doremir.Closure))

(defcfun "Doremir.Thread.Future.destroy" :void (:Doremir.Thread.Future))

(defcfun "Doremir.Thread.Future.isDone" :boolean (:Doremir.Thread.Future))

(defcfun "Doremir.Thread.Future.wait" :void (:Doremir.Thread.Future))

(defcfun "Doremir.Thread.Future.get" :Doremir.Thread.Future.Value (:Doremir.Thread.Future))