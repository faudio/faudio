(defctype Doremir.Improving :pointer)

(defctype Doremir.Thread.Improving.Value :pointer)

(defcfun "Doremir.Thread.Improving.create" :Doremir.Improving ())

(defcfun "Doremir.Thread.Improving.destroy" :void (:Doremir.Improving))

(defcfun "Doremir.Thread.Improving.isDone" :boolean (:Doremir.Improving))

(defcfun "Doremir.Thread.Improving.wait" :void (:Doremir.Improving))

(defcfun "Doremir.Thread.Improving.get" :Doremir.Thread.Improving.Value (:Doremir.Improving))