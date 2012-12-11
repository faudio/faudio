(defctype Doremir.Thread.Improving :pointer)

(defctype Doremir.Thread.Improving.Value :pointer)

(defcfun "Doremir.Thread.Improving.create" :Doremir.Thread.Improving ())

(defcfun "Doremir.Thread.Improving.destroy" :void (:Doremir.Thread.Improving))

(defcfun "Doremir.Thread.Improving.isDone" :boolean (:Doremir.Thread.Improving))

(defcfun "Doremir.Thread.Improving.wait" :void (:Doremir.Thread.Improving))

(defcfun "Doremir.Thread.Improving.get" :Doremir.Thread.Improving.Value (:Doremir.Thread.Improving))