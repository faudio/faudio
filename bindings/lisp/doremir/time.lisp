(defctype Doremir.Time :pointer)

(defcfun "Doremir.Time.create" :Doremir.Time (:int32 :int32 :int32 :Doremir.Ratio))

(defcfun "Doremir.Time.copy" :Doremir.Time (:Doremir.Time))

(defcfun "Doremir.Time.destroy" :void (:Doremir.Time))

(defcfun "Doremir.Time.days" :int32 (:Doremir.Time))

(defcfun "Doremir.Time.hours" :int32 (:Doremir.Time))

(defcfun "Doremir.Time.minutes" :int32 (:Doremir.Time))

(defcfun "Doremir.Time.seconds" :int32 (:Doremir.Time))

(defcfun "Doremir.Time.divisions" :Doremir.Ratio (:Doremir.Time))

(defcfun "Doremir.Time.toIso" :Doremir.String (:Doremir.Time))