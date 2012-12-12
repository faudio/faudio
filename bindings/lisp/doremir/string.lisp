(defctype Doremir.String (:pointer :char))

(defctype Doremir.String.FilePath :Doremir.String)

(defcfun "Doremir.String.empty" :Doremir.String ())

(defcfun "Doremir.String.single" :Doremir.String (:char))

(defcfun "Doremir.String.length" :int (:Doremir.String))

(defcfun "Doremir.String.length" :int (:Doremir.String))

(defctype Doremir.String.Nullary (:pointer (:pointer :void)))

(defctype Doremir.String.Unary (:pointer (:pointer :void)))

(defctype Doremir.String.Binary (:pointer (:pointer :void)))

(defcfun "Doremir.String.map" :Doremir.String (:Doremir.String :Doremir.String.Unary))

(defcfun "Doremir.String.reduce" :Doremir.String (:Doremir.String :Doremir.String.Nullary :Doremir.String.Binary :char))

(defcfun "Doremir.String.mapReduce" :Doremir.String (:Doremir.String :Doremir.String.Nullary :Doremir.String.Unary :Doremir.String.Binary :char))