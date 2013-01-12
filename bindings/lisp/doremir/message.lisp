(defctype Doremir.Message.Address :Doremir.Ptr)

(defctype Doremir.Message :Doremir.Ptr)

(defctype Doremir.Message.Receiver (:pointer :void))

(defctype Doremir.Message.Sender (:pointer :void))

(defctype Doremir.Message.Dispatcher :pointer)

(defcfun "Doremir.Message.simple" :Doremir.Message.Dispatcher ())

(defcfun "Doremir.Message.destroy" :void (:Doremir.Message.Dispatcher))

(defcfun "Doremir.Message.buffered" :Doremir.Pair ())

(defcfun "Doremir.Message.nonBlocking" :Doremir.Pair ())