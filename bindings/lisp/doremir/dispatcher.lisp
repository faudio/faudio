(defctype Doremir.Dispatcher.Message :Doremir.List)

(defctype Doremir.Dispatcher.Address :Doremir.Ptr)

(defctype Doremir.Dispatcher.Sender (:pointer :void))

(defctype Doremir.Dispatcher.Receiver (:pointer :void))

(defctype Doremir.Dispatcher (:pointer :void))