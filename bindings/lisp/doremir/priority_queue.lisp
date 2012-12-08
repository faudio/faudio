(defctype PriorityQueue :long)

(defctype Value :long)

(defcfun "create" :Doremir.PriorityQueue.PriorityQueue ())

(defcfun "swap" :void (:Doremir.PriorityQueue.PriorityQueue :Doremir.PriorityQueue.PriorityQueue))

(defcfun "destroy" :void (:Doremir.PriorityQueue.PriorityQueue))

(defcfun "insert" :void (:Doremir.PriorityQueue.PriorityQueue :Doremir.PriorityQueue.Value))

(defcfun "peek" :Doremir.PriorityQueue.Value (:Doremir.PriorityQueue.PriorityQueue))

(defcfun "pop" :Doremir.PriorityQueue.Value (:Doremir.PriorityQueue.PriorityQueue))