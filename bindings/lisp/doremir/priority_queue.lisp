(defctype Doremir.PriorityQueue :pointer)

(defctype Doremir.PriorityQueue.Value :pointer)

(defcfun "Doremir.PriorityQueue.create" :Doremir.PriorityQueue ())

(defcfun "Doremir.PriorityQueue.swap" :void (:Doremir.PriorityQueue :Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.destroy" :void (:Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.insert" :void (:Doremir.PriorityQueue :Doremir.PriorityQueue.Value))

(defcfun "Doremir.PriorityQueue.peek" :Doremir.PriorityQueue.Value (:Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.pop" :Doremir.PriorityQueue.Value (:Doremir.PriorityQueue))