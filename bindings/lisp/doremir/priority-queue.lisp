(defctype Doremir.PriorityQueue :pointer)

(defcfun "Doremir.PriorityQueue.create" :Doremir.PriorityQueue ())

(defcfun "Doremir.PriorityQueue.swap" :void (:Doremir.PriorityQueue :Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.destroy" :void (:Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.insert" :void (:Doremir.PriorityQueue :Doremir.Ptr))

(defcfun "Doremir.PriorityQueue.peek" :Doremir.Ptr (:Doremir.PriorityQueue))

(defcfun "Doremir.PriorityQueue.pop" :Doremir.Ptr (:Doremir.PriorityQueue))