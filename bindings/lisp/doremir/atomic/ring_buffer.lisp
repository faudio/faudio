(defctype Doremir.RingBuffer :pointer)

(defcfun "Doremir.Atomic.RingBuffer.create" :Doremir.RingBuffer (:Doremir.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.copy" :Doremir.RingBuffer (:Doremir.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.swap" :void (:Doremir.RingBuffer :Doremir.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.destroy" :void (:Doremir.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.read" :pointer (:Doremir.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.write" :void (:Doremir.RingBuffer :pointer))