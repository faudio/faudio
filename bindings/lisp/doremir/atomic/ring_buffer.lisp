(defctype RingBuffer :long)

(defcfun "create" :Doremir.Atomic.RingBuffer.RingBuffer (:Doremir.Atomic.RingBuffer.RingBuffer))

(defcfun "copy" :Doremir.Atomic.RingBuffer.RingBuffer (:Doremir.Atomic.RingBuffer.RingBuffer))

(defcfun "swap" :void (:Doremir.Atomic.RingBuffer.RingBuffer :Doremir.Atomic.RingBuffer.RingBuffer))

(defcfun "destroy" :void (:Doremir.Atomic.RingBuffer.RingBuffer))

(defcfun "read" :long (:Doremir.Atomic.RingBuffer.RingBuffer))

(defcfun "write" :long (:Doremir.Atomic.RingBuffer.RingBuffer :long))