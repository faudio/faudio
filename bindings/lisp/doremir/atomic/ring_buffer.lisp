(defctype Doremir.Atomic.RingBuffer :pointer)

(defcfun "Doremir.Atomic.RingBuffer.create" :Doremir.Atomic.RingBuffer (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.copy" :Doremir.Atomic.RingBuffer (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.swap" :void (:Doremir.Atomic.RingBuffer :Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.destroy" :void (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.read" :uint8 (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.write" :void (:Doremir.Atomic.RingBuffer :uint8))