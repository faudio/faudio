(defctype Doremir.Atomic.RingBuffer :pointer)

(defcfun "Doremir.Atomic.RingBuffer.create" :Doremir.Atomic.RingBuffer (:size))

(defcfun "Doremir.Atomic.RingBuffer.copy" :Doremir.Atomic.RingBuffer (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.copySized" :Doremir.Atomic.RingBuffer (:size :Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.swap" :void (:Doremir.Atomic.RingBuffer :Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.destroy" :void (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.size" :size (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.read" :uint8 (:Doremir.Atomic.RingBuffer))

(defcfun "Doremir.Atomic.RingBuffer.write" :void (:Doremir.Atomic.RingBuffer :uint8))