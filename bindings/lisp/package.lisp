
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(defpackage #:audio-engine

  (:nicknames   :ae)

  (:use         :common-lisp
                :cffi)

  (:shadow      :string
                :number
                :list
                :loop
                :set
                :map
                :ratio
                :time
                :type
                :sequence
                :parallel
                :identity
                :constant
                :get
                :error
                :signal
                :equal
                :find
                :min
                :max
                :print
                :list-length
                :set-difference))