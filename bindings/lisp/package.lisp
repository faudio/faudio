
#|
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
|#

(defpackage #:faudio

  (:nicknames   :fa)

  (:use         :common-lisp
                :cffi)

  (:shadow      :string
                :number
                :list
                :random
                :sin
                :cos
                :+
                :-
                :*
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
                :merge
                :error
                :signal
                :equal
                :find
                :min
                :max
                :print
                :list-length
                :set-difference))