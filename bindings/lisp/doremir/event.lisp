(in-package :audio-engine)
(define-foreign-type event-type () () (:actual-type :pointer))
(define-parse-method event () (make-instance 'event-type))
(defclass event () ((event-ptr :initarg :event-ptr)))
(defmethod translate-to-foreign (x (type event-type)) (slot-value x 'event-ptr))
(defmethod translate-from-foreign (x (type event-type)) (make-instance 'event :event-ptr x))
(defcfun (event-never "doremir_event_never") event)
(defcfun (event-now "doremir_event_now") event (a ptr))
(defcfun (event-delay "doremir_event_delay") event (a time) (b event))
(defcfun (event-merge "doremir_event_merge") event (a event) (b event))
(defcfun (event-switch "doremir_event_switch") event (a event) (b event) (c event))
(defcfun (event-destroy "doremir_event_destroy") :void (a event))
(defcfun (event-later "doremir_event_later") event (a time) (b ptr))
(defcfun (event-upon "doremir_event_upon") event (a event) (b event))
(defcfun (event-after "doremir_event_after") event (a event))
(defcfun (event-before "doremir_event_before") event (a event))
(defcfun (event-sample "doremir_event_sample") event (a event))
(defcfun (event-toggle "doremir_event_toggle") event (a event))
(defcfun (event-select "doremir_event_select") event (a event) (b event))
(defcfun (event-recv "doremir_event_recv") event (a atomic-queue))
(defcfun (event-send "doremir_event_send") event (a atomic-queue) (b event))
(defcfun (event-has-value "doremir_event_has_value") :boolean (a event) (b time))
(defcfun (event-value "doremir_event_value") ptr (a event))
(defcfun (event-offset "doremir_event_offset") time (a event))
(defcfun (event-head "doremir_event_head") event (a event))
(defcfun (event-tail "doremir_event_tail") event (a event))
(defcfun (event-filter "doremir_event_filter") event (a pred) (b ptr) (c event))
(defcfun (event-dfilter "doremir_event_dfilter") event (a pred) (b ptr) (c event))
(defcfun (event-map "doremir_event_map") event (a unary) (b ptr) (c event))
(defcfun (event-dmap "doremir_event_dmap") event (a unary) (b ptr) (c event))
(defcfun (event-join-map "doremir_event_join_map") event (a unary) (b ptr) (c event))
(defcfun (event-djoin-map "doremir_event_djoin_map") event (a unary) (b ptr) (c event))
(defcfun (event-join "doremir_event_join") event (a event))
(defcfun (event-djoin "doremir_event_djoin") event (a event))