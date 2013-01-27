(in-package :audio-engine)
(defctype plot-func (:pointer (:pointer :void)))
(defcfun (plot-show "doremir_plot_show") :void (a plot-func) (b ptr) (c nullary) (d ptr))