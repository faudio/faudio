(in-package :faudio)
(defctype plot-function (:pointer (:pointer :void)))
(defcfun (plot-use-gnu "fa_plot_use_gnu") :void)
(defcfun (plot-use-core "fa_plot_use_core") :void)
(defcfun (plot-continous "fa_plot_continous") :void (a plot-function) (b ptr) (c nullary) (d ptr))
(defcfun (plot-buffer-float "fa_plot_buffer_float") :void (a buffer) (b nullary) (c ptr))
(defcfun (plot-buffer-double "fa_plot_buffer_double") :void (a buffer) (b nullary) (c ptr))

