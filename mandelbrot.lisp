;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass mandelbrot (complex-fractal)
  ((newgl:shader-program :initform (newgl:make-shader-program
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "mandel-fragment.glsl"))
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "complex-vertex.glsl")))))
  (:documentation "A Mandelbrot set."))

(defun make-mandelbrot (&key
                          (window (make-instance 'complex-window))
                          (max-iterations 3200))
  (ctypecase window
    (complex-window (make-instance 'mandelbrot
                                   :zoom-window window
                                   :max-iterations max-iterations))
    (vector (make-instance 'mandelbrot
                           :zoom-window (window-from-vertices window)))))

(defun show-mandelbrot (&key (mandel (make-mandelbrot)))
  (newgl:display mandel :debug t))
