;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass mandel-vertex-shader (newgl:gl-shader)
  ((newgl:layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (newgl:shader :initform 0 :type fixnum)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "mandel-vertex.glsl") :type (or pathname string))
   (newgl:shader-type :initform :vertex-shader)))

(defclass mandel-fragment-shader (newgl:gl-shader)
  ((newgl:shader-type :initform :fragment-shader)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "mandel-fragment.glsl"))))

(defclass mandel-program (newgl:shader-program)
  ((newgl:shaders :initform (list
                       (make-instance 'mandel-vertex-shader)
                       (make-instance 'mandel-fragment-shader)))))


(defclass mandelbrot (complex-fractal)
  ((newgl:shader-program :initform (make-instance 'mandel-program)))
  (:documentation "A Mandelbrot set."))

(defun make-mandelbrot (&key (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'mandelbrot
                                   :vertices (to-vertices window)
                                   :zoom-window window))
    (vector (make-instance 'mandelbrot
                           :vertices window
                           :zoom-window (window-from-vertices window)))))

(defun show-mandelbrot (&key (mandel (make-mandelbrot)) (in-thread nil) (show-traces nil))
  (newgl:viewer :objects mandel :in-thread in-thread :show-traces show-traces))
