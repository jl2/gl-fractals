;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass julia-set-vertex-shader (newgl:gl-shader)
  ((newgl:layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (newgl:shader :initform 0 :type fixnum)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "julia-set-vertex.glsl") :type (or pathname string))
   (newgl:shader-type :initform :vertex-shader)))

(defclass julia-set-fragment-shader (newgl:gl-shader)
  ((newgl:shader-type :initform :fragment-shader)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "julia-set-fragment.glsl"))))

(defclass julia-set-program (newgl:shader-program)
  ((newgl:shaders :initform (list
                       (make-instance 'julia-set-vertex-shader)
                       (make-instance 'julia-set-fragment-shader)))))


(defclass julia-setbrot (complex-fractal)
  ((newgl:shader-program :initform (make-instance 'julia-set-program)))
  (:documentation "A Julia-Setbrot set."))

(defun make-julia-setbrot (&key (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'julia-setbrot
                                   :vertices (to-vertices window)
                                   :zoom-window window))
    (vector (make-instance 'julia-setbrot
                           :vertices window
                           :zoom-window (window-from-vertices window)))))

(defun show-julia-setbrot (&key (julia-set (make-julia-setbrot)) (in-thread nil) (show-traces nil))
  (newgl:viewer :objects julia-set :in-thread in-thread :show-traces show-traces))
