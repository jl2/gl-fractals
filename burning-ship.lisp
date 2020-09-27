;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass burning-ship (complex-fractal)
  ((newgl:shader-program :initform (newgl:make-shader-program
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "complex-vertex.glsl"))
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "burning-ship-fragment.glsl"))))
   (center :initarg :center :initform #C(-1.0 0.0) :type complex))
  (:documentation "A Burning-Shipbrot set."))

(defmethod newgl:assign-uniforms ((object burning-ship) &optional view-xform)
  (declare (ignorable view-xform))
  (with-slots (newgl:shader-program center) object
    (call-next-method)))

(defmethod newgl:handle-key ((object burning-ship) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots ( center zoom-window ) object
    (with-slots (radius) zoom-window
      (cond
        ((and (eq key :j) (eq action :press))
         (format t
                 "(newgl:display (gl-fractals:make-burning-ship~%    :center ~a~%    :window (make-instance 'gl-fractals:complex-window~%        :center ~a~%        :radius ~a)))~%"
                 center
                 (slot-value zoom-window 'center)
                 radius))
        (t (call-next-method))))))

(defun make-burning-ship (&key (center #C(-0.7682546058236294 0.3247886940487651)) (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'burning-ship
                                   :center center
                                   :zoom-window window))
    (vector (make-instance 'burning-ship
                           :center center
                           :zoom-window (window-from-vertices window)))))

(defun show-burning-ship (&key
                     (center #C(-0.7682546058236294 0.3247886940487651))
                     (burning-ship (make-burning-ship
                                     :center center
                                     :window (make-instance
                                              'complex-window
                                              :center #C(-0.035025446142364025 0.07755634502952052)
                                              :radius #C(0.2949329984606149 0.2949329984606149)))))
  (newgl:display burning-ship))
