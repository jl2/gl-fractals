;;;; bs-js.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass bs-js (complex-fractal)
  ((newgl:shader-program :initform (newgl:make-shader-program
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "complex-vertex.glsl"))
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "bs-js-fragment.glsl"))))
   (center :initarg :center :initform #C(-1.0 0.0) :type complex)
   (animate :initarg :animate :initform nil))
  (:documentation "A Bs-Jsbrot set."))

(defmethod newgl:assign-uniforms ((object bs-js) &optional view-xform)
  (declare (ignorable view-xform))
  (with-slots (newgl:shader-program center) object
    (newgl:set-uniform newgl:shader-program "cReal" (realpart center))
    (newgl:set-uniform newgl:shader-program "cImag" (imagpart center))
    (call-next-method)))

(defmethod newgl:update ((object bs-js) elapsed-time)
  (with-slots (center zoom-window animate) object
    (when animate
      (with-slots (radius) zoom-window
        (let ((real-offset (ju:random-between (* -0.0048 (realpart radius))
                                              (* 0.0048 (realpart radius))))
              (imag-offset (ju:random-between (* -0.0048 (imagpart radius))
                                              (* 0.0048 (imagpart radius)))))
          (setf center (complex (+ (realpart center) real-offset)
                                (+ (imagpart center) imag-offset)))))
      (newgl:assign-uniforms object))))

(defmethod newgl:handle-key ((object bs-js) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots ( animate center zoom-window ) object
    (with-slots (radius) zoom-window
      (let ((real-offset (* 0.00125 (realpart radius)))
            (imag-offset (* 0.00125 (imagpart radius))))
        (cond
          ((and (eq key :down) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (realpart center) (- (imagpart center) imag-offset)))
           (newgl:assign-uniforms object))

          ((and (eq key :up) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (realpart center) (+ (imagpart center) imag-offset)))
           (newgl:assign-uniforms object))

          ((and (eq key :left) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (- (realpart center) real-offset) (imagpart center)))
           (newgl:assign-uniforms object))

          ((and (eq key :a) (eq action :press))
           (setf animate (not animate)))

          ((and (eq key :right) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (+ (realpart center) real-offset) (imagpart center)))
           (newgl:assign-uniforms object))

          ((and (eq key :j) (eq action :press))
           (format t
                   "(newgl:display (gl-fractals:make-bs-js~%    :center ~a~%    :window (make-instance 'gl-fractals:complex-window~%        :center ~a~%        :radius ~a)))~%"
                   center
                   (slot-value zoom-window 'center)
                   radius))
          (t
           (call-next-method)))))))



(defun make-bs-js (&key (center #C(-0.7682546058236294 0.3247886940487651)) (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'bs-js
                                   :center center
                                   :zoom-window window))
    (vector (make-instance 'bs-js
                           :center center
                           :zoom-window (window-from-vertices window)))))

(defun show-bs-js (&key
                     (center #C(-0.7682546058236294 0.3247886940487651))
                     (bs-js (make-bs-js
                                     :center center
                                     :window (make-instance
                                              'complex-window
                                              :center #C(-0.035025446142364025 0.07755634502952052)
                                              :radius #C(0.2949329984606149 0.2949329984606149)))))
  (newgl:display bs-js))
