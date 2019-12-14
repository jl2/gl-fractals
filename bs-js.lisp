;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass bs-js-vertex-shader (newgl:gl-shader)
  ((newgl:layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (newgl:shader :initform 0 :type fixnum)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "bs-js-vertex.glsl") :type (or pathname string))
   (newgl:shader-type :initform :vertex-shader)))

(defclass bs-js-fragment-shader (newgl:gl-shader)
  ((newgl:shader-type :initform :fragment-shader)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "bs-js-fragment.glsl"))))

(defclass bs-js-program (newgl:shader-program)
  ((newgl:shaders :initform (list
                       (make-instance 'bs-js-vertex-shader)
                       (make-instance 'bs-js-fragment-shader)))))


(defclass bs-js (complex-fractal)
  ((newgl:shader-program :initform (make-instance 'bs-js-program))
   (center :initarg :center :initform #C(-1.0 0.0) :type complex))
  (:documentation "A Bs-Jsbrot set."))

(defmethod newgl:set-uniforms ((object bs-js))
  ;; (format t "set-uniforms bs-js~%")
  (with-slots (newgl:shader-program center) object
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cReal") (realpart center))
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cImag") (imagpart center)))
    (call-next-method))

(defmethod newgl:handle-key ((object bs-js) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots ( center zoom-window ) object
    (with-slots (radius) zoom-window
      (let ((real-offset (* 0.00125 (realpart radius)))
            (imag-offset (* 0.00125 (imagpart radius))))
        (cond
          ((and (eq key :down) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (realpart center) (- (imagpart center) imag-offset))))

          ((and (eq key :up) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (realpart center) (+ (imagpart center) imag-offset))))

          ((and (eq key :left) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (- (realpart center) real-offset) (imagpart center))))

          ((and (eq key :right) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
           (setf center (complex (+ (realpart center) real-offset) (imagpart center))))
          ((and (eq key :j) (eq action :press))
           (with-slots (newgl:vertices) object
             (format t
                     "(newgl:viewer (gl-fractals:make-bs-js~%    :center ~a~%    :window (make-instance 'gl-fractals:complex-window~%        :center ~a~%        :radius ~a)))~%"
                     center
                     (slot-value zoom-window 'center)
                     radius)))
          (t
           (call-next-method)))))))

(defun make-bs-js (&key (center #C(-0.7682546058236294 0.3247886940487651)) (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'bs-js
                                   :center center
                                   :vertices (to-vertices window)
                                   :zoom-window window))
    (vector (make-instance 'bs-js
                           :center center
                           :vertices window
                           :zoom-window (window-from-vertices window)))))

(defun show-bs-js (&key
                     (center #C(-0.7682546058236294 0.3247886940487651))
                     (bs-js (make-bs-js
                                     :center center
                                     :window (make-instance
                                              'complex-window
                                              :center #C(-0.035025446142364025 0.07755634502952052)
                                              :radius #C(0.2949329984606149 0.2949329984606149))))
                     (in-thread nil)
                     (show-traces nil))
  (newgl:viewer bs-js :in-thread in-thread :show-traces show-traces))
