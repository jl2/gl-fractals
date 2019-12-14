;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass burning-ship-vertex-shader (newgl:gl-shader)
  ((newgl:layout :initform
           '(((:name . "position")
              (:count . 3)
              (:type . :float))

             ((:name . "uv")
              (:count . 2)
              (:type . :float)))
           :type (or null list))

   (newgl:shader :initform 0 :type fixnum)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "burning-ship-vertex.glsl") :type (or pathname string))
   (newgl:shader-type :initform :vertex-shader)))

(defclass burning-ship-fragment-shader (newgl:gl-shader)
  ((newgl:shader-type :initform :fragment-shader)
   (newgl:source-file :initform (merge-pathnames *shader-dir* "burning-ship-fragment.glsl"))))

(defclass burning-ship-program (newgl:shader-program)
  ((newgl:shaders :initform (list
                       (make-instance 'burning-ship-vertex-shader)
                       (make-instance 'burning-ship-fragment-shader)))))


(defclass burning-ship (complex-fractal)
  ((newgl:shader-program :initform (make-instance 'burning-ship-program))
   (center :initarg :center :initform #C(-1.0 0.0) :type complex))
  (:documentation "A Burning-Shipbrot set."))

(defmethod newgl:set-uniforms ((object burning-ship))
  ;; (format t "set-uniforms burning-ship~%")
  (with-slots (newgl:shader-program center) object
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cReal") (realpart center))
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cImag") (imagpart center)))
    (call-next-method))

(defmethod newgl:handle-key ((object burning-ship) window key scancode action mod-keys)
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
                     "(newgl:viewer (gl-fractals:make-burning-ship~%    :center ~a~%    :window (make-instance 'gl-fractals:complex-window~%        :center ~a~%        :radius ~a)))~%"
                     center
                     (slot-value zoom-window 'center)
                     radius)))
          (t
           (call-next-method)))))))

(defun make-burning-ship (&key (center #C(-0.7682546058236294 0.3247886940487651)) (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'burning-ship
                                   :center center
                                   :vertices (to-vertices window)
                                   :zoom-window window))
    (vector (make-instance 'burning-ship
                           :center center
                           :vertices window
                           :zoom-window (window-from-vertices window)))))

(defun show-burning-ship (&key
                     (center #C(-0.7682546058236294 0.3247886940487651))
                     (burning-ship (make-burning-ship
                                     :center center
                                     :window (make-instance
                                              'complex-window
                                              :center #C(-0.035025446142364025 0.07755634502952052)
                                              :radius #C(0.2949329984606149 0.2949329984606149))))
                     (in-thread nil)
                     (show-traces nil))
  (newgl:viewer burning-ship :in-thread in-thread :show-traces show-traces))
