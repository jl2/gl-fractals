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


(defclass julia-set (complex-fractal)
  ((newgl:shader-program :initform (make-instance 'julia-set-program))
   (center :initarg :center :initform #C(-1.0 0.0) :type complex)
   (animate :initarg :animate :initform nil))
  (:documentation "A Julia-Setbrot set."))

(defmethod newgl:update ((object julia-set))
  (with-slots (center zoom-window animate) object
    (when animate
      (with-slots (radius) zoom-window
        (let ((real-offset (ju:random-between (* -0.0048 (realpart radius))
                                              (* 0.0048 (realpart radius))))
              (imag-offset (ju:random-between (* -0.0048 (imagpart radius))
                                              (* 0.0048 (imagpart radius)))))
          (setf center (complex (+ (realpart center) real-offset)
                                (+ (imagpart center) imag-offset))))))))


(defmethod newgl:set-uniforms ((object julia-set))
  ;; (format t "set-uniforms julia-set~%")
  (with-slots (newgl:shader-program center) object
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cReal") (realpart center))
    (gl:uniformf (gl:get-uniform-location (slot-value newgl:shader-program 'newgl:program) "cImag") (imagpart center)))
    (call-next-method))

(defmethod newgl:handle-key ((object julia-set) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots ( center zoom-window animate ) object
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

          ((and (eq key :a) (eq action :press))
           (setf animate (not animate)))

          ((and (eq key :j) (eq action :press))
           (with-slots (newgl:vertices) object
             (format t
                     "(newgl:viewer (gl-fractals:make-julia-set~%    :center ~a~%    :window (make-instance 'gl-fractals:complex-window~%        :center ~a~%        :radius ~a)))~%"
                     center
                     (slot-value zoom-window 'center)
                     radius)))
          (t
           (call-next-method)))))))

(defun make-julia-set (&key (center #C(-0.7682546058236294 0.3247886940487651)) (window (make-instance 'complex-window)))
  (ctypecase window
    (complex-window (make-instance 'julia-set
                                   :center center
                                   :vertices (to-vertices window)
                                   :zoom-window window))
    (vector (make-instance 'julia-set
                           :center center
                           :vertices window
                           :zoom-window (window-from-vertices window)))))

(defun show-julia-set (&key
                         (center #C(-0.7682546058236294 0.3247886940487651))
                         (julia-set (make-julia-set
                                     :center center
                                     :window (make-instance
                                              'complex-window
                                              :center #C(-0.035025446142364025 0.07755634502952052)
                                              :radius #C(0.2949329984606149 0.2949329984606149))))
                         (in-thread nil)
                         (show-traces nil))
  (newgl:viewer julia-set :in-thread in-thread :show-traces show-traces))
