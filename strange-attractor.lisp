;;;; mandelbrot.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass strange-attractor ()
  ((color :initarg :color :initform (3d-vectors:vec4-random 0.1 1.0))
   (iterations :initarg :iterations :initform 10000)
   (a :initarg :a :initform 2.24)
   (b :initarg :b :initform 0.43)
   (c :initarg :c :initform -0.65)
   (d :initarg :d :initform -2.43)
   (e :initarg :e :initform 1.0)))

(defun random-attractor (&key
                           (a-max 2.0) (a-min -2.0)
                           (b-max 2.0) (b-min -2.0)
                           (c-max 2.0) (c-min -2.0)
                           (d-max 2.0) (d-min -2.0)
                           (e-max 2.0) (e-min -2.0)
                           (color (3d-vectors:vec4-random 0.1 1.0))
                           (iterations 10000))
  (make-instance 'strange-attractor
                 :color color
                 :iterations iterations
                 :a (random-between a-min a-max)
                 :b (random-between b-min b-max)
                 :c (random-between c-min c-max)
                 :d (random-between d-min d-max)
                 :e (random-between e-min e-max)))

(defmethod print-object ((object strange-attractor-scene) stream)
  (with-slots (a b c d e) object
    (format t "(strange-attractor ~5,4f ~5,4f ~5,4f ~5,4f ~5,4f)" a b c d e)))

(defun randomize-attractor (attractor &key
                           (a-max 2.0) (a-min -2.0)
                           (b-max 2.0) (b-min -2.0)
                           (c-max 2.0) (c-min -2.0)
                           (d-max 2.0) (d-min -2.0)
                           (e-max 2.0) (e-min -2.0))
  (with-slots (a b c d e) attractor
   (setf a (ju:random-between a-min a-max))
   (setf b (ju:random-between b-min b-max))
   (setf c (ju:random-between c-min c-max))
   (setf d (ju:random-between d-min d-max))
   (setf e (ju:random-between e-min e-max))))

(defun set-attractor (attractor aval bval cval dval eval)
  (with-slots (a b c d e) attractor
   (setf a aval)
   (setf b bval)
   (setf c cval)
   (setf d dval)
   (setf e eval)))

(defun make-strange-attractor (&key
                                 (attractor (random-attractor)))
  (let* ((pc (newgl:make-point-cloud))
         (x 0)
         (y 0)
         (z 0))
    (with-slots (iterations color a b c d e) attractor

      (dotimes (i iterations)
        (let ((xx (- (sin (* a y)) (* z (cos (* b x)))))
              (yy (- (sin (* c x)) (cos (* d y))))
              (zz (* e (sin x))))
          (newgl:add-point pc
                           :x x
                           :y y
                           :z z
                           :red (3d-vectors:vx color)
                           :green (3d-vectors:vy color)
                           :blue (3d-vectors:vz color)
                           :alpha (3d-vectors:vw color))
          (setf x xx
                y yy
                z zz))))
    pc))

(defun make-line-strange-attractor (&key
                                 (attractor (random-attractor)))
  (let* ((ls (newgl:make-line-segments))
         (x 0)
         (y 0)
         (z 0))
    (with-slots (iterations color a b c d e) attractor

      (dotimes (i (* 2 iterations))
        (let ((xx (- (sin (* a y)) (* z (cos (* b x)))))
              (yy (- (sin (* c x)) (cos (* d y))))
              (zz (* e (sin x))))
          (newgl:add-point ls
                           :x xx
                           :y yy
                           :z zz
                           :red (3d-vectors:vx color)
                           :green (3d-vectors:vy color)
                           :blue (3d-vectors:vz color)
                           :alpha (3d-vectors:vw color))
          (setf x xx
                y yy
                z zz))))
    ls))
