;;;; complex-window.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass complex-window ()
  ((center :initarg :center :initform #C(-1.0 0.0))
   (radius :initarg :radius :initform #C(3.0 3.0)))
  (:documentation "A rectangular region in the complex plain."))

(defun window-from-center-radius (center radius)
  (make-instance 'complex-window
                 :center center
                 :radius radius))

(defun window-from-min-max (&key real-min real-max imag-min imag-max)
  (let ((real-center (+ real-min (/ (- real-max real-min) 2)))
        (imag-center (+ imag-min (/ (- imag-max imag-min) 2)))
        (real-radius (/ (- real-max real-min) 2))
        (imag-radius (/ (- imag-max imag-min) 2)))
    (make-instance 'complex-window
                   :radius (complex real-radius imag-radius)
                   :center (complex real-center imag-center))))

(defun compute-min-max (window)
  "Returns (values real-min real-max imag-min imag-max)"
  (with-slots (center radius) window
    (let ((cr (realpart center))
          (ci (imagpart center))
          (rr (realpart radius))
          (ri (imagpart radius)))
      (values (- cr rr)
              (+ cr rr)
              (- ci ri)
              (+ ci ri)))))

(defun to-vertices (window)
  (multiple-value-bind (real-min real-max imag-min imag-max) (compute-min-max window)
    (make-array
     20
     :element-type 'single-float
     :initial-contents (list
                        -1.0f0  1.0f0  0.0f0 (coerce real-min 'single-float) (coerce imag-max 'single-float)
                        -1.0f0 -1.0f0  0.0f0 (coerce real-min 'single-float) (coerce imag-min 'single-float)

                        1.0f0  1.0f0  0.0f0 (coerce real-max 'single-float) (coerce imag-max 'single-float)
                        1.0f0 -1.0f0  0.0f0 (coerce real-max 'single-float) (coerce imag-min 'single-float)))))

(defun window-from-vertices (array)
  (window-from-min-max :real-min (aref array 3)
                       :real-max (aref array 13)
                       :imag-min (aref array 9)
                       :imag-max (aref array 4)))

(defun imag-min (window)
  (with-slots (center radius) window
    (- (imagpart center) (imagpart radius))))

(defun imag-max (window)
  (with-slots (center radius) window
    (+ (imagpart center) (imagpart radius))))

(defun real-min (window)
  (with-slots (center radius) window
    (- (realpart center) (realpart radius))))

(defun real-max (window)
  (with-slots (center radius) window
    (+ (realpart center) (realpart radius))))

(defun cursor-position-to-complex (cpos window)
  (let* ((x-pos (car cpos))
         (y-pos (cadr cpos))
         (win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size))
         (real-mouse (ju:map-val x-pos 0.0 cur-width (real-min window) (real-max window)))
         (imag-mouse (ju:map-val (- cur-height y-pos) 0.0 cur-height (imag-min window) (imag-max window))))
    (complex real-mouse imag-mouse)))
