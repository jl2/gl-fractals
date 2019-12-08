;;;; complex-fractal.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass complex-window ()
  ((center :initarg :center :initform #C(-1.0 0.0))
   (radius :initarg :width :initform #C(3.0 3.0)))
  (:documentation "A rectangular region in the complex plain."))

(defun from-center-radius (center radius)
  (make-instance 'complex-window
                 :center center
                 :radius radius))

(defun window-from-min-max (&key real-min real-max imag-min imag-max)
  (let ((real-center (+ real-min (- real-max real-min)))
        (imag-center (+ imag-min (- imag-max imag-min)))
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

(defclass complex-fractal (newgl:opengl-object)
  ((vertices :initarg :vertices)
   (zoom-window :initarg :zoom-window)
   (indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2))))
  (:documentation "Base class for fractals iterated on the complex plane."))

(defmethod newgl:fill-buffers ((object complex-fractal))
  (call-next-method)
  (with-slots (newgl:vbos newgl:ebos vertices indices) object
    (cond ((null newgl:vbos)
           (setf newgl:vbos (gl:gen-buffers 1))
           (setf newgl:ebos (gl:gen-buffers 1))
           (let ((gl-vertices (newgl:to-gl-float-array vertices))
                 (gl-indices (newgl:to-gl-array indices :unsigned-int)))

             (gl:bind-buffer :array-buffer (car newgl:vbos))
             (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
             (gl:free-gl-array gl-vertices)

             (gl:bind-buffer :element-array-buffer (car newgl:ebos))
             (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
             (gl:free-gl-array gl-indices)))
          (t
           (gl:bind-buffer :array-buffer (car newgl:vbos))
           (gl:bind-buffer :element-array-buffer (car newgl:ebos))))))

(defmethod newgl:render ((object complex-fractal))
  (with-slots (indices) object
    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length indices))))

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

(defun zoom-complex-fractal-window (scale cpos mandel)
  (with-slots (vertices zoom-window) mandel
    (with-slots (center radius) zoom-window
      (let* ((new-radius (* radius scale))
             (new-center (cursor-position-to-complex cpos zoom-window)))
        (setf center new-center
              radius new-radius)))
    (setf vertices (to-vertices zoom-window))))

(defun pan-complex-fractal-window (offset-percent mandel)
  (with-slots (vertices zoom-window) mandel
    (with-slots (radius center) zoom-window
      (incf center (complex (* (realpart radius) (realpart offset-percent))
                            (* (imagpart radius) (imagpart offset-percent)))))
    (setf vertices (to-vertices zoom-window))))

(defmethod newgl:handle-key ((object complex-fractal) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (need-reload
          (cond ((and (eq key :f5) (eq action :press))
                 (with-slots (vertices zoom-window) object
                   (setf zoom-window (make-instance 'complex-window))
                   (setf vertices (to-vertices zoom-window))))

                 ((and (eq key :m) (eq action :press))
                 (with-slots (vertices) object
                   (format t "(newgl:viewer :objects (newgl:make-complex-fractal :window ~a))~%" vertices))
                 t)

                 ((and (eq key :page-down)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                 ((and (eq key :page-down)  (eq action :release))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                 ((and (eq key :page-up)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                 ((and (eq key :page-up)  (eq action :release))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                ((and (eq key :down) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 (- pan-offset)) object))

                ((and (eq key :down) (eq action :release))
                 (pan-complex-fractal-window (complex 0.0 (- (* 2 pan-offset))) object))

                ((and (eq key :up) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 pan-offset) object))

                ((and (eq key :up) (eq action :release))
                 (pan-complex-fractal-window (complex 0.0 (* 2 pan-offset)) object))

                ((and (eq key :left) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

                ((and (eq key :left) (eq action :release))
                 (pan-complex-fractal-window (complex (- (* 2 pan-offset)) 0.0) object))

                ((and (eq key :right) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex pan-offset 0.0) object))

                ((and (eq key :right) (eq action :release))
                 (pan-complex-fractal-window (complex (* 2 pan-offset) 0.0) object))

                (t
                 nil))))
    (when need-reload
      (newgl:reload-object object)
      t)))


(defclass complex-fractal-click (newgl:mouse-click)
  ((window :initarg :window)))

(defmethod newgl:rebuild-shaders ((object complex-fractal))
  (call-next-method)
  (with-slots (newgl:shader-program) object
    (newgl:build-shader-program newgl:shader-program)))

(defmethod newgl:handle-drag ((object complex-fractal) window (click complex-fractal-click) cursor-pos)
  (declare (ignorable window))

  (with-slots (vertices zoom-window) object
    (with-slots (center radius) zoom-window
      (setf center (cursor-position-to-complex cursor-pos zoom-window)))
    (setf vertices (to-vertices zoom-window))
    (newgl:reload-object object)
    (with-slots (newgl:cursor-pos newgl:mod-keys newgl:action newgl:button newgl:time) click
    (setf newgl:*previous-mouse-drag* (make-instance 'complex-fractal-click
                                                     :window zoom-window
                                                     :cursor-pos cursor-pos
                                                     :mod-keys newgl:mod-keys
                                                     :action newgl:action
                                                     :button newgl:button
                                                     :time newgl:time)))))


(defmethod newgl:handle-click ((object complex-fractal) window click)
  (declare (ignorable window))
  (with-slots (zoom-window) object
    (with-slots (newgl:cursor-pos newgl:mod-keys newgl:action newgl:button newgl:time) click
      (let ((mp (make-instance 'complex-fractal-click
                                              :window zoom-window
                                              :cursor-pos newgl:cursor-pos
                                              :mod-keys newgl:mod-keys
                                              :action newgl:action
                                              :button newgl:button
                                              :time newgl:time)))
        (cond ((eq newgl:action :press)
               (setf newgl:*previous-mouse-drag* mp)
               (setf newgl:*mouse-press-info* mp)
               (setf newgl:*mouse-release-info* nil))

              ((eq newgl:action :release)
               (setf newgl:*previous-mouse-drag* nil)
               (setf newgl:*mouse-press-info* nil)
               (setf newgl:*mouse-release-info* mp)))
        t))))

(defmethod newgl:handle-scroll ((object complex-fractal) window cpos x-scroll y-scroll)
  (declare (ignorable window x-scroll y-scroll))
  (zoom-complex-fractal-window (if (< 0 y-scroll)
                              0.95
                              1.05)
                          cpos
                          object)
  (let* ((win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size)))
    (glfw:set-cursor-position (coerce (/ cur-width 2.0) 'double-float)
                              (coerce (/ cur-height 2.0) 'double-float)))
  (newgl:reload-object object)
  t)
