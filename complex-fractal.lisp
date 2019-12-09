;;;; complex-fractal.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass complex-fractal (newgl:vertex-object)
  ((newgl:vertices :initarg :vertices)
   (newgl:indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))
   (zoom-window :initarg :zoom-window))
  (:documentation "Base class for fractals iterated on the complex plane."))

(defun zoom-complex-fractal-window (scale cpos fractal)
  (with-slots (newgl:vertices zoom-window) fractal
    (with-slots (center radius) zoom-window
      (let* ((new-radius (* radius scale))
             (new-center (cursor-position-to-complex cpos zoom-window)))
        (setf center new-center
              radius new-radius)))
    (setf newgl:vertices (to-vertices zoom-window))))

(defun pan-complex-fractal-window (offset-percent fractal)
  (with-slots (newgl:vertices zoom-window) fractal
    (with-slots (radius center) zoom-window
      (incf center (complex (* (realpart radius) (realpart offset-percent))
                            (* (imagpart radius) (imagpart offset-percent)))))
    (setf newgl:vertices (to-vertices zoom-window))))

(defmethod newgl:handle-key ((object complex-fractal) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (need-reload
          (cond ((and (eq key :f5) (eq action :press))
                 (with-slots (newgl:vertices zoom-window) object
                   (setf zoom-window (make-instance 'complex-window))
                   (setf newgl:vertices (to-vertices zoom-window))))

                 ((and (eq key :m) (eq action :press))
                 (with-slots (newgl:vertices) object
                   (format t "(newgl:viewer :objects (newgl:make-complex-fractal :window ~a))~%" newgl:vertices))
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

  (with-slots (newgl:vertices zoom-window) object
    (with-slots (center radius) zoom-window
      (incf center (- (cursor-position-to-complex (slot-value newgl:*previous-mouse-drag* 'newgl:cursor-pos)
                                                  zoom-window)
                      (cursor-position-to-complex cursor-pos zoom-window))))
    (setf newgl:vertices (to-vertices zoom-window))
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
