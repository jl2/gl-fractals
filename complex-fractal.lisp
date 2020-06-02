;;;; complex-fractal.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)

(defclass complex-fractal (newgl:vertex-object)
  ((newgl:vertices :initform nil)
   (newgl:indices :initform (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2)))
   (max-iterations :initform 320 :initarg :max-iterations :type fixnum)
   (aspect-ratio :initform 1.0 :initarg :aspect-ratio :type float)
   (zoom-window :initarg :zoom-window))
  (:documentation "Base class for fractals iterated on the complex plane."))

(defmethod newgl:set-uniforms ((object complex-fractal))
  (call-next-method)
  (with-slots (newgl:shader-program max-iterations aspect-ratio) object
    (newgl:set-uniform newgl:shader-program
                       "maxIterations"
                       max-iterations)
    (newgl:set-uniform newgl:shader-program
                       "aspectRatio"
                       aspect-ratio)))

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
  (call-next-method)
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (iter-up-percent 1.10)
         (iter-down-percent 0.90)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (need-reload
          (cond ((and (eq key :f5) (eq action :press))
                 (with-slots (newgl:vertices zoom-window) object
                   (setf zoom-window (make-instance 'complex-window))
                   (setf newgl:vertices (to-vertices zoom-window))))

                ((and (eq key :w) (eq action :press))
                 (with-slots (newgl:vertices) object
                   (format t "(newgl:display (gl-fractals:make-complex-fractal :window ~a))~%" newgl:vertices))
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

                ((and (eq key :up) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 pan-offset) object))

                ((and (eq key :equal) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (1+ (floor (* max-iterations iter-up-percent)))))))

                ((and (eq key :minus) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (floor (* max-iterations iter-down-percent))))))

                ((and (eq key :left) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

                ((and (eq key :right) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex pan-offset 0.0) object))

                (t
                 nil))))
    (when need-reload
      (newgl:reload-object object)
      t)))


(defclass complex-fractal-click (newgl:mouse-click)
  ((window :initarg :window)))

(defmethod newgl:handle-resize ((object complex-fractal) window width height)
  (with-slots (aspect-ratio) object
    (setf aspect-ratio (if (< height width )
                           (/ width height 1.0)
                           (/ height width -1.0)))
    (newgl:set-uniforms object)))

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
