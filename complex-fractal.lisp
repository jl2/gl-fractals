;;;; complex-fractal.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:gl-fractals)


(defclass complex-fractal (newgl:geometry)
  ((max-iterations :initform 320 :initarg :max-iterations :type fixnum)
   (aspect-ratio :initform 1.0 :type real)
   (zoom-window :initarg :zoom-window)
   (mouse-press-info :initform nil)
   (mouse-release-info :initform nil)
   (previous-mouse-drag :initform nil))
  (:documentation "Base class for fractals iterated on the complex plane."))

(defmethod newgl:vertex-buffers ((object complex-fractal))
  (with-slots (zoom-window) object
    (values (to-vertices zoom-window)
            (make-array
             6
             :element-type 'fixnum
             :initial-contents '(0 1 2 1 3 2)))))

(defmethod newgl:render ((object complex-fractal) view-xform)
  (declare (ignorable view-xform))
  (with-slots (newgl:shader-program max-iterations aspect-ratio) object
    (newgl:set-uniform newgl:shader-program "maxIterations" max-iterations)
    (newgl:set-uniform newgl:shader-program "aspectRatio" aspect-ratio))
  (call-next-method))

(defun zoom-complex-fractal-window (scale cpos fractal)
  (with-slots (zoom-window) fractal
    (with-slots (center radius) zoom-window
      (let* ((new-radius (* radius scale))
             (new-center (cursor-position-to-complex cpos zoom-window)))
        (setf center new-center
              radius new-radius)))))

(defun pan-complex-fractal-window (offset-percent fractal)
  (with-slots (zoom-window) fractal
    (with-slots (radius center) zoom-window
      (incf center (complex (* (realpart radius) (realpart offset-percent))
                            (* (imagpart radius) (imagpart offset-percent)))))))

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
                 (with-slots (zoom-window) object
                   (setf zoom-window (make-instance 'complex-window))))

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

 (defmethod update-instance-for-different-class :before ((old newgl:mouse-click)
                                                         (new complex-fractal-click)
                                                         &key (zoom-window))
            (setf (slot-value new 'window) zoom-window))

(defmethod newgl:handle-click ((object complex-fractal) window click)
  (declare (ignorable window))
  (with-slots (zoom-window previous-mouse-drag) object
    (with-slots (zoom-window previous-mouse-drag) object
      (with-slots (newgl:action newgl:cursor-pos) click
        (cond
          ((eq newgl:action :press)
           (setf previous-mouse-drag (change-class click 'complex-fractal-click :window zoom-window)))
          ((eq newgl:action :release)
           (setf previous-mouse-drag nil))
          (previous-mouse-drag
           (with-slots (center radius) zoom-window
             (incf center (- (cursor-position-to-complex (slot-value previous-mouse-drag 'newgl:cursor-pos)
                                                         zoom-window)
                             (cursor-position-to-complex newgl:cursor-pos
                                                         zoom-window))))
           (newgl:reload-object object)
           (setf previous-mouse-drag (change-class click 'complex-fractal-click :window zoom-window)))))
      t)))

(defmethod newgl:handle-resize ((object complex-fractal) window width height)
  (declare (ignorable window))
  (with-slots (aspect-ratio) object
    (setf aspect-ratio (if (< height width )
                           (/ width height 1.0)
                           (/ height width -1.0)))
    (newgl:assign-uniforms object)))

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

(defmethod newgl:update ((object complex-fractal) elapsed-time)
  (declare (ignorable elapsed-time))
  (with-slots (zoom-window previous-mouse-drag) object
    (when previous-mouse-drag
      (format t "Dragging: ~a~%" previous-mouse-drag)
      (with-slots (newgl:cursor-pos) previous-mouse-drag
        (with-slots (center radius) zoom-window
          (incf center (- (cursor-position-to-complex newgl:cursor-pos
                                                      zoom-window)
                          (cursor-position-to-complex (glfw:get-cursor-position)
                                                      zoom-window))))
        (newgl:reload-object object)
        (with-slots ((cp newgl:cursor-pos)) previous-mouse-drag
          (setf cp (glfw:get-cursor-position)))))))
