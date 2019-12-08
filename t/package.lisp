;;;; package.lisp
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :gl-fractals.test
  (:use :cl
        :fiveam
        :alexandria
        :gl-fractals))

(in-package :gl-fractals.test)

(def-suite :gl-fractals)
(in-suite :gl-fractals)

(defun near (a b &optional (tol 0.0000001))
  (< (abs (- a b)) tol))

(test complex-window
  (let ((default-window (make-instance 'complex-window)))
    (is (= (real-min default-window) -4.0))
    (is (= (real-max default-window) 2.0))
    (is (= (imag-min default-window) -3.0))
    (is (= (imag-max default-window) 3.0)))

  (let ((default-window (make-instance 'complex-window)))
    (multiple-value-bind (real-min real-max imag-min imag-max) (gl-fractals::compute-min-max default-window)
      (is (= real-min -4.0))
      (is (= real-max 2.0))
      (is (= imag-min -3.0))
      (is (= imag-max 3.0))))

  (let ((window (gl-fractals::window-from-vertices #(-1.0f0 1.0f0 0.0f0 -1.1581888f0 -0.30270645f0
                                                     -1.0f0 -1.0f0 0.0f0 -1.1581888f0 -0.3111232f0
                                                     1.0f0 1.0f0 0.0f0 -1.1485512f0 -0.30270645f0
                                                     1.0f0 -1.0f0 0.0f0 -1.1485512f0 -0.3111232f0))))

    (multiple-value-bind (real-min real-max imag-min imag-max) (gl-fractals::compute-min-max window)
      (is (near -1.1581888f0 real-min))
      (is (near -1.1485512f0 real-max))
      (is (near -0.3111232f0 imag-min))
      (is (near -0.30270645f0 imag-max))))

  )
