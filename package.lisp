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

(defpackage :gl-fractals
  (:use #:cl #:j-utils #:alexandria)
  (:export #:show-mandelbrot
           #:mandelbrot
           #:make-mandelbrot

           #:julia-set
           #:make-julia-set
           #:show-julia-set

           #:bs-js
           #:make-bs-js
           #:show-bs-js

           #:burning-ship
           #:make-burning-ship
           #:show-burning-ship

           #:complex-window

           #:complex-window
           #:window-from-min-max
           #:window-from-center-radius
           #:imag-max
           #:imag-min
           #:real-max
           #:real-min

           #:make-strange-attractor
           #:make-line-strange-attractor
           ))

