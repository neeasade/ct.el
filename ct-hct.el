;;; ct-hct.el --- HCT emacs lisp implementation -*- coding: utf-8; lexical-binding: t -*-

;; Author: neeasade
;; SPDX-License-Identifier: MIT
;;

;;; Commentary:
;; This file is is an llm generated file (5.6 sol).

;; HCT implementation adapted from Material Color Utilities.
;; https://github.com/material-foundation/material-color-utilities
;; Copyright 2021 Google LLC, licensed under the Apache License 2.0.

(defconst ct--hct-scaled-discount-from-linrgb
  '((0.001200833568784504 0.002389694492170889 0.0002795742885861124)
     (0.0005891086651375999 0.0029785502573438758 0.0003270666104008398)
     (0.00010146692491640572 0.0005364214359186694 0.0032979401770712076)))

(defconst ct--hct-linrgb-from-scaled-discount
  '((1373.2198709594231 -1100.4251190754821 -7.278681089101213)
     (-271.815969077903 559.6580465940733 -32.46047482791194)
     (1.9622899599665666 -57.173814538844006 308.7233197812385)))

(defconst ct--hct-y-from-linrgb '(0.2126 0.7152 0.0722))

(defun ct--hct-signum (number)
  "Return the sign of NUMBER."
  (cond
    ((< number 0) -1.0)
    ((> number 0) 1.0)
    (t 0.0)))

(defun ct--hct-matrix-multiply (row matrix)
  "Multiply three-element ROW by three-by-three MATRIX."
  (--map (apply #'+ (-zip-with #'* row it)) matrix))

(defun ct--hct-linearized (component)
  "Linearize an sRGB COMPONENT in the 0-255 range to the 0-100 range."
  (let ((normalized (/ component 255.0)))
    (* 100.0
      (if (<= normalized 0.040449936)
        (/ normalized 12.92)
        (expt (/ (+ normalized 0.055) 1.055) 2.4)))))

(defun ct--hct-true-delinearized (component)
  "Delinearize a linear RGB COMPONENT to a floating-point sRGB value."
  (let ((normalized (/ component 100.0)))
    (* 255.0
      (if (<= normalized 0.0031308)
        (* normalized 12.92)
        (- (* 1.055 (expt normalized (/ 1.0 2.4))) 0.055)))))

(defun ct--hct-delinearized (component)
  "Delinearize a linear RGB COMPONENT to an integer sRGB value."
  (round (ct-clamp (ct--hct-true-delinearized component) 0 255)))

(defun ct--hct-rgb-to-name (rgb)
  "Convert integer RGB components to a color name."
  (apply #'ct--rgb-to-name (--map (/ it 255.0) rgb)))

(defun ct--hct-name-to-rgb (color)
  "Convert COLOR to integer RGB components without display gamut reduction."
  (if (string-match "\\`#\\([[:xdigit:]]+\\)\\'" color)
    (let* ((hex (match-string 1 color))
            (length (length hex))
            (width (/ length 3)))
      (if (memq length '(3 6 9 12))
        (--map
          (round (* 255.0
                   (/ (string-to-number
                        (substring hex (* it width) (* (1+ it) width))
                        16)
                     (float (1- (expt 16 width))))))
          '(0 1 2))
        (error "Invalid RGB color: %s" color)))
    (-map (lambda (component) (round (* component 255.0)))
      (or (color-name-to-rgb color)
        (error "Unknown color: %s" color)))))

(defun ct--hct-linrgb-to-name (linrgb)
  "Convert linear RGB components to a color name."
  (ct--hct-rgb-to-name (-map #'ct--hct-delinearized linrgb)))

(defun ct--hct-lab-f (value)
  "Apply the Lab transfer function to VALUE."
  (let ((epsilon (/ 216.0 24389.0))
         (kappa (/ 24389.0 27.0)))
    (if (> value epsilon)
      (expt value (/ 1.0 3.0))
      (/ (+ (* kappa value) 16.0) 116.0))))

(defun ct--hct-lab-inv-f (value)
  "Apply the inverse Lab transfer function to VALUE."
  (let* ((epsilon (/ 216.0 24389.0))
          (kappa (/ 24389.0 27.0))
          (cubed (* value value value)))
    (if (> cubed epsilon)
      cubed
      (/ (- (* 116.0 value) 16.0) kappa))))

(defun ct--hct-y-from-tone (tone)
  "Convert HCT TONE, an L* value, to XYZ Y."
  (* 100.0 (ct--hct-lab-inv-f (/ (+ tone 16.0) 116.0))))

(defun ct--hct-tone-from-y (y)
  "Convert XYZ Y to HCT tone."
  (- (* 116.0 (ct--hct-lab-f (/ y 100.0))) 16.0))

(defun ct--hct-default-viewing-conditions ()
  "Return CAM16 viewing conditions for sRGB."
  (let* ((white-point '(95.047 100.0 108.883))
          (adapting-luminance
            (* (/ 200.0 float-pi) (/ (ct--hct-y-from-tone 50.0) 100.0)))
          (r-w (+ (* (nth 0 white-point) 0.401288)
                 (* (nth 1 white-point) 0.650173)
                 (* (nth 2 white-point) -0.051461)))
          (g-w (+ (* (nth 0 white-point) -0.250268)
                 (* (nth 1 white-point) 1.204414)
                 (* (nth 2 white-point) 0.045854)))
          (b-w (+ (* (nth 0 white-point) -0.002079)
                 (* (nth 1 white-point) 0.048952)
                 (* (nth 2 white-point) 0.953127)))
          (f 1.0)
          (c 0.69)
          (d (ct-clamp
               (* f (- 1.0
                      (* (/ 1.0 3.6)
                        (exp (/ (- (- adapting-luminance) 42.0) 92.0)))))
               0.0 1.0))
          (rgb-d (list (+ (* d (/ 100.0 r-w)) 1.0 (- d))
                   (+ (* d (/ 100.0 g-w)) 1.0 (- d))
                   (+ (* d (/ 100.0 b-w)) 1.0 (- d))))
          (k (/ 1.0 (+ (* 5.0 adapting-luminance) 1.0)))
          (k4 (expt k 4))
          (k4-f (- 1.0 k4))
          (fl (+ (* k4 adapting-luminance)
                (* 0.1 k4-f k4-f
                  (expt (* 5.0 adapting-luminance) (/ 1.0 3.0)))))
          (n (/ (ct--hct-y-from-tone 50.0) (nth 1 white-point)))
          (z (+ 1.48 (sqrt n)))
          (nbb (/ 0.725 (expt n 0.2)))
          (rgb-a-factors
            (-zip-with (lambda (discount response)
                         (expt (/ (* fl discount response) 100.0) 0.42))
              rgb-d (list r-w g-w b-w)))
          (rgb-a (--map (/ (* 400.0 it) (+ it 27.13)) rgb-a-factors))
          (aw (* (+ (* 2.0 (nth 0 rgb-a))
                   (nth 1 rgb-a)
                   (* 0.05 (nth 2 rgb-a)))
                nbb)))
    (list :n n :aw aw :nbb nbb :ncb nbb :c c :nc f :rgb-d rgb-d
      :fl fl :fl-root (expt fl 0.25) :z z)))

(defconst ct--hct-viewing-conditions (ct--hct-default-viewing-conditions))

(defun ct--hct-from-name (color)
  "Return the HCT hue, chroma, and tone of COLOR."
  (let* ((rgb (ct--hct-name-to-rgb color))
          (linrgb (-map #'ct--hct-linearized rgb))
          (x (+ (* 0.41233895 (nth 0 linrgb))
               (* 0.35762064 (nth 1 linrgb))
               (* 0.18051042 (nth 2 linrgb))))
          (y (+ (* 0.2126 (nth 0 linrgb))
               (* 0.7152 (nth 1 linrgb))
               (* 0.0722 (nth 2 linrgb))))
          (z (+ (* 0.01932141 (nth 0 linrgb))
               (* 0.11916382 (nth 1 linrgb))
               (* 0.95034478 (nth 2 linrgb))))
          (r-c (+ (* 0.401288 x) (* 0.650173 y) (* -0.051461 z)))
          (g-c (+ (* -0.250268 x) (* 1.204414 y) (* 0.045854 z)))
          (b-c (+ (* -0.002079 x) (* 0.048952 y) (* 0.953127 z)))
          (vc ct--hct-viewing-conditions)
          (rgb-d (plist-get vc :rgb-d))
          (fl (plist-get vc :fl))
          (adapt
            (lambda (component)
              (let ((factor (expt (/ (* fl (abs component)) 100.0) 0.42)))
                (/ (* (ct--hct-signum component) 400.0 factor)
                  (+ factor 27.13)))))
          (r-a (funcall adapt (* (nth 0 rgb-d) r-c)))
          (g-a (funcall adapt (* (nth 1 rgb-d) g-c)))
          (b-a (funcall adapt (* (nth 2 rgb-d) b-c)))
          (a (/ (+ (* 11.0 r-a) (* -12.0 g-a) b-a) 11.0))
          (b (/ (- (+ r-a g-a) (* 2.0 b-a)) 9.0))
          (u (/ (+ (* 20.0 r-a) (* 20.0 g-a) (* 21.0 b-a)) 20.0))
          (p2 (/ (+ (* 40.0 r-a) (* 20.0 g-a) b-a) 20.0))
          (hue (mod (radians-to-degrees (atan b a)) 360.0))
          (hue-prime (if (< hue 20.14) (+ hue 360.0) hue))
          (e-hue (* 0.25 (+ (cos (+ (degrees-to-radians hue-prime) 2.0)) 3.8)))
          (p1 (* (/ 50000.0 13.0) e-hue (plist-get vc :nc) (plist-get vc :ncb)))
          (ac (* p2 (plist-get vc :nbb)))
          (j (* 100.0 (expt (/ ac (plist-get vc :aw))
                        (* (plist-get vc :c) (plist-get vc :z)))))
          (t-value (/ (* p1 (sqrt (+ (* a a) (* b b)))) (+ u 0.305)))
          (alpha (* (expt t-value 0.9)
                   (expt (- 1.64 (expt 0.29 (plist-get vc :n))) 0.73)))
          (chroma (* alpha (sqrt (/ j 100.0)))))
    (list hue chroma (ct--hct-tone-from-y y))))

(defun ct--hct-sanitize-radians (angle)
  "Return a coterminal ANGLE between zero and two pi."
  (mod (+ angle (* float-pi 8.0)) (* float-pi 2.0)))

(defun ct--hct-chromatic-adaptation (component)
  "Apply CAM16 chromatic adaptation to COMPONENT."
  (let ((factor (expt (abs component) 0.42)))
    (/ (* (ct--hct-signum component) 400.0 factor) (+ factor 27.13))))

(defun ct--hct-hue-of (linrgb)
  "Return CAM16 hue in radians for LINRGB."
  (-let* (((r-a g-a b-a)
            (-map #'ct--hct-chromatic-adaptation
              (ct--hct-matrix-multiply linrgb ct--hct-scaled-discount-from-linrgb)))
           (a (/ (+ (* 11.0 r-a) (* -12.0 g-a) b-a) 11.0))
           (b (/ (- (+ r-a g-a) (* 2.0 b-a)) 9.0)))
    (atan b a)))

(defun ct--hct-in-cyclic-order-p (a b c)
  "Return non-nil when angle B occurs between angles A and C."
  (< (ct--hct-sanitize-radians (- b a))
    (ct--hct-sanitize-radians (- c a))))

(defun ct--hct-lerp-point (source amount target)
  "Interpolate from SOURCE to TARGET by AMOUNT."
  (-zip-with (lambda (start end) (+ start (* (- end start) amount)))
    source target))

(defun ct--hct-set-coordinate (source coordinate target axis)
  "Intersect SOURCE-TARGET with COORDINATE on AXIS."
  (ct--hct-lerp-point source
    (/ (- coordinate (nth axis source))
      (- (nth axis target) (nth axis source)))
    target))

(defun ct--hct-nth-vertex (y n)
  "Return possible vertex N of the RGB cube intersected at Y."
  (-let* (((k-r k-g k-b) ct--hct-y-from-linrgb)
           (coordinate-a (if (<= (mod n 4) 1) 0.0 100.0))
           (coordinate-b (if (= (mod n 2) 0) 0.0 100.0))
           (bounded (lambda (value) (<= 0.0 value 100.0))))
    (cond
      ((< n 4)
        (let ((r (/ (- y (* coordinate-a k-g) (* coordinate-b k-b)) k-r)))
          (when (funcall bounded r) (list r coordinate-a coordinate-b))))
      ((< n 8)
        (let ((g (/ (- y (* coordinate-b k-r) (* coordinate-a k-b)) k-g)))
          (when (funcall bounded g) (list coordinate-b g coordinate-a))))
      (t
        (let ((b (/ (- y (* coordinate-a k-r) (* coordinate-b k-g)) k-b)))
          (when (funcall bounded b) (list coordinate-a coordinate-b b)))))))

(defun ct--hct-bisect-to-segment (y target-hue)
  "Find the RGB boundary segment containing TARGET-HUE at Y."
  (let ((left)
         (right)
         (left-hue 0.0)
         (right-hue 0.0)
         (uncut t))
    (dotimes (n 12)
      (let ((mid (ct--hct-nth-vertex y n)))
        (when mid
          (let ((mid-hue (ct--hct-hue-of mid)))
            (if (not left)
              (setq left mid right mid left-hue mid-hue right-hue mid-hue)
              (when (or uncut (ct--hct-in-cyclic-order-p left-hue mid-hue right-hue))
                (setq uncut nil)
                (if (ct--hct-in-cyclic-order-p left-hue target-hue mid-hue)
                  (setq right mid right-hue mid-hue)
                  (setq left mid left-hue mid-hue))))))))
    (list left right)))

(defun ct--hct-critical-plane (index)
  "Return linear RGB coordinate of sRGB half-step INDEX."
  (ct--hct-linearized (+ index 0.5)))

(defun ct--hct-bisect-to-limit (y target-hue)
  "Find the maximum-chroma linear RGB color at Y and TARGET-HUE."
  (-let* (((left right) (ct--hct-bisect-to-segment y target-hue))
           (left-hue (ct--hct-hue-of left)))
    (dotimes (axis 3)
      (unless (= (nth axis left) (nth axis right))
        (let ((left-plane (if (< (nth axis left) (nth axis right))
                            (floor (- (ct--hct-true-delinearized (nth axis left)) 0.5))
                            (ceiling (- (ct--hct-true-delinearized (nth axis left)) 0.5))))
               (right-plane (if (< (nth axis left) (nth axis right))
                              (ceiling (- (ct--hct-true-delinearized (nth axis right)) 0.5))
                              (floor (- (ct--hct-true-delinearized (nth axis right)) 0.5)))))
          (dotimes (_ 8)
            (when (> (abs (- right-plane left-plane)) 1)
              (let* ((mid-plane (floor (/ (+ left-plane right-plane) 2.0)))
                      (mid (ct--hct-set-coordinate left
                             (ct--hct-critical-plane mid-plane) right axis))
                      (mid-hue (ct--hct-hue-of mid)))
                (if (ct--hct-in-cyclic-order-p left-hue target-hue mid-hue)
                  (setq right mid right-plane mid-plane)
                  (setq left mid left-hue mid-hue left-plane mid-plane))))))))
    (-zip-with (lambda (a b) (/ (+ a b) 2.0)) left right)))

(defun ct--hct-inverse-chromatic-adaptation (adapted)
  "Invert CAM16 chromatic adaptation for ADAPTED."
  (let* ((magnitude (abs adapted))
          (base (max 0.0 (/ (* 27.13 magnitude) (- 400.0 magnitude)))))
    (* (ct--hct-signum adapted) (expt base (/ 1.0 0.42)))))

(defun ct--hct-find-result-by-j (hue chroma y)
  "Find an exact HCT color with HUE, CHROMA, and luminance Y."
  (let* ((j (* (sqrt y) 11.0))
          (vc ct--hct-viewing-conditions)
          (t-inner-coefficient
            (/ 1.0 (expt (- 1.64 (expt 0.29 (plist-get vc :n))) 0.73)))
          (e-hue (* 0.25 (+ (cos (+ hue 2.0)) 3.8)))
          (p1 (* e-hue (/ 50000.0 13.0) (plist-get vc :nc) (plist-get vc :ncb)))
          (h-sin (sin hue))
          (h-cos (cos hue))
          result
          done)
    (dotimes (iteration 5)
      (unless done
        (let* ((j-normalized (/ j 100.0))
                (alpha (if (or (= chroma 0.0) (= j 0.0))
                         0.0
                         (/ chroma (sqrt j-normalized))))
                (t-value (expt (* alpha t-inner-coefficient) (/ 1.0 0.9)))
                (ac (* (plist-get vc :aw)
                      (expt j-normalized
                        (/ 1.0 (plist-get vc :c) (plist-get vc :z)))))
                (p2 (/ ac (plist-get vc :nbb)))
                (gamma (/ (* 23.0 (+ p2 0.305) t-value)
                         (+ (* 23.0 p1)
                           (* 11.0 t-value h-cos)
                           (* 108.0 t-value h-sin))))
                (a (* gamma h-cos))
                (b (* gamma h-sin))
                (r-a (/ (+ (* 460.0 p2) (* 451.0 a) (* 288.0 b)) 1403.0))
                (g-a (/ (- (* 460.0 p2) (* 891.0 a) (* 261.0 b)) 1403.0))
                (b-a (/ (- (* 460.0 p2) (* 220.0 a) (* 6300.0 b)) 1403.0))
                (linrgb
                  (ct--hct-matrix-multiply
                    (-map #'ct--hct-inverse-chromatic-adaptation (list r-a g-a b-a))
                    ct--hct-linrgb-from-scaled-discount))
                (fn-j (apply #'+ (-zip-with #'* ct--hct-y-from-linrgb linrgb))))
          (cond
            ((or (< (apply #'min linrgb) 0.0) (<= fn-j 0.0))
              (setq done t))
            ((or (= iteration 4) (< (abs (- fn-j y)) 0.002))
              (when (<= (apply #'max linrgb) 100.01)
                (setq result (ct--hct-linrgb-to-name linrgb)))
              (setq done t))
            (t
              (setq j (- j (/ (* (- fn-j y) j) (* 2.0 fn-j)))))))))
    result))

(defun ct--hct-solve-to-name (hue chroma tone)
  "Return the sRGB color nearest HCT HUE, CHROMA, and TONE."
  (setq tone (ct-clamp tone))
  (if (or (< chroma 0.0001) (< tone 0.0001) (> tone 99.9999))
    (ct--hct-linrgb-to-name
      (let ((y (ct--hct-y-from-tone tone))) (list y y y)))
    (let* ((hue (mod hue 360.0))
            (hue-radians (degrees-to-radians hue))
            (y (ct--hct-y-from-tone tone)))
      (or (ct--hct-find-result-by-j hue-radians (max 0.0 chroma) y)
        (ct--hct-linrgb-to-name (ct--hct-bisect-to-limit y hue-radians))))))

(provide 'ct-hct)
;;; ct-hct.el ends here
