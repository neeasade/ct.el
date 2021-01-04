;;; color-tools.el --- Utilities for editing individual colors -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2020 neeasade
;;
;; Version: 0.1
;; Author: neeasade
;; Keywords: color, theming, rgb, hsv, hsl, cie-lab, background
;; URL: https://github.com/neeasade/color-tools.el
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (hsluv "1.0.0"))

;;; Commentary:
;; neeasade's color tools for Emacs.
;; primarily oriented towards a consistent interface into color spaces.
;;
;; There are 3 sections to this file (IE, you may use these bullets as search text):
;; - helpers to build color space functions
;; - color space functions
;; - other color functions

(require 'color)
(require 'hsluv)
(require 'dash)

;; some standardish aliases
(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'third 'caddr)

;; customization:

(defgroup color-tools nil
  "Utilities for editing individual colors."
  :group 'color-tools
  :prefix "ct/")

(defcustom ct/always-shorten t
  "Whether results of color functions should ensure format #HHHHHH rather than #HHHHHHHHHHHH."
  :type 'boolean
  :group 'color-tools)

;;;
;;; helpers to build color space functions
;;;

(defun ct/clamp (value min max)
  "Make sure VALUE is a number between MIN and MAX inclusive."
  (min max (max min value)))

(defun ct/shorten (c)
  "Optionally transform C #HHHHHHHHHHHH to #HHHHHH."
  (if (= (length c) 7)
    c
    (--> c
      (color-name-to-rgb it)
      `(color-rgb-to-hex ,@ it 2)
      (eval it))))

(defun ct/maybe-shorten (c)
  "Internal function for optionally shortening color C -- see variable ct/always-shorten."
  (if ct/always-shorten
    (ct/shorten c)
    c))

(defun ct/name-to-lab (name &optional white-point)
  "Transform NAME into LAB colorspace with optional lighting assumption WHITE-POINT."
  (--> name
    (color-name-to-rgb it)
    (apply 'color-srgb-to-xyz it)
    (append it (list (or white-point color-d65-xyz)))
    (apply 'color-xyz-to-lab it)))

(defun ct/lab-to-name (lab &optional white-point)
  "Convert LAB color to #HHHHHH with optional lighting assumption WHITE-POINT."
  (->>
    (-snoc lab (or white-point color-d65-xyz))
    (apply 'color-lab-to-xyz)
    (apply 'color-xyz-to-srgb)
    ;; when pulling it out we might die (srgb is not big enough to hold all possible values)
    (-map 'color-clamp)
    (apply 'color-rgb-to-hex)
    (ct/maybe-shorten)))

(defun ct/hsv-to-rgb (H S V)
  ;; Expected values:
  ;; H in radians
  ;; S,V are both 0 to 1
  ;; cf http://peteroupc.github.io/colorgen.html#HSV
  (let* ((pi2 (* pi 2))
          (H (cond
               ((< H 0) (- pi2 (mod (- H) pi2)))
               ((>= H pi2) (mod H pi2))
               (t H)))
          (hue60 (/ (* H 3) pi))
          (hi (floor hue60))
          (f (- hue60 hi))
          (c (* V (- 1 S)))
          (a (* V (- 1 (* S f))))
          (e (* V (- 1 (* S (- 1 f))))))
    (cond
      ((= hi 0) (list V e c))
      ((= hi 1) (list a V c))
      ((= hi 2) (list c V e))
      ((= hi 3) (list c a V))
      ((= hi 4) (list e c V))
      (t (list V c a)))))

;;;
;;; color space functions
;;;

(defun ct/transform-lab (c transform)
  "Work with a color C in the LAB space using function TRANSFORM. Ranges for LAB are 0-100, -100 -> 100, -100 -> 100."
  (->> c
    (ct/name-to-lab)
    (apply transform)
    (apply (lambda (L A B)
             (list
               (ct/clamp L 0 100)
               (ct/clamp A -100 100)
               (ct/clamp B -100 100))))
    (ct/lab-to-name)))

(defun ct/transform-lch (c transform)
  "Perform a transformation in the LAB LCH space. LCH values are {0-100, 0-100, 0-360}."
  ;; color-lab-to-lch returns a form with H in radians.
  ;; we do some hamfisted handling here for a consistent expectation.
  (ct/transform-lab c
    (lambda (L A B)
      (apply 'color-lch-to-lab
        (let ((result (apply transform
                        (append
                          (-take 2 (color-lab-to-lch L A B))
                          (list (radians-to-degrees (third (color-lab-to-lch L A B))))))))
          (append
            ;; (-map (lambda (p) (* 100.0 p)) (-take 2 result))
            (-take 2 result)
            (list (degrees-to-radians (mod (third result) 360.0)))))))))

(defun ct/transform-hsl (c transform)
  "Tweak C in the HSL colorspace. TRANSFORM gets HSL in values {0 to 360,0 to 100,0 to 100}."
  (->> (color-name-to-rgb c)
    (apply 'color-rgb-to-hsl)
    (funcall (lambda (hsl)
               (apply transform
                 (list
                   (* 360.0 (first hsl))
                   (* 100.0 (second hsl))
                   (* 100.0 (third hsl))))))
    ;; from transformed to what color.el expects
    (funcall (lambda (hsl)
               (list
                 (/ (mod (first hsl) 360) 360.0)
                 (/ (second hsl) 100.0)
                 (/ (third hsl) 100.0))))
    (-map 'color-clamp)
    (apply 'color-hsl-to-rgb)
    (apply 'color-rgb-to-hex)
    (ct/maybe-shorten)))

(defun ct/transform-hsv (c transform)
  "Tweak C in the HSV colorspace. TRANSFORM gets HSV in values {0-360,0-100,0-100}."
  (->> (color-name-to-rgb c)
    (apply 'color-rgb-to-hsv)
    (funcall (lambda (hsv)
               (apply transform
                 (list
                   (radians-to-degrees (first hsv))
                   (* 100.0 (second hsv))
                   (* 100.0 (third hsv))))))
    ;; from transformed to what our function expects
    (funcall (lambda (hsv)
               (list
                 (degrees-to-radians (first hsv))
                 (color-clamp (/ (second hsv) 100.0))
                 (color-clamp (/ (third hsv) 100.0)))))
    (apply 'ct/hsv-to-rgb)
    (apply 'color-rgb-to-hex)
    (ct/maybe-shorten)))

(defun ct/transform-hpluv (c transform)
  "Tweak a color in the HPLuv space. P,L range is {0-100}"
  (ct/maybe-shorten
    (apply 'color-rgb-to-hex
      (-map 'color-clamp
        (hsluv-hpluv-to-rgb
          (let ((result (apply transform (-> c ct/shorten hsluv-hex-to-hpluv))))
            (list
              (mod (first result) 360.0)
              (ct/clamp (second result) 0 100)
              (ct/clamp (third result) 0 100))))))))

(defun ct/transform-hsluv (c transform)
  "Tweak a color in the HSLuv space. S,L range is {0-100}"
  (ct/maybe-shorten
    (apply 'color-rgb-to-hex
      (-map 'color-clamp
        (hsluv-hsluv-to-rgb
          (let ((result (apply transform (-> c ct/shorten hsluv-hex-to-hsluv))))
            (list
              (mod (first result) 360.0)
              (ct/clamp (second result) 0 100)
              (ct/clamp (third result) 0 100))))))))

;; individual property tweaks:
(defmacro ct/transform-prop (transform index)
  `(,transform c
     (lambda (&rest args)
       (-replace-at ,index
         (if (functionp func)
           (funcall func (nth ,index args))
           func)
         args))))

(defun ct/transform-hsl-h (c func) (ct/transform-prop ct/transform-hsl 0))
(defun ct/transform-hsl-s (c func) (ct/transform-prop ct/transform-hsl 1))
(defun ct/transform-hsl-l (c func) (ct/transform-prop ct/transform-hsl 2))

(defun ct/transform-hsv-h (c func) (ct/transform-prop ct/transform-hsv 0))
(defun ct/transform-hsv-s (c func) (ct/transform-prop ct/transform-hsv 1))
(defun ct/transform-hsv-v (c func) (ct/transform-prop ct/transform-hsv 2))

(defun ct/transform-hsluv-h (c func) (ct/transform-prop ct/transform-hsluv 0))
(defun ct/transform-hsluv-s (c func) (ct/transform-prop ct/transform-hsluv 1))
(defun ct/transform-hsluv-l (c func) (ct/transform-prop ct/transform-hsluv 2))

(defun ct/transform-hpluv-h (c func) (ct/transform-prop ct/transform-hpluv 0))
(defun ct/transform-hpluv-p (c func) (ct/transform-prop ct/transform-hpluv 1))
(defun ct/transform-hpluv-l (c func) (ct/transform-prop ct/transform-hpluv 2))

(defun ct/transform-lab-l (c func) (ct/transform-prop ct/transform-lab 0))
(defun ct/transform-lab-a (c func) (ct/transform-prop ct/transform-lab 1))
(defun ct/transform-lab-b (c func) (ct/transform-prop ct/transform-lab 2))

(defun ct/transform-lch-l (c func) (ct/transform-prop ct/transform-lch 0))
(defun ct/transform-lch-c (c func) (ct/transform-prop ct/transform-lch 1))
(defun ct/transform-lch-h (c func) (ct/transform-prop ct/transform-lch 2))

(defun ct/getter (c transform getter)
  "Internal function for making a GETTER of C using a TRANSFORM function."
  (let ((return))
    (apply transform
      (list c
        (lambda (&rest props)
          (setq return (funcall getter props))
          props)))
    return))

(defun ct/get-hsl (c) (ct/getter c 'ct/transform-hsl 'identity))
(defun ct/get-hsl-h (c) (ct/getter c 'ct/transform-hsl 'first))
(defun ct/get-hsl-s (c) (ct/getter c 'ct/transform-hsl 'second))
(defun ct/get-hsl-l (c) (ct/getter c 'ct/transform-hsl 'third))

(defun ct/get-hsv (c) (ct/getter c 'ct/transform-hsv 'identity))
(defun ct/get-hsv-h (c) (ct/getter c 'ct/transform-hsv 'first))
(defun ct/get-hsv-s (c) (ct/getter c 'ct/transform-hsv 'second))
(defun ct/get-hsv-v (c) (ct/getter c 'ct/transform-hsv 'third))

(defun ct/get-hsluv (c) (ct/getter c 'ct/transform-hsluv 'identity))
(defun ct/get-hsluv-h (c) (ct/getter c 'ct/transform-hsluv 'first))
(defun ct/get-hsluv-s (c) (ct/getter c 'ct/transform-hsluv 'second))
(defun ct/get-hsluv-l (c) (ct/getter c 'ct/transform-hsluv 'third))

(defun ct/get-hpluv (c) (ct/getter c 'ct/transform-hpluv 'identity))
(defun ct/get-hpluv-h (c) (ct/getter c 'ct/transform-hpluv 'first))
(defun ct/get-hpluv-s (c) (ct/getter c 'ct/transform-hpluv 'second))
(defun ct/get-hpluv-l (c) (ct/getter c 'ct/transform-hpluv 'third))

(defun ct/get-lab (c) (ct/getter c 'ct/transform-lab 'identity))
(defun ct/get-lab-l (c) (ct/getter c 'ct/transform-lab 'first))
(defun ct/get-lab-a (c) (ct/getter c 'ct/transform-lab 'second))
(defun ct/get-lab-b (c) (ct/getter c 'ct/transform-lab 'third))

(defun ct/get-lch (c) (ct/getter c 'ct/transform-lch 'identity))
(defun ct/get-lch-l (c) (ct/getter c 'ct/transform-lch 'first))
(defun ct/get-lch-c (c) (ct/getter c 'ct/transform-lch 'second))
(defun ct/get-lch-h (c) (ct/getter c 'ct/transform-lch 'third))

;; make colors within our normalized transform functions:
(defun ct/make-color-meta (transform properties)
  "Internal function for creating a color using TRANSFORM function forcing PROPERTIES."
  (apply transform
    (list "#cccccc"                     ; throwaway
      (lambda (&rest _) properties))))

(defun ct/make-hsl (H S L) (ct/make-color-meta 'ct/transform-hsl (list H S L)))
(defun ct/make-hsv (H S V) (ct/make-color-meta 'ct/transform-hsv (list H S V)))
(defun ct/make-hsluv (H S L) (ct/make-color-meta 'ct/transform-hsluv (list H S L)))
(defun ct/make-hpluv (H P L) (ct/make-color-meta 'ct/transform-hpluv (list H P L)))
(defun ct/make-lab (L A B) (ct/make-color-meta 'ct/transform-lab (list L A B)))
(defun ct/make-lch (L C H) (ct/make-color-meta 'ct/transform-lch (list L C H)))


(defun ct/rotation-meta (transform c interval)
  (-map (lambda (offset) (funcall transform c (-partial '+ offset)))
    (if (< 0 interval)
      (number-sequence 0 359 interval)
      (number-sequence 360 1 interval))))

(defun ct/rotation-hsl (c interval) (ct/rotation-meta 'ct/transform-hsl-h c interval))
(defun ct/rotation-hsv (c interval) (ct/rotation-meta 'ct/transform-hsv-h c interval))
(defun ct/rotation-hsluv (c interval) (ct/rotation-meta 'ct/transform-hsluv-h c interval))
(defun ct/rotation-hpluv (c interval) (ct/rotation-meta 'ct/transform-hpluv-h c interval))
(defun ct/rotation-lch (c interval) (ct/rotation-meta 'ct/transform-lch-h c interval))

;;;
;;; other color functions
;;;

(defun ct/lab-lighten (c &optional value)
  ;; note: lightening colors is a little more sensitive than darkening them
  ;; the increased default value here reflects that -- so we don't get false
  ;; stops in color iteration
  (ct/transform-lab-l c (-partial '+ (or value 0.7))))

(defun ct/lab-darken (c &optional value)
  (ct/transform-lab-l c (-rpartial '- (or value 0.5))))

(defun ct/pastel (c &optional Smod Vmod)
  "Make a color C more 'pastel' in the hsl space -- optionally change the rate of change with SMOD and VMOD."
  ;; cf https://en.wikipedia.org/wiki/Pastel_(color)
  ;; pastel colors belong to a pale family of colors, which, when described in the HSV color space,
  ;; have high value and low saturation.
  (ct/transform-hsl c
    (lambda (H S L)
      (list
        H
        (* S (or Smod 0.9))
        (* L (or Vmod 1.1))))))

(defun ct/gradient (step start end &optional with-ends)
  "Create a gradient length STEP from START to END, optionally including START and END (toggle: WITH-ENDS)."
  (if with-ends
    `(,start
       ,@(-map
           (lambda (c) (eval `(color-rgb-to-hex ,@c 2)))
           (color-gradient
             (color-name-to-rgb start)
             (color-name-to-rgb end)
             (- step 2)))
       ,end)
    (-map
      (lambda (c) (eval `(color-rgb-to-hex ,@c 2)))
      (color-gradient
        (color-name-to-rgb start)
        (color-name-to-rgb end)
        step))))

(defun ct/luminance-srgb (c)
  "Get the srgb luminance value of C."
  ;; cf https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
  (let ((rgb (-map
               (lambda (part)
                 (if (<= part 0.03928)
                   (/ part 12.92)
                   (expt (/ (+ 0.055 part) 1.055) 2.4)))
               (color-name-to-rgb c))))
    (+
      (* (nth 0 rgb) 0.2126)
      (* (nth 1 rgb) 0.7152)
      (* (nth 2 rgb) 0.0722))))

(defun ct/contrast-ratio (c1 c2)
  "Get the contrast ratio between C1 and C2."
  ;; cf https://peteroupc.github.io/colorgen.html#Contrast_Between_Two_Colors
  (let ((rl1 (ct/luminance-srgb c1))
         (rl2 (ct/luminance-srgb c2)))
    (/ (+ 0.05 (max rl1 rl2))
      (+ 0.05 (min rl1 rl2)))))

(defun ct/lab-change-whitepoint (c w1 w2)
  "Convert a color C wrt white points W1 and W2 through the lab colorspace."
  (ct/lab-to-name (ct/name-to-lab c w1) w2))

(defun ct/name-distance (c1 c2)
  "Get cie-DE2000 distance between C1 and C2."
  ;; note: there are 3 additional optional params to cie-de2000: compensation for
  ;; {lightness,chroma,hue} (all 0.0-1.0)
  ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
  (apply 'color-cie-de2000 (-map 'ct/name-to-lab (list c1 c2))))

(defun ct/is-light-p (c &optional scale)
  "Determine if C is a light color with lightness in the LAB space -- optionally override SCALE comparison value."
  (> (first (ct/name-to-lab c)) (or scale 65)))

(defun ct/greaten (c percent)
  "Make a light color lighter, a dark color darker."
  (ct/shorten
    (if (ct/is-light-p c)
      (color-lighten-name c percent)
      (color-darken-name c percent))))

(defun ct/lessen (c percent)
  "Make a light color darker, a dark color lighter"
  (ct/shorten
    (if (ct/is-light-p c)
      (color-darken-name c percent)
      (color-lighten-name c percent))))

(defun ct/iterations (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect - return all intermediate steps."
  (let ((colors (list start))
         (iterations 0))
    (while (and (not (funcall condition (-last-item colors)))
             (not (string= (funcall op (-last-item colors)) (-last-item colors)))
             (< iterations 10000))
      (setq iterations (+ iterations 1))
      (setq colors (-snoc colors (funcall op (-last-item colors)))))
    colors))

(defun ct/iterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (-last-item (ct/iterations start op condition)))

(defun ct/tint-ratio (c against ratio)
  (ct/iterate c
    (if (ct/is-light-p against)
      'ct/lab-darken
      'ct/lab-lighten)
    (lambda (step) (> (ct/contrast-ratio step against) ratio))))

(provide 'color-tools)
;;; color-tools.el ends here
