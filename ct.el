;;; ct.el --- Color Tools - a color api -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2022 neeasade
;; SPDX-License-Identifier: MIT
;;
;; Version: 0.1
;; Author: neeasade
;; Keywords: convenience color theming rgb hsv hsl cie-lab background
;; URL: https://github.com/neeasade/ct.el
;; Package-Requires: ((emacs "26.1") (dash "2.18.0") (hsluv "1.0.0"))

;;; Commentary:
;; neeasade's color tools for Emacs.

;;; Code:

;; There are 3 sections to this file (IE, you may use these bullets as search text):
;; - helpers to build color space functions
;; - color space functions
;; - other color functions

;; conventions:
;; setting the wrap convention for docstrings at ~100 chars.

(require 'color)
(require 'dash)
(require 'hsluv)
(require 'seq)

;; customization:

(defgroup ct nil
  "Utilities for editing individual colors."
  :group 'ct
  :prefix "ct-")

(defcustom ct-always-shorten t
  "Whether results of color functions should ensure format #HHHHHH rather than #HHHHHHHHHHHH."
  :type 'boolean
  :group 'ct)

;;;
;;; helpers to build color space functions
;;;

(defun ct-clamp (value min max)
  "Make sure VALUE is a number between MIN and MAX inclusive."
  (min max (max min value)))

(defun ct-shorten (c)
  "Optionally transform C #HHHHHHHHHHHH to #HHHHHH."
  (if (= (length c) 7)
    c
    (--> c
      (color-name-to-rgb it)
      `(color-rgb-to-hex ,@ it 2)
      (eval it))))

(defun ct-maybe-shorten (c)
  "Internal function for optionally shortening color C -- see variable 'ct-always-shorten'."
  (if ct-always-shorten
    (ct-shorten c)
    c))

(defun ct--within (value tolerance anchor)
  "Return if a VALUE is within TOLERANCE of ANCHOR."
  (>= (+ anchor tolerance)
    value
    (- anchor tolerance)))

(defun ct--amp-value (v fn arg1 arg1-amp-fn tolerance-fn)
  "Call FN with V and ARG1 and see if V changes.
If V does not change, call ARG1-AMP-FN on ARG1 and call FN with the new ARG1.
Use TOLERANCE-FN to check if ARG1 can be updated further "
  (let* ((next v)
          (iterations 0))
    (while (and (string= next v)
             (funcall tolerance-fn arg1)
             (< iterations 1000))
      ;; (message (format "arg is %s" arg1))
      (setq iterations (+ 1 iterations)
        next (funcall fn v arg1)
        arg1 (funcall arg1-amp-fn arg1)))
    next))

(defun ct-name-to-lab (name &optional white-point)
  "Transform NAME into LAB colorspace with optional lighting assumption WHITE-POINT."
  (--> name
    (color-name-to-rgb it)
    (apply #'color-srgb-to-xyz it)
    (append it (list (or white-point color-d65-xyz)))
    (apply #'color-xyz-to-lab it)))

(defun ct-lab-to-name (lab &optional white-point)
  "Convert LAB color to #HHHHHH with optional lighting assumption WHITE-POINT."
  (->>
    (-snoc lab (or white-point color-d65-xyz))
    (apply #'color-lab-to-xyz)
    (apply #'color-xyz-to-srgb)
    ;; when pulling it out we might die (srgb is not big enough to hold all possible values)
    (-map #'color-clamp)
    (apply #'color-rgb-to-hex)
    (ct-maybe-shorten)))

(defun ct-hsv-to-rgb (H S V)
  "Convert HSV to RGB. Expected values: H: radian, S,V: 0 to 1."
  ;; ref https://peteroupc.github.io/colorgen.html#HSV
  (let* ((pi2 (* float-pi 2))
          (H (cond
               ((< H 0) (- pi2 (mod (- H) pi2)))
               ((>= H pi2) (mod H pi2))
               (t H)))
          (hue60 (/ (* H 3) float-pi))
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

(defun ct-transform-rgb (c transform)
  "Work with a color C in the RGB space using function TRANSFORM. Ranges for RGB are all 0-100."
  (->> c
    (color-name-to-rgb)
    (-map (-partial #'* 100.0))
    (apply transform)
    (-map (-rpartial #'/ 100.0))
    (-map #'color-clamp)
    (apply #'color-rgb-to-hex)
    (ct-maybe-shorten)))

(defun ct-transform-lab (c transform)
  "Work with a color C in the LAB space using function TRANSFORM.
Ranges for LAB are {0-100,-100-100,-100-100}."
  (->> c
    (ct-name-to-lab)
    (apply transform)
    (apply (lambda (L A B)
             (list
               (ct-clamp L 0 100)
               (ct-clamp A -100 100)
               (ct-clamp B -100 100))))
    (ct-lab-to-name)))

(defun ct-transform-lch (c transform)
  "Work with a color C in the LCH space using function TRANSFORM.
Ranges for LCH are {0-100,0-100,0-360}."
  (ct-transform-lab c
    (lambda (&rest lab)
      (->> lab
        (apply #'color-lab-to-lch)
        (apply (lambda (L C H) (list L C (radians-to-degrees H))))
        (apply transform)
        (apply (lambda (L C H) (list L C (degrees-to-radians (mod H 360)))))
        (apply #'color-lch-to-lab)))))

(defun ct-transform-hsl (c transform)
  "Work with a color C in the HSL space using function TRANSFORM.
Ranges for HSL are {0-360,0-100,0-100}."
  (->> c
    (color-name-to-rgb)
    (apply #'color-rgb-to-hsl)
    (apply (lambda (H S L) (list (* 360.0 H) (* 100.0 S) (* 100.0 L))))
    (apply transform)
    (apply (lambda (H S L) (list (/ (mod H 360) 360.0) (/ S 100.0) (/ L 100.0))))
    (apply #'color-hsl-to-rgb)
    (-map #'color-clamp)
    (apply #'color-rgb-to-hex)
    (ct-maybe-shorten)))

(defun ct-transform-hsv (c transform)
  "Work with a color C in the HSV space using function TRANSFORM.
Ranges for HSV are {0-360,0-100,0-100}."
  (->> (color-name-to-rgb c)
    (apply #'color-rgb-to-hsv)
    (funcall (-lambda ((H S V))
               (apply transform
                 (list
                   (radians-to-degrees H)
                   (* 100.0 S)
                   (* 100.0 V)))))
    ;; from transformed to what our function expects
    (funcall (-lambda ((H S V))
               (list
                 (degrees-to-radians H)
                 (color-clamp (/ S 100.0))
                 (color-clamp (/ V 100.0)))))
    (apply #'ct-hsv-to-rgb)
    (apply #'color-rgb-to-hex)
    (ct-maybe-shorten)))

(defun ct-transform-hpluv (c transform)
  "Work with a color C in the HPLUV space using function TRANSFORM.
Ranges for HPLUV are {0-360,0-100,0-100}."
  (ct-maybe-shorten
    (apply #'color-rgb-to-hex
      (-map #'color-clamp
        (hsluv-hpluv-to-rgb
          (-let (((H P L) (apply transform (-> c ct-shorten hsluv-hex-to-hpluv))))
            (list
              (mod H 360.0)
              (ct-clamp P 0 100)
              (ct-clamp L 0 100))))))))

(defun ct-transform-hsluv (c transform)
  "Work with a color C in the HSLUV space using function TRANSFORM.
Ranges for HSLUV are {0-360,0-100,0-100}."
  (ct-maybe-shorten
    (apply #'color-rgb-to-hex
      (-map #'color-clamp
        (hsluv-hsluv-to-rgb
          (let ((result (apply transform (-> c ct-shorten hsluv-hex-to-hsluv))))
            (list
              (mod (-first-item result) 360.0)
              (ct-clamp (-second-item result) 0 100)
              (ct-clamp (-third-item result) 0 100))))))))

(defun ct--colorspace-map (label)
  "Map a LABEL to plist'd utility functions associated with a space. LABEL is one of: rgb hsl hsluv hpluv lch lab hsv."
  (->>
    `(:transform "ct-transform-%s"
       :make "ct-make-%s"
       :get "ct-get-%s"
       :get-1 ,(concat "ct-get-%s-" (string (elt label 0)))
       :get-2 ,(concat  "ct-get-%s-" (string (elt label 1)))
       :get-3 ,(concat "ct-get-%s-" (string (elt label 2))))
    (-partition 2)
    (-map (-lambda ((key name))
            (list key (intern (format name label)))))
    (-flatten)))

(defmacro ct--make-transform-property-functions (colorspace)
  "Build the functions for tweaking individual properties of colors in COLORSPACE."
  (-let* (((&plist :transform transform :get get) (ct--colorspace-map colorspace))
           (result '(progn))
           (collect (lambda (sexp) (setq result (-snoc result sexp)))))

    (funcall collect
      `(defun ,get (c)
         ,(format "Get %s representation of color C." colorspace)
         (let ((return))
           (,transform c
             (lambda (&rest props)
               (setq return props)
               props))
           return)))

    (->> '(0 1 2)
      (-map
        (lambda (index)
          (let* ((prop-name (format "%s-%s" colorspace (substring colorspace index (+ index 1))))
                  (transform-prop-fn (format "ct-transform-%s" prop-name)))
            (funcall collect
              `(defun ,(intern transform-prop-fn) (c func-or-val)
                 ,(format "Transform property %s of C using FUNC-OR-VAL." prop-name)
                 (,transform c
                   (lambda (&rest color-props)
                     (-replace-at ,index
                       (if (functionp func-or-val)
                         (funcall func-or-val (nth ,index color-props))
                         func-or-val)
                       color-props)))))

            (funcall collect
              `(defun ,(intern (format "ct-get-%s" prop-name)) (c)
                 ,(format "Get %s representation of color C." prop-name)
                 (nth ,index (,get c))))

            (funcall collect
              `(defun ,(intern (format "%s-inc" transform-prop-fn)) (c &optional v)
                 ,(format "Increase %s value of C by V (defaults to the minimum amound needed to change C)." prop-name)
                 (if v (,(intern transform-prop-fn) c (-partial #'+ v))
                   (ct--amp-value c
                     (lambda (color amount)
                       (,(intern transform-prop-fn) color (-partial #'+ amount)))
                     0.1 (-partial #'+ 0.1)
                     ;; nb: 30% limit is arbitrary
                     (lambda (arg) (ct--within arg 30 0.1))))))

            (funcall collect
              `(defun ,(intern (format "%s-dec" transform-prop-fn)) (c &optional v)
                 ,(format "Decrease %s value of C by the minimum amound needed to change C." prop-name)
                 (if v (,(intern transform-prop-fn) c (-rpartial #'- v))
                   (ct--amp-value c
                     (lambda (color amount)
                       (,(intern transform-prop-fn) color (-partial #'+ amount)))
                     -0.1 (-rpartial #'- 0.1)
                     ;; nb: 30% limit is arbitrary
                     (lambda (arg) (ct--within arg 30 0.1))))))))))
    result))

(ct--make-transform-property-functions "rgb")
(ct--make-transform-property-functions "hsl")
(ct--make-transform-property-functions "hsv")
(ct--make-transform-property-functions "lch")
(ct--make-transform-property-functions "lab")
(ct--make-transform-property-functions "hpluv")
(ct--make-transform-property-functions "hsluv")

;; make colors within our normalized transform functions:
(defun ct--make-color-meta (transform properties)
  "Internal function for creating a color using TRANSFORM function forcing PROPERTIES."
  (funcall transform "#cccccc" (lambda (&rest _) properties)))

(defun ct-make-rgb (R G B) "Make a color using R*G*B properties." (ct--make-color-meta 'ct-transform-rgb (list R G B)))
(defun ct-make-hsl (H S L) "Make a color using H*S*L properties." (ct--make-color-meta 'ct-transform-hsl (list H S L)))
(defun ct-make-hsv (H S V) "Make a color using H*S*V properties." (ct--make-color-meta 'ct-transform-hsv (list H S V)))
(defun ct-make-hsluv (H S L) "Make a color using H*S*L*uv properties." (ct--make-color-meta 'ct-transform-hsluv (list H S L)))
(defun ct-make-hpluv (H P L) "Make a color using H*P*L*uv properties." (ct--make-color-meta 'ct-transform-hpluv (list H P L)))
(defun ct-make-lab (L A B) "Make a color using L*A*B properties." (ct--make-color-meta 'ct-transform-lab (list L A B)))
(defun ct-make-lch (L C H) "Make a color using L*C*H properties." (ct--make-color-meta 'ct-transform-lch (list L C H)))

(defun ct--rotation-meta (transform c interval)
  "Internal function for managing hue rotation in TRANSFORM starting at C by degree count INTERVAL."
  (-map (lambda (offset) (funcall transform c (-partial '+ offset)))
    (if (< 0 interval)
      ;; todo: should this be range?
      (number-sequence 0 359 interval)
      (number-sequence 360 1 interval))))

(defun ct-rotation-hsl (c interval) "Perform a hue rotation in HSL space starting with color C by INTERVAL degrees." (ct--rotation-meta 'ct-transform-hsl-h c interval))
(defun ct-rotation-hsv (c interval) "Perform a hue rotation in HSV space starting with color C by INTERVAL degrees." (ct--rotation-meta 'ct-transform-hsv-h c interval))
(defun ct-rotation-hsluv (c interval) "Perform a hue rotation in HSLuv space starting with color C by INTERVAL degrees." (ct--rotation-meta 'ct-transform-hsluv-h c interval))
(defun ct-rotation-hpluv (c interval) "Perform a hue rotation in HPLUV space starting with color C by INTERVAL degrees." (ct--rotation-meta 'ct-transform-hpluv-h c interval))
(defun ct-rotation-lch (c interval) "Perform a hue rotation in LCH space starting with color C by INTERVAL degrees." (ct--rotation-meta 'ct-transform-lch-h c interval))

;;;
;;; other color functions
;;;

(defun ct-format-rbga (C &optional opacity)
  "RGBA formatting:
Pass in C and OPACITY 0-100, get a string
representation of C as follows: 'rgba(R, G, B, OPACITY)', where
values RGB are 0-255, and OPACITY is 0-1.0 (default 1.0)."
  (->>
    (ct-get-rgb C)
    (-map (-partial '* (/ 255.0 100)))
    (-map #'round)
    (funcall (lambda (coll) (-snoc coll
                              (/
                                (ct-clamp (or opacity 100) 0 100)
                                100.0))))
    (apply (-partial 'format "rgba(%s, %s, %s, %s)"))))

(defun ct-format-argb (C &optional opacity end)
  "Argb formatting:
Pass in C and OPACITY 0-100, get a string representation of C
as follows: '#AAFFFFFF', where AA is a hex pair for the alpha,
followed by FF times 3 hex pairs for red, green, blue. If END is
truthy, then format will be '#FFFFFFAA'."
  (->>
    (ct-clamp (or opacity 100) 0 100)
    (* (/ 255.0 100))
    (round)
    (format "%02x")
    (funcall (lambda (A)
               (if end
                 (format "#%s%s" (-> C ct-shorten (substring 1)) A)
                 (format "#%s%s" A (-> C ct-shorten (substring 1))))))))

;; sRGB <-> linear RGB conversion
;; https://en.wikipedia.org/wiki/SRGB#The_forward_transformation_(CIE_XYZ_to_sRGB)
;; TODO: compare to 'ct-luminance-srgb' -- the comparison value is different though.
(defun ct--linearize (comp)
  "Convert a color COMPonent value from sRGB to linear RGB.
Component value in 0-100 range."
  (let ((c (/ comp 100.0)))
    (* 100
      (if (<= c 0.04045)
        (/ c 12.92)
        (expt (/ (+ c 0.055) 1.055) 2.4)))))

(defun ct--delinearize (comp)
  "Convert a color COMPonent value from linear RGB to sRGB.
Component value in 0-100 range."
  (let ((c (/ comp 100.0)))
    (* 100
       (if (<= c 0.0031308)
           (* c 12.92)
         (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))))

(defun ct-srgb-to-rgb (c)
  "Convert color C from sRGB color space to linear RGB.
Ranges for sRGB color are all 0-100."
  (ct-transform-rgb c
    (lambda (&rest comps)
      (-map 'ct--linearize comps))))

(defun ct-rgb-to-srgb (c)
  "Convert linear color C to sRGB color space.
Ranges for RGB color are all 0-100."
  (ct-transform-rgb c
    (lambda (&rest comps)
      (-map 'ct--delinearize comps))))

(defalias 'ct-lab-lighten 'ct-transform-lab-l-inc)
(defalias 'ct-lab-darken 'ct-transform-lab-l-dec)

(defun ct-pastel (c &optional Smod Vmod)
  "Make a color C more 'pastel' in the hsluv space -- optionally change the rate of change with SMOD and VMOD."
  ;; ref https://en.wikipedia.org/wiki/Pastel_(color)
  (ct-transform-hsl c
    (lambda (H S L)
      (let ((new-S (- S (or Smod 1)))
             (new-L (+ L (or Vmod 1))))
        (list
          H
          ;; pastel colors have high lightness, low saturation
          ;; arbitrary clamps here
          ;; todo: maybe try and make this smarter (preserve ratio when clamping either s or l
          (ct-clamp new-S 0 40)
          (ct-clamp new-L 60 100))))))

(defun ct-luminance-srgb (c)
  "Get the srgb luminance value of C."
  ;; ref https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
  (let ((rgb (-map
               (lambda (part)
                 (if (<= part
                       0.03928)
                   (/ part 12.92)
                   (expt (/ (+ 0.055 part) 1.055) 2.4)))
               (color-name-to-rgb c))))
    (+
      (* (nth 0 rgb) 0.2126)
      (* (nth 1 rgb) 0.7152)
      (* (nth 2 rgb) 0.0722))))

(defun ct-contrast-ratio (c1 c2)
  "Get the contrast ratio between C1 and C2."
  ;; ref https://peteroupc.github.io/colorgen.html#Contrast_Between_Two_Colors
  (let ((rl1 (ct-luminance-srgb c1))
         (rl2 (ct-luminance-srgb c2)))
    (/ (+ 0.05 (max rl1 rl2))
      (+ 0.05 (min rl1 rl2)))))

(defun ct-lab-change-whitepoint (c w1 w2)
  "Convert a color C wrt white points W1 and W2 through the lab colorspace."
  (ct-lab-to-name (ct-name-to-lab c w1) w2))

(defun ct-distance (c1 c2)
  "Get cie-DE2000 distance between C1 and C2 -- value is 0-100."
  ;; note: there are 3 additional optional params to cie-de2000: compensation for
  ;; {lightness,chroma,hue} (all 0.0-1.0)
  ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
  (apply #'color-cie-de2000 (-map 'ct-name-to-lab (list c1 c2))))

(defun ct-is-light-p (c &optional scale)
  "Determine if C is a light color with lightness in the LAB space.
Optionally override SCALE comparison value."
  (> (-first-item (ct-name-to-lab c)) (or scale 65)))

(defun ct-greaten (c &optional percent)
  "Make a light color C lighter, a dark color C darker (by PERCENT)."
  (ct-maybe-shorten
    (if (ct-is-light-p c)
      (ct-lab-lighten c percent)
      (ct-lab-darken c percent))))

(defun ct-lessen (c &optional percent)
  "Make a light color C darker, a dark color C lighter (by PERCENT)."
  (ct-maybe-shorten
    (if (ct-is-light-p c)
      (ct-lab-darken c percent)
      (ct-lab-lighten c percent))))

(defun ct-iterations (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect - return all intermediate steps."
  (let ((colors (list start))
         (iterations 0))
    (while (and (not (funcall condition (-last-item colors)))
             (not (string= (funcall op (-last-item colors)) (-last-item colors)))
             (< iterations 10000))
      (setq iterations (+ iterations 1)
        colors (-snoc colors (funcall op (-last-item colors)))))
    colors))

(defun ct-iterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (let ((color start)
         (iterations 0))
    (while (and (not (funcall condition color))
             (not (string= (funcall op color) color))
             (< iterations 10000))
      (setq iterations (+ iterations 1)
        color (funcall op color)))
    color))

(defun ct-tint-ratio (c against ratio)
  "Tint a foreground color C against background color AGAINST until contrast RATIO minimum is reached."
  (ct-iterate c
    (if (ct-is-light-p against)
      #'ct-lab-darken
      #'ct-lab-lighten)
    (lambda (step) (> (ct-contrast-ratio step against) ratio))))

(defun ct-mix-opacity (top bottom opacity)
  "Get resulting color of TOP color with OPACITY overlayed against BOTTOM. Opacity is expected to be 0.0-1.0."
  ;; ref https://stackoverflow.com/questions/12228548/finding-equivalent-color-with-opacity
  (seq-let (r1 g1 b1 r2 g2 b2)
    (append (ct-get-rgb top) (ct-get-rgb bottom))
    (ct-make-rgb
      (+ r2 (* (- r1 r2) opacity))
      (+ g2 (* (- g1 g2) opacity))
      (+ b2 (* (- b1 b2) opacity)))))

(defun ct-gradient (step start end &optional with-ends space)
  "Create a gradient from color START to color END with STEP steps.
Optionally include START and END in results using
WITH-ENDS. Optionally choose a colorspace with SPACE (see
'ct--colorspace-map'). Hue-inclusive colorspaces may see mixed
results."
  ;; todo: consider hue
  (-let* (((&plist :make make-color :get get-color) (ct--colorspace-map (or space "rgb")))
           (step (if with-ends (- step 2) step))
           (get-step-vals (lambda (start end)
                            (number-sequence
                              start end
                              (if (> end start)
                                (/ (- end start) (float (+ step 1)))
                                (/ (- start end) (float (+ step 1))))))))
    (->> (-zip-lists
           (funcall get-color start)
           (funcall get-color end))
      (-map (-lambda ((start end))
              (funcall get-step-vals start end)))
      ;; if start and end have REALLY similar values, the range is nil
      ;; cope here
      (funcall (lambda (result)
                 (let ((expected-length (apply 'max (-map 'length result))))
                   (-map-indexed
                     (lambda (i l)
                       (if l l
                         (-repeat expected-length (nth i (funcall get-color start)))))
                     result))))
      (apply #'-zip-lists)
      (-map (-applify make-color))
      (funcall (lambda (result)
                 (if with-ends
                   result
                   (->> result
                     (-drop 1)
                     (-drop-last 1))))))))

(defun ct-mix (c1 c2 &optional space)
  "Mix colors C1 and C2 in SPACE."
  (-second-item (ct-gradient 3 c1 c2 t space)))

(defun ct-average (colors &optional space)
  "Compute the average color from COLORS in space SPACE. See also: 'ct--colorspace-map'."
  (-reduce (lambda (color new)
             (ct-mix color new space))
    colors))

(provide 'ct)
;;; ct.el ends here
