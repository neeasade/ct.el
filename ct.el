;;; ct.el --- Color Tools - a color api -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2024 neeasade
;; SPDX-License-Identifier: MIT
;;
;; Version: 0.2
;; Author: neeasade
;; Keywords: convenience color theming rgb hsv hsl cielab background
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

(defcustom ct-verbose-p nil
  "Enable logs. Logs will be prefixed with 'ct: '."
  :type 'boolean
  :group 'ct)

(defcustom ct-interactive-step-interval 3
  "Interval to use for the ct-point-* interactive functions.
If set to nil the smallest amount needed to affect a change is used."
  :type '(restricted-sexp :match-alternatives (integerp 'nil))
  :group 'ct)

;;;
;;; helpers to build color space functions
;;;

(defun ct-clamp (value &optional min max)
  "Make sure VALUE is a number between MIN and MAX inclusive.

MIN and MAX default to 0 and 100."
  ;; will there be any issues here with amp function?
  (let ((min (or min 0.0))
         (max (or max 100.0))
         (result (min max (max min value))))
    (when (and ct-verbose-p
            (not (= result value))
            (< 0.001 (abs (- result value))))
      (message "ct: ct-clamped %s -> %s" value result))
    result))

(defun ct-shorten (c)
  "Optionally shorten C #HHHHHHHHHHHH to #HHHHHH."
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

;; utility function from: https://github.com/emacsfodder/kurecolor/blob/d17a77d9210b3e7b8141d03c04d1898bcab2b876/kurecolor.el#L201-L220
(defun ct--replace-current (fn &rest args)
  "Get the current unspaced string at point.
Replace with the return value of the function FN with ARGS"
  (let (pos1 pos2 replacement excerpt change)
    (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (when (looking-at "#") (forward-char 1))
        (setq pos1 (car (bounds-of-thing-at-point 'symbol))
          pos2 (cdr (bounds-of-thing-at-point 'symbol)))
        (when (> pos1 0)
          (setq pos1 (- pos1 1)))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (if args
      (progn (setq change (car args))
        (setq replacement (funcall fn excerpt change)))
      ;; no args
      (setq replacement (funcall fn excerpt)))
    (delete-region pos1 pos2)
    (insert replacement)))

(defun ct--within (value tolerance anchor)
  "Return if a VALUE is within TOLERANCE of ANCHOR."
  (>= (+ anchor tolerance)
    value
    (- anchor tolerance)))

(defun ct--amp-value (v fn arg1 arg1-amp-fn tolerance-fn)
  "Call FN with V and ARG1 and see if V is affected.
If V does not change, call ARG1-AMP-FN on ARG1 and call FN with the new ARG1.
Use TOLERANCE-FN to check if ARG1 can be updated further."
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

(defun ct-name-to-cielab (name &optional white-point)
  "Transform NAME into LAB colorspace with optional lighting assumption WHITE-POINT."
  (--> name
    (color-name-to-rgb it)
    (apply #'color-srgb-to-xyz it)
    (append it (list (or white-point color-d65-xyz)))
    (apply #'color-xyz-to-lab it)))


(defun ct-cielab-to-name (lab &optional white-point)
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

(defun ct-edit-rgb (c transform)
  "Work with a color C in the RGB space using function TRANSFORM. Ranges for RGB are all 0-100."
  (->> c
    (color-name-to-rgb)
    (--map (* it 100.0))
    (apply transform)
    (-map #'ct-clamp)
    (--map (/ it 100.0))
    (apply #'color-rgb-to-hex)
    (ct-maybe-shorten)))

(defun ct-edit-cielab (c transform)
  "Work with a color C in the LAB space using function TRANSFORM.
Ranges for LAB are {0-100,-100-100,-100-100}."
  (->> c
    (ct-name-to-cielab)
    (apply transform)
    (apply (lambda (L A B)
             (list
               (ct-clamp L)
               (ct-clamp A -100 100)
               (ct-clamp B -100 100))))
    (ct-cielab-to-name)))

(defun ct-edit-lch (c transform)
  "Work with a color C in the LCH space using function TRANSFORM.
Ranges for LCH are {0-100,0-100,0-360}."
  (ct-edit-cielab c
    (lambda (&rest lab)
      (->> lab
        (apply #'color-cielab-to-lch)
        (apply (lambda (L C H) (list L C (radians-to-degrees H))))
        (apply transform)
        (apply (lambda (L C H) (list L C (degrees-to-radians (mod H 360)))))
        (apply #'color-lch-to-cielab)))))

(defun ct-edit-hsl (c transform)
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

(defun ct-edit-hsv (c transform)
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

(defun ct-edit-hpluv (c transform)
  "Work with a color C in the HPLUV space using function TRANSFORM.
Ranges for HPLUV are {0-360,0-100,0-100}."
  (ct-maybe-shorten
    (apply #'color-rgb-to-hex
      (-map #'color-clamp
        (hsluv-hpluv-to-rgb
          (-let (((H P L) (apply transform (-> c ct-shorten hsluv-hex-to-hpluv))))
            (list
              (mod H 360.0)
              (ct-clamp P)
              (ct-clamp L))))))))

(defun ct-edit-hsluv (c transform)
  "Work with a color C in the HSLUV space using function TRANSFORM.
Ranges for HSLUV are {0-360,0-100,0-100}."
  (ct-maybe-shorten
    (apply #'color-rgb-to-hex
      (-map #'color-clamp
        (hsluv-hsluv-to-rgb
          (let ((result (apply transform (-> c ct-shorten hsluv-hex-to-hsluv))))
            (list
              (mod (-first-item result) 360.0)
              (ct-clamp (-second-item result))
              (ct-clamp (-third-item result)))))))))

(eval-and-compile
  (defun ct--colorspace-map (&optional label)
    "Map a quoted colorspace LABEL to a plist with utility functions associated with a space. LABEL is one of: rgb hsl hsluv hpluv lch lab hsv. Defaults to \"rgb\"."
    (let ((label (or label "rgb")))
      (->> '(:transform "ct-edit-%s"
              :make "ct-make-%s"
              :get "ct-get-%s")
        (-partition 2)
        (-mapcat (-lambda ((key name))
                   (list key (intern (format name label)))))))))

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
          (let* ((prop-single (let ((from (cond
                                            ((string= colorspace "oklab") (+ 2 index))
                                            ((string= colorspace "cielab") (+ 3 index))
                                            (t index))))
                                (substring colorspace from (1+ from))))
                  (prop-name (format "%s-%s" colorspace prop-single))
                  (transform-prop-fn (format "ct-edit-%s" prop-name)))
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
                 ,(format "Increase %s value of C by V (defaults to the minimum amount needed to change C)." prop-name)
                 (if v (,(intern transform-prop-fn) c (-partial #'+ v))
                   (ct--amp-value c
                     (lambda (color amount)
                       (,(intern transform-prop-fn) color (-partial #'+ amount)))
                     0.1 (-partial #'+ 0.1)
                     ;; nb: 30% limit is arbitrary
                     (lambda (arg) (ct--within arg 30 0.1))))))

            (funcall collect
              `(defun ,(intern (format "%s-dec" transform-prop-fn)) (c &optional v)
                 ,(format "Decrease %s value of C by V (defaults to the minimum amount needed to change C)." prop-name)
                 (if v (,(intern transform-prop-fn) c (-rpartial #'- v))
                   (ct--amp-value c
                     (lambda (color amount)
                       (,(intern transform-prop-fn) color (-partial #'+ amount)))
                     -0.1 (-rpartial #'- 0.1)
                     ;; nb: 30% limit is arbitrary
                     (lambda (arg) (ct--within arg 30 0.1))))))

            (funcall collect
              `(defun ,(intern (format "ct-point-%s-inc" prop-name)) ()
                 ,(format "Change color at point by invoking (%s-inc ct-interactive-step-interval)" transform-prop-fn)
                 (interactive)
                 (ct--replace-current ',(intern (format "%s-inc" transform-prop-fn)) ct-interactive-step-interval)))

            (funcall collect
              `(defun ,(intern (format "ct-point-%s-dec" prop-name)) ()
                 ,(format "Change color at point by invoking (%s-dec ct-interactive-step-interval)" transform-prop-fn)
                 (interactive)
                 (ct--replace-current ',(intern (format "%s-dec" transform-prop-fn)) ct-interactive-step-interval)))

            (when (string= prop-single "h")
              (funcall collect
                `(defun ,(intern (format "ct-rotation-%s" colorspace)) (c interval)
                   ,(format "Perform a hue rotation in %s space starting with color C by INTERVAL degrees." colorspace)
                   (-map (-partial #',(intern (format "%s-inc" transform-prop-fn)) c)
                     (-iota (abs (/ 360 interval))
                       0 interval))))))))) result))

(ct--make-transform-property-functions "rgb")
(ct--make-transform-property-functions "hsl")
(ct--make-transform-property-functions "hsv")
(ct--make-transform-property-functions "lch")
(ct--make-transform-property-functions "cielab")
(ct--make-transform-property-functions "hpluv")
(ct--make-transform-property-functions "hsluv")

;; make colors within our normalized transform functions:
(defmacro ct--make-color-meta (transform properties)
  "Internal macro for creating a color using TRANSFORM function forcing PROPERTIES."
  `(,transform "#cccccc" (lambda (&rest _) ,properties)))

(defun ct-make-rgb (R G B) "Make a color using R*G*B properties." (ct--make-color-meta ct-edit-rgb (list R G B)))
(defun ct-make-hsl (H S L) "Make a color using H*S*L properties." (ct--make-color-meta ct-edit-hsl (list H S L)))
(defun ct-make-hsv (H S V) "Make a color using H*S*V properties." (ct--make-color-meta ct-edit-hsv (list H S V)))
(defun ct-make-hsluv (H S L) "Make a color using H*S*L*uv properties." (ct--make-color-meta ct-edit-hsluv (list H S L)))
(defun ct-make-hpluv (H P L) "Make a color using H*P*L*uv properties." (ct--make-color-meta ct-edit-hpluv (list H P L)))
(defun ct-make-cielab (L A B) "Make a color using L*A*B properties." (ct--make-color-meta ct-edit-cielab (list L A B)))
(defun ct-make-lch (L C H) "Make a color using L*C*H properties." (ct--make-color-meta ct-edit-lch (list L C H)))

;;;
;;; other color functions
;;;

(defun ct-format-rbga (C &optional opacity)
  "RGBA formatting:
Pass in C and OPACITY 0-100, get a string
representation of C as follows: 'rgba(R, G, B, OPACITY)', where
values RGB are 0-255, and OPACITY is 0-1.0 (default 1.0)."
  (->> (ct-get-rgb C)
    (-map (-partial '* (/ 255.0 100)))
    (-map #'round)
    (funcall (lambda (coll) (-snoc coll
                              (/
                                (ct-clamp (or opacity 100))
                                100.0))))
    (apply (-partial 'format "rgba(%s, %s, %s, %s)"))))

(defun ct-format-argb (C &optional opacity end)
  "Argb formatting:
Pass in C and OPACITY 0-100, get a string representation of C
as follows: '#AAFFFFFF', where AA is a hex pair for the alpha,
followed by FF times 3 hex pairs for red, green, blue. If END is
truthy, then format will be '#FFFFFFAA'."
  (->> (ct-clamp (or opacity 100))
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
  "Convert a color COMP*onent value from sRGB to linear RGB.
Component value in 0-100 range."
  (let ((c (/ comp 100.0)))
    (* 100
      (if (<= c 0.04045)
        (/ c 12.92)
        (expt (/ (+ c 0.055) 1.055) 2.4)))))

(defun ct--delinearize (comp)
  "Convert a color COMP*onent value from linear RGB to sRGB.
Component value in 0-100 range."
  (let ((c (/ comp 100.0)))
    (* 100
       (if (<= c 0.0031308)
           (* c 12.92)
         (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))))

(defun ct-srgb-to-rgb (c)
  "Convert color C from sRGB color space to linear RGB.
Ranges for sRGB color are all 0-100."
  (ct-edit-rgb c
    (lambda (&rest comps)
      (-map 'ct--linearize comps))))

(defun ct-rgb-to-srgb (c)
  "Convert linear color C to sRGB color space.
Ranges for RGB color are all 0-100."
  (ct-edit-rgb c
    (lambda (&rest comps)
      (-map 'ct--delinearize comps))))

(defun ct-pastel (c &optional Smod Vmod)
  "Make a color C more 'pastel' in the hsluv space -- optionally change the rate of change with SMOD and VMOD."
  ;; ref https://en.wikipedia.org/wiki/Pastel_(color)
  (ct-edit-hsl c
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

(defun ct-cielab-change-whitepoint (c w1 w2)
  "Convert a color C wrt white points W1 and W2 through the lab colorspace."
  (ct-cielab-to-name (ct-name-to-cielab c w1) w2))

(defun ct-distance (c1 c2)
  "Get cie-DE2000 distance between C1 and C2 -- value is 0-100."
  ;; note: there are 3 additional optional params to cie-de2000: compensation for
  ;; {lightness,chroma,hue} (all 0.0-1.0)
  ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
  (apply #'color-cie-de2000 (-map 'ct-name-to-cielab (list c1 c2))))

(defun ct-light-p (c &optional scale)
  "Determine if C is a light color with lightness in the LAB space.
Optionally override SCALE comparison value."
  ;; nb: 65 here is arbitrary
  (> (ct-get-cielab-l c) (or scale 65)))

(defun ct-iterations (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect - return all intermediate parts."
  (let ((colors (list start))
         (iterations 0))
    (while (and (not (funcall condition (-last-item colors)))
             (not (string= (funcall op (-last-item colors)) (-last-item colors)))
             (< iterations 10000))
      (setq iterations (+ iterations 1)
        colors (-snoc colors (funcall op (-last-item colors)))))
    colors))

(defun ct-iterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect.

CONDITION is a function that takes the current color value being iterated."
  (let ((color start)
         (iterations 0))
    (while (and (not (funcall condition color))
             (not (string= (funcall op color) color))
             (< iterations 10000))
      (setq iterations (+ iterations 1)
        color (funcall op color)))
    color))

(defmacro ct-aiterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect.

This is an anaphoric version of ct-iterations - the current color value is bound
to 'C', and the START color is bound to C0."
  `(ct-iterate ,start ,op
     (lambda (C) (let ((C0 ,start)) ,condition))))

(defmacro ct-aiterations (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect - return all intermediate parts.

This is an anaphoric version of ct-iterations - the current color value is bound
to 'C', and the START color is bound to C0."
  `(ct-iterations ,start ,op
     (lambda (C) (let ((C0 ,start)) ,condition))))

(defun ct-contrast-min (foreground background contrast-ratio &optional color-property)
  "Edit FOREGROUND to have a minimum CONTRAST-RATIO on BACKGROUND.

Optionally specify the COLOR-PROPERTY used to tweak foreground (default 'lab-l)"
  (-let* ((color-property (or color-property 'lab-l))
           (darken-fn (intern (format "ct-edit-%s-dec" color-property)))
           (lighten-fn (intern (format "ct-edit-%s-inc" color-property))))
    (ct-aiterate foreground
      (if (ct-light-p background) darken-fn lighten-fn)
      (> (ct-contrast-ratio C background) contrast-ratio))))

(defun ct-contrast-max (foreground background contrast-ratio &optional color-property)
  "Edit FOREGROUND to have a maximum CONTRAST-RATIO on BACKGROUND.

Optionally specify the COLOR-PROPERTY used to tweak foreground (default 'lab-l)"
  (-let* ((color-property (or color-property 'lab-l))
           (darken-fn (intern (format "ct-edit-%s-dec" color-property)))
           (lighten-fn (intern (format "ct-edit-%s-inc" color-property))))
    (ct-aiterate foreground
      (if (ct-light-p background) lighten-fn darken-fn)
      (< (ct-contrast-ratio C background) contrast-ratio))))

(defun ct-contrast-clamp (foreground background contrast-ratio &optional color-property)
  "Conform FOREGROUND to be CONTRAST-RATIO against BACKGROUND.

Optionally specify the COLOR-PROPERTY used to tweak foreground (default 'lab-l)"
  (if (< contrast-ratio (ct-contrast-ratio foreground background))
    (ct-contrast-max foreground background contrast-ratio color-property)
    (ct-contrast-min foreground background contrast-ratio color-property)))

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
  "Create a gradient from color START to color END in STEP parts.
Optionally include START and END in results using
WITH-ENDS. Optionally choose a colorspace with SPACE (see
'ct--colorspace-map'). Hue-inclusive colorspaces may see mixed
results."
  ;; todo: consider hue
  (-let* (((&plist :make make-color :get get-color) (ct--colorspace-map space))
           (step (if with-ends (- step 2) step))
           (get-step-vals (lambda (start end)
                            (let ((step-amount (if (> end start)
                                                 (/ (- end start) (float (+ 1 step)))
                                                 (- (/ (- start end) (float (+ 1 step)))))))
                              (-iota step (+ start step-amount)
                                step-amount)))))
    (->> (-zip-lists
           (funcall get-color start)
           (funcall get-color end))
      (-map (-lambda ((start end))
              (funcall get-step-vals start end)))
      (apply #'-zip-lists)
      (-map (-applify make-color))
      (funcall (lambda (result)
                 (if with-ends
                   `(,start ,@result ,end)
                   result))))))

(defun ct-mix (colors &optional space)
  "Mix COLORS in space SPACE. See also: 'ct--colorspace-map'."
  (-reduce (lambda (color new)
             (-second-item (ct-gradient 3 color new t space)))
    colors))

(defun ct-complement (c)
  "Return a complement color of C in the HSLUV space."
  (ct-edit-hsluv-h-inc c 180))

(defun ct-greaten (c &optional percent)
  "Make a light color C lighter, a dark color C darker (by PERCENT)."
  (ct-edit-cielab-l-inc c
    (* percent (if (ct-light-p c) 1 -1))))

(defun ct-lessen (c &optional percent)
  "Make a light color C darker, a dark color C lighter (by PERCENT)."
  (ct-edit-cielab-l-inc c
    (* percent (if (ct-light-p c) -1 1))))

(define-obsolete-function-alias 'ct-name-distance 'ct-distance "2022-06-03")
(define-obsolete-function-alias 'ct-lab-lighten 'ct-edit-cielab-l-inc "2022-06-03")
(define-obsolete-function-alias 'ct-lab-darken 'ct-edit-cielab-l-dec "2022-06-03")
(define-obsolete-function-alias 'ct-is-light-p 'ct-light-p "2022-06-03")
(define-obsolete-function-alias 'ct-tint-ratio 'ct-contrast-min "2023-05-18")
(define-obsolete-function-alias 'ct-lab-to-name 'ct-cielab-to-name "2024-04-13")
(define-obsolete-function-alias 'ct-name-to-lab 'ct-name-to-cielab "2024-04-13")
(define-obsolete-function-alias 'ct-make-lab 'ct-make-cielab "2024-04-13")
(define-obsolete-function-alias 'ct-get-lab 'ct-get-cielab "2024-04-13")
(define-obsolete-function-alias 'ct-get-lab-l 'ct-get-cielab-l "2024-04-13")
(define-obsolete-function-alias 'ct-get-lab-b 'ct-get-cielab-b "2024-04-13")
(define-obsolete-function-alias 'ct-get-lab-a 'ct-get-cielab-a "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab 'ct-edit-cielab "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-a 'ct-edit-cielab-a "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-a-dec 'ct-edit-cielab-a-dec "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-a-inc 'ct-edit-cielab-a-inc "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-b 'ct-edit-cielab-b "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-b-dec 'ct-edit-cielab-b-dec "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-b-inc 'ct-edit-cielab-b-inc "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-l 'ct-edit-cielab-l "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-l-dec 'ct-edit-cielab-l-dec "2024-04-13")
(define-obsolete-function-alias 'ct-edit-lab-l-inc 'ct-edit-cielab-l-inc "2024-04-13")
(define-obsolete-function-alias 'ct-lab-change-whitepoint 'ct-cielab-change-whitepoint "2024-04-13")

(provide 'ct)
;;; ct.el ends here
