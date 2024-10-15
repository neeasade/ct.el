;;; ct-doc.el --- ct doc generation -*- coding: utf-8; lexical-binding: t -*-

;; todo: can we do inline svg instead of a service?
;;     - <2024-10-11 Fri 11:15> it appears not

;;; Commentary:
;; used when generating documentation for the readme

;;; Code:

(when t
  (defun ct--get-functions ()
    (let (funcs)
      (mapatoms
        (lambda (sym)
          (when (and (fboundp sym)
                  (s-starts-with-p "ct-" (prin1-to-string sym)))
            (push
              (prin1-to-string sym)
              funcs))))
      funcs))

  ;; todo: consider these translation functions (maybe these should be internal/ct--?)
  ;; (ct-clamp 10 0 1)
  ;; (ct-hsv-to-rgb color)
  ;; (ct-lab-to-name color)
  ;; (ct-name-to-lab color)
  ;; (ct-rgb-to-srgb color)
  ;; (ct-srgb-to-rgb color)
  ;; (ct-shorten color)
  ;; (ct-maybe-shorten color)
  ;; (ct-luminance-srgb color)

  (let* ((ct-funcs (ct--get-functions))
          (color "#4fa5e8")
          (color-complement (ct-complement color))
          (plain-color "#bbbbbb"))
    (setq ct--docs
      `(
         ("Color Properties"
           "Functions for seeing properties of colors not necessarily related to a particular color space.")
         ((ct-contrast-ratio ,plain-color ,color)
           (ct-distance ,color ,color-complement)
           (ct-format-argb ,color 80 t)
           (ct-format-rbga ,color 80)
           (ct-light-p ,color))
         ("Color Modification" "Functions for modifying colors in some way potentially unrelated to a specific colorspace")
         ((ct-complement ,color)
           (ct-gradient 5 ,color ,color-complement t)
           (ct-greaten ,color 20)
           (ct-lessen ,color 20)

           (ct-iterate ,color 'ct-edit-hsv-v-inc (lambda (c) (> (ct-distance c ,color) 10)))
           (ct-iterations ,color 'ct-edit-hsv-v-inc (lambda (c) (> (ct-distance c ,color) 10)))

           (ct-lab-change-whitepoint ,color color-d50-xyz color-d55-xyz)

           (ct-mix ,(list 'list color plain-color color-complement))
           (ct-mix-opacity ,color ,plain-color 80)
           (ct-pastel ,color)
           (ct-contrast-min ,color ,plain-color 3)

           (ct-rotation-hpluv ,color 60)
           (ct-rotation-hsl ,color 60)
           (ct-rotation-hsluv ,color 60)
           (ct-rotation-hsv ,color 60)
           (ct-rotation-lch ,color 60)
           )

         ("RGB" "https://notes.neeasade.net/color-spaces.html#h-99356355-d54c-41d8-bc1a-6e14e29f42c8")
         ((ct-make-rgb ,@(ct-get-rgb color))
           (ct-get-rgb ,color)
           (ct-get-rgb-r ,color)
           (ct-get-rgb-g ,color)
           (ct-get-rgb-b ,color)
           (ct-edit-rgb ,color (lambda (R G B) (list R 0 0)))
           (ct-edit-rgb-b ,color (lambda (b) (+ b 50)))
           (ct-edit-rgb-b-dec ,plain-color 10)
           (ct-edit-rgb-b-inc ,plain-color)
           (ct-edit-rgb-g ,color 100)
           (ct-edit-rgb-g-dec ,plain-color 10)
           (ct-edit-rgb-g-inc ,plain-color)
           (ct-edit-rgb-r ,color 100)
           (ct-edit-rgb-r-dec ,plain-color 10)
           (ct-edit-rgb-r-inc ,plain-color))

         ("LAB" "https://notes.neeasade.net/color-spaces.html#h-9d5a1a9a-75d3-48f5-bf00-85332d9b023e")
         ((ct-make-lab ,@(ct-get-lab color))
           (ct-get-lab ,color)
           (ct-get-lab-l ,color)
           (ct-get-lab-b ,color)
           (ct-get-lab-a ,color)
           (ct-edit-lab ,color (lambda (L A B) (list L -100 -100)))
           (ct-edit-lab-a ,color (lambda (a) (- a 20)))
           (ct-edit-lab-a-dec ,color 20)
           (ct-edit-lab-a-inc ,color 20)
           (ct-edit-lab-b ,color 100)
           (ct-edit-lab-b-dec ,color 20)
           (ct-edit-lab-b-inc ,color 20)
           (ct-edit-lab-l ,color 0)
           (ct-edit-lab-l-dec ,color)
           (ct-edit-lab-l-inc ,color))

         ("HSL" "https://notes.neeasade.net/color-spaces.html#h-43869bc7-a7d1-410f-9341-521974751dac")
         ((ct-make-hsl ,@(ct-get-hsl color))
           (ct-get-hsl ,color)
           (ct-get-hsl-s ,color)
           (ct-get-hsl-l ,color)
           (ct-get-hsl-h ,color)
           (ct-edit-hsl ,color (lambda (H S L) (list (+ H 60) 100 L)))
           (ct-edit-hsl-h ,color (lambda (H) (+ H 60)))
           (ct-edit-hsl-h-dec ,color)
           (ct-edit-hsl-h-inc ,color)
           (ct-edit-hsl-l ,color 0)
           (ct-edit-hsl-l-dec ,color)
           (ct-edit-hsl-l-inc ,color)
           (ct-edit-hsl-s ,color 100)
           (ct-edit-hsl-s-dec ,color)
           (ct-edit-hsl-s-inc ,color))

         ("HSLuv" "https://notes.neeasade.net/color-spaces.html#h-c147b84d-d95b-4d2d-8426-2f96529a8428")
         ((ct-make-hsluv ,@(ct-get-hsluv color))
           (ct-get-hsluv ,color)
           (ct-get-hsluv-s ,color)
           (ct-get-hsluv-l ,color)
           (ct-get-hsluv-h ,color)
           (ct-edit-hsluv ,color (lambda (H S L) (list (+ H 60) 100 L)))
           (ct-edit-hsluv-h ,color (lambda (H) (+ H 60)))
           (ct-edit-hsluv-h-dec ,color)
           (ct-edit-hsluv-h-inc ,color)
           (ct-edit-hsluv-l ,color 0)
           (ct-edit-hsluv-l-dec ,color)
           (ct-edit-hsluv-l-inc ,color)
           (ct-edit-hsluv-s ,color 100)
           (ct-edit-hsluv-s-dec ,color)
           (ct-edit-hsluv-s-inc ,color))

         ("LCH" "https://notes.neeasade.net/color-spaces.html#h-c4f93e1f-4fa6-4ebc-99c1-18b6de0ef413")
         ((ct-make-lch ,@(ct-get-lch color))
           (ct-get-lch ,color)
           (ct-get-lch-l ,color)
           (ct-get-lch-h ,color)
           (ct-get-lch-c ,color)
           (ct-edit-lch ,color (lambda (L C H) (list L 100 (+ H 90))))
           (ct-edit-lch-c ,color 100)
           (ct-edit-lch-c-dec ,color)
           (ct-edit-lch-c-inc ,color)
           (ct-edit-lch-h ,color (lambda (H) (+ H 90)))
           (ct-edit-lch-h-dec ,color)
           (ct-edit-lch-h-inc ,color)
           (ct-edit-lch-l ,color 100)
           (ct-edit-lch-l-dec ,color)
           (ct-edit-lch-l-inc ,color))

         ("HSV" "https://en.wikipedia.org/wiki/HSL_and_HSV")
         ((ct-make-hsv ,@(ct-get-hsv color))
           (ct-get-hsv ,color)
           (ct-get-hsv-v ,color)
           (ct-get-hsv-s ,color)
           (ct-get-hsv-h ,color)
           (ct-edit-hsv ,color (lambda (H S V) (list H 20 100)))
           (ct-edit-hsv-h ,color (-partial #'+ 30))
           (ct-edit-hsv-h-dec ,color)
           (ct-edit-hsv-h-inc ,color)
           (ct-edit-hsv-s ,color 20)
           (ct-edit-hsv-s-dec ,color)
           (ct-edit-hsv-s-inc ,color)
           (ct-edit-hsv-v ,color 100)
           (ct-edit-hsv-v-dec ,color)
           (ct-edit-hsv-v-inc ,color))

         ("HPLUV" "https://ajalt.github.io/colormath/api/colormath/com.github.ajalt.colormath.model/-h-p-luv/index.html")
         ((ct-make-hpluv ,@(ct-get-hpluv color))
           (ct-get-hpluv ,color)
           (ct-get-hpluv-p ,color)
           (ct-get-hpluv-l ,color)
           (ct-get-hpluv-h ,color)
           (ct-edit-hpluv ,color (lambda (H P L) (list H 100 L)))
           (ct-edit-hpluv-h ,color 0)
           (ct-edit-hpluv-h-dec ,color)
           (ct-edit-hpluv-h-inc ,color)
           (ct-edit-hpluv-l ,color 100)
           (ct-edit-hpluv-l-dec ,color)
           (ct-edit-hpluv-l-inc ,color)
           (ct-edit-hpluv-p ,color 100)
           (ct-edit-hpluv-p-dec ,color)
           (ct-edit-hpluv-p-inc ,color)))))

  (defun ct--generate-toc ()
    (->> ct--docs
      (-partition 2)
      (-map (-lambda (((heading desc) examples))
              ;; example part/full:
              (format "*** %s\n%s"
                heading
                (->> examples
                  (-map 'first)
                  (-map 'ct--sym-to-toc)
                  (s-join "\n")
                  ))))
      (s-join "\n")))

  (defun ct--generate-contents ()
    (->> ct--docs
      (-partition 2)
      (-map (-lambda (((heading desc) examples))
              ;; example part/full:
              (format "** %s\n%s\n%s"
                heading
                desc
                (s-join "\n"
                  (-map
                    (lambda (example)
                      (-let* ((result (eval example))
                               (match-colors (ct--get-colors example))
                               (result-colors (ct--get-colors result))

                               (helpful--signature 'ct-rotation-lch)

                               ((name . args) (read (helpful--signature (first example))))
                               (docstring (helpful--docstring (first example) t))
                               (show-quote-p (not (and (or (s-ends-with-p "-inc" (prin1-to-string (first example)))
                                                         (s-ends-with-p "-dec" (prin1-to-string (first example)) ))
                                                    (= (length example) 2)
                                                    ))))
                        (format "**** %s ~%s~\n%s\n%s"
                          name args docstring
                          (format
                            (if show-quote-p
                              "#+BEGIN_src elisp
%s ;; => %s
#+END_src
#+BEGIN_quote
%s → %s
#+END_quote"
                              "#+BEGIN_src elisp
%s ;; => %s
#+END_src"
                              )


                            (prin1-to-string example)
                            (prin1-to-string result)
                            (s-join "," match-colors)
                            (if result-colors
                              (s-join "," result-colors)
                              result)
                            )))
                      )
                    examples)
                  )
                )))
      (s-join "\n")))

  (defun ct--get-preview (n)
    (when (and (stringp n)
            (= (length n) 7))
      (format "[[https://via.placeholder.com/16/%s/000000.png?text=+]]"
        (substring n 1))))

  (defun ct--get-colors (form)
    (-keep 'ct--get-preview
      (-list form)))

  (defun ct--sym-to-toc (sym)
    (-let (((name . args) (read (helpful--signature sym))))
      (format "- [[#%s][%s]] ~%s~"
        (->> (list
               (prin1-to-string name)
               (prin1-to-string args))
          (s-join " " )
          (s-downcase)
          (s-replace "&" "")
          (s-replace "(" "")
          (s-replace ")" "")
          (s-replace " " "-"))
        ;; name
        name args)))

  ;; off the rails
  (f-write (s-replace "{{replace-me}}"
             (s-join "\n" (list
                            (ct--generate-toc)
                            (ct--generate-contents)))
             (f-read "readme.org"))
    'utf-8 "../readme.org")
  )

(provide 'ct-doc)
;;; ct-doc.el ends here
