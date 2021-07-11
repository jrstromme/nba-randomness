(TeX-add-style-hook
 "specifications"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "margin=10pt" "font=small" "labelfont=bf") ("natbib" "round" "sort" "comma") ("datetime" "yyyymmdd" "hhmmss")))
   (TeX-run-style-hooks
    "latex2e"
    "../ginitable"
    "article"
    "art11"
    "amsfonts"
    "amsmath"
    "bm"
    "amssymb"
    "array"
    "caption"
    "color"
    "comment"
    "enumitem"
    "fancyhdr"
    "footmisc"
    "fullpage"
    "geometry"
    "graphicx"
    "hyperref"
    "mathtools"
    "multirow"
    "natbib"
    "pdflscape"
    "pdfpages"
    "subfigure"
    "subfloat"
    "ulem"
    "datetime")
   (TeX-add-symbols
    "argmin"
    "argmax"
    "newline")
   (LaTeX-add-labels
    "fig:randomgini")
   (LaTeX-add-bibliographies
    "Placeholder"))
 :latex)

