(TeX-add-style-hook
 "whoplaysrandom"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "margin=10pt" "font=small" "labelfont=bf") ("natbib" "round" "sort" "comma") ("datetime" "yyyymmdd" "hhmmss")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "../plots_tables/ginitable"
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
    "Placeholder")
   (LaTeX-add-array-newcolumntypes
    "L"
    "C"
    "R")
   (LaTeX-add-mathtools-DeclarePairedDelimiters
    '("abs" "")
    '("norm" "")))
 :latex)

