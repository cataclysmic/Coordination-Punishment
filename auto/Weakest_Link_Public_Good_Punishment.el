(TeX-add-style-hook
 "Weakest_Link_Public_Good_Punishment"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("hyperref" "colorlinks=true" "linkcolor=hcolor" "citecolor=hcolor") ("ulem" "normalem") ("mathdesign" "bitstream-charter") ("xcolor" "table") ("biblatex-chicago" "authordate" "numbermonth=false" "eprint=false" "doi=false" "natbib" "backend=biber" "url=false" "isbn=false")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "amssymb"
    "amsthm"
    "amsmath"
    "a4wide"
    "inputenc"
    "hyperref"
    "ulem"
    "mathdesign"
    "setspace"
    "graphicx"
    "xcolor"
    "booktabs"
    "dcolumn"
    "floatrow"
    "multirow"
    "rotating"
    "pdfpages"
    "titling"
    "marvosym"
    "caption"
    "todonotes"
    "biblatex-chicago")
   (TeX-add-symbols
    '("SK" 1)
    '("CT" 1)
    '("FA" 1)
    '("mro" 2)
    '("mco" 2)
    "ra"
    "fns"
    "tw"
    "nc"
    "nl"
    "lpun"
    "lnpun"
    "lapun"
    "ltotal")
   (LaTeX-add-labels
    "eq:con1"
    "fig:rpstype"
    "fig:wpstype"
    "fig:punXtab"
    "tab:avgPun"
    "eq:cctypes"
    "fig:cctypes"
    "tab:ccpunRPS"
    "tab:ccpunWPS"
    "fig:groupoutRP"
    "tab:coopRP"
    "fig:groupoutWP"
    "fig:equibub"
    "tab:equiWPS"
    "ap:sec:triple"
    "ap:sec:con1pay"
    "ap:tab:con1pay"
    "ap:tab:sdiffpay"
    "ap:tab:sdifftype"
    "ap:tab:avgPun"
    "fig:groupout"
    "tab: "
    "tab:linear"
    "tab:piecewise"
    "fig:sdiff"
    "eq:pun"
    "tab:triplet")
   (LaTeX-add-bibliographies
    "lit")
   (LaTeX-add-xcolor-definecolors
    "hcolor"
    "gray")
   (LaTeX-add-array-newcolumntypes
    "d"
    "b"
    "m"))
 :latex)

