setwd(file.path(getwd(), "lisa"))
# Render Book
bookdown::render_book(
  "index.Rmd", 
  bookdown::pdf_document2(
    toc = FALSE,
    documentclass = "article",
    latex_engine = "xelatex",
    citation_package = "natbib",
    keep_tex = TRUE,
    fontsize = "12pt",
    includes = rmarkdown::includes(in_header = "preamble.tex"),
    # fig_width = 10,
    dev = "png",
    df_print = "kable",
    bibliography = "lisa.bib"
    ),
  clean=FALSE
  )
# Render Book in bookdown_site
bookdown::render_book("index.Rmd")
