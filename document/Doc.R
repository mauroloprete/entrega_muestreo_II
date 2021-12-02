## ----include = FALSE----------------------------------------------------------

if(!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  here,
  magrittr,
  tidytable,
  knitr,
  kableExtra,
  stringr
 )


options(kableExtra.latex.load_packages = FALSE)

 knitr::write_bib(
   .packages(),
   here(
     "document",
     "biblio",
     "bib.bib"
   )
 )



# Para poder recuperar el script de los chunk

knitr::purl(
  here::here(
    "document",
    "Doc.Rnw"
  )
)

