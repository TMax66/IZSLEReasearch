librerie()
library(openalexR)
options(openalexR.mailto = "vito.tranquillo@izsler.it")


oa_fetch(
  doi = c(oa_fetch(
    doi = c( "10.1038/s41467-019-08832-8"),
    entity = "works",
    verbose = TRUE
  ) %>%
    show_works() %>%
    knitr::kable()),
  entity = "works",
  verbose = TRUE
) %>%
  show_works() %>%
  knitr::kable()
