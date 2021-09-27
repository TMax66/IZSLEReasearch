library(tidyverse)
library(here)

library(readxl)
abstrEst <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                       sheet = "esterni")



abstrInt <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                                                                sheet = "interni")



abstrEst %>% 
  pivot_longer(cols = 8:11, names_to = "area", values_to = "score")  %>%
  select(-7) %>% 
  bind_rows(
    abstrInt %>% 
      pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
  ) %>% View()
 
  
