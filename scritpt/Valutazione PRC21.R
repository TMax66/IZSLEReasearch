library(tidyverse)
library(here)
library(ggrepel)
library(readxl)
library(hrbrthemes)
abstrEst <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                       sheet = "esterni")



abstrInt <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                                                                sheet = "interni")

deletedpr <- c(3,5, 6, 7, 9, 11, 13, 19, 26,29)
              

abstrEst %>% 
  pivot_longer(cols = 8:11, names_to = "area", values_to = "score")  %>%
  select(-7) %>% 
  bind_rows(
    abstrInt %>% 
      pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
  ) %>% 
  mutate(approvazione = ifelse(`Vecchia numerazione` %in% deletedpr, "Non Approvati dal Ministero", "Approvati dal Ministero")) %>% View() 
  # filter(approvazione == "NO") %>% 
  group_by(Progetto,approvazione) %>% 
  summarise(Score = sum(score, na.rm = TRUE)) %>% 
  mutate(Progetto = factor(Progetto)) %>% 
  ggplot(aes(x = fct_reorder(Progetto, Score), y = Score, label = Score))+
  geom_point(size = 8.5, col= "gray", alpha = 0.8)+
  geom_text(size = 3)+
  geom_segment(aes(y = 0, x = Progetto, yend = Score-1, xend = Progetto))+ 
  coord_flip()+ 
  theme_ipsum(axis_title_size = 15)+
  theme_minimal() +
  facet_wrap(~approvazione, scale="free")

  abstrEst %>% 
    pivot_longer(cols = 8:11, names_to = "area", values_to = "score")  %>%
    select(-7) %>% 
    bind_rows(
      abstrInt %>% 
        pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
    ) %>% 
    mutate(approvazione = ifelse(`Vecchia numerazione` %in% deletedpr, "Non Approvati dal Ministero", "Approvati dal Ministero"), 
           Referee = ifelse(referee %in% c(1,2,3), "Esterni", "Interni"))  %>% 
    group_by(Progetto, Referee) %>% 
    summarise(#Score = sum(score, na.rm = TRUE), 
              MScore = mean(score,  na.rm = TRUE)) %>%
    pivot_wider(names_from = "Referee", values_from = "MScore") %>%   
    ggplot(aes(x=Esterni, y=Interni, label = Progetto ))+
    geom_point(size = 8.5, col="gray50")+
    geom_text()

