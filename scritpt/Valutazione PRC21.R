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
  mutate(approvazione = ifelse(`Vecchia numerazione` %in% deletedpr, "Non Approvati dal Ministero", 
                               "Approvati dal Ministero")) %>%  
  filter(approvazione == "Non Approvati dal Ministero") %>% 
  group_by(Progetto) %>% 
  summarise(MScore = round(mean(score, na.rm = TRUE),2), 
            Score = sum(score, na.rm = TRUE) )%>% 
  pivot_longer(cols = 2:3, names_to = "Valutazione", values_to = "Punteggio") %>% 
  filter(Valutazione == "Score") %>%  
  mutate(Progetto = factor(Progetto)) %>% 
  ggplot(aes(x = fct_reorder(Progetto, Punteggio), y = Punteggio, label = Punteggio))+
  geom_point(size = 9.5, col= "gray", alpha = 0.8)+
  geom_text(size = 4)+
  geom_segment(aes(y = 0, x = Progetto, yend = Punteggio-1, xend = Progetto))+ 
  coord_flip()+ 
  theme_ipsum(axis_title_size = 15)+
  theme_minimal() + labs(title = "Progetti non approvati dal Ministero", y = "Somma punteggi", x = "Codice Progetto")
  

dt <- abstrEst %>% 
  pivot_longer(cols = 8:11, names_to = "area", values_to = "score")  %>%
  select(-7) %>% 
  bind_rows(
    abstrInt %>% 
      pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
  ) %>% View()
   





#facet_wrap(Valutazione~approvazione, scale="free")

  
# abstrEst %>% 
#     pivot_longer(cols = 8:11, names_to = "area", values_to = "score")  %>%
#     select(-7) %>% 
#     bind_rows(
#       abstrInt %>% 
#         pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
#     ) %>% 
#     mutate(approvazione = ifelse(`Vecchia numerazione` %in% deletedpr, "Non Approvati dal Ministero", "Approvati dal Ministero"), 
#            Referee = ifelse(referee %in% c(1,2,3), "Esterni", "Interni"))  %>% 
#     group_by(Progetto, Referee) %>% 
#     summarise(#Score = sum(score, na.rm = TRUE), 
#               MScore = mean(score,  na.rm = TRUE)) %>%
#     pivot_wider(names_from = "Referee", values_from = "MScore") %>%   
#     ggplot(aes(x=Esterni, y=Interni, label = Progetto ))+
#     geom_point(size = 8.5, col="gray50")+
#     geom_text()

