library(tidyverse)
library(here)
library(ggrepel)
library(readxl)
library(hrbrthemes)
prc22 <- read_excel(here("data", "PRJC22.xlsx"))

prc22 <- prc22 %>% 
  mutate(minsal = ifelse(Progetto %in% c(22,14,9,8,24,2,1,13,10,25,15,4, 19,5,16,6,18,17), "Approvato", "Non Approvato"))
 
#score punteggi----
prc22 %>% 
  pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>% 
  group_by(Progetto,  respscient, minsal) %>% 
  summarise(MScore = round(mean(score, na.rm = TRUE),2), 
            Score = sum(score, na.rm = TRUE) )%>% 
  mutate(Progetto = factor(Progetto)) %>%  
  ggplot(aes(x = fct_reorder(Progetto, Score), y = Score, label = Score))+
  geom_point(size = 9.5,alpha = 0.8, color = "steelblue")+
   
  geom_text(size = 4)+
  geom_segment(aes(y = 0, x = Progetto, yend = Score-1, xend = Progetto), linetype = 3)+ 
  coord_flip()+  
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), 
        legend.position = "blank", 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white")) +
  #theme_ipsum(axis_title_size = 15)+
  #theme_minimal() + 
  labs(title = "", y = "Punteggio", x = "Progetto")+
  theme(strip.placement = "outside")+ ylim (0, 90) + geom_hline(yintercept = 60, colour = "red", linetype = "dashed")+
  scale_y_continuous(breaks = c(seq(0, 60, by=20), 65)) +
  facet_wrap(~minsal, scales = "free")






#media punteggi----
# prc22 %>% 
#   pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>% 
#   group_by(Progetto,  respscient) %>% 
#   summarise(MScore = round(mean(score, na.rm = TRUE),2), 
#             Score = sum(score, na.rm = TRUE) )%>%
#   mutate(Progetto = factor(Progetto)) %>% 
#   ggplot(aes(x = fct_reorder(Progetto, MScore), y = MScore, label = MScore))+
#   geom_point(size = 9.5,alpha = 0.8, color = "steelblue")+
#   
#   geom_text(size = 4)+
#   geom_segment(aes(y = 0, x = Progetto, yend = MScore-1, xend = Progetto), linetype = 3)+ 
#   coord_flip()+  
#   theme_bw() +
#   theme(panel.grid.major.y = element_blank(), 
#         legend.position = "blank", 
#         panel.grid.major = element_line(colour = "white"), 
#         panel.grid.minor = element_line(colour = "white")) +
#   #theme_ipsum(axis_title_size = 15)+
#   #theme_minimal() + 
#   labs(title = "", y = "Punteggio", x = "Progetto")+
#   theme(strip.placement = "outside")+ ylim (0, 90) + geom_hline(yintercept = 60, colour = "red", linetype = "dashed")+
#   scale_y_continuous(breaks = c(seq(0, 60, by=20), 65))



  rank <- prc22 %>% 
    pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>% 
    group_by(Progetto,  respscient) %>% 
    summarise(MScore = round(mean(score, na.rm = TRUE),2), 
              Score = sum(score, na.rm = TRUE) )%>%
    mutate(Progetto = factor(Progetto)) %>%  
    arrange(desc(Score)) %>% 
    select(1) %>% ungroup() %>% 
    t() %>%  as.vector()



prc22 %>% 
  mutate(across(7:10, ~recode(., "1" =  "scarso", 
                              "2" =  "sufficiente", 
                              "3" = "buono", 
                              "4" = "molto buono", 
                              "5" = "eccellente")),  
         Progetto = as.character(Progetto)) %>%  
  select(1, 7:10) %>%  
  pivot_longer(cols = 2:5, names_to = "Item", values_to = "Giudizio") %>%  
  mutate(Giudizio= factor(Giudizio, levels = c( "scarso", 
                                                "sufficiente", 
                                                "buono", 
                                                "molto buono", 
                                                "eccellente"))) %>%  
  group_by(Progetto, Item, Giudizio) %>% 
  count() %>% 
  mutate(Progetto = factor(Progetto, levels = rank)) %>% 
  ggplot(aes(Giudizio, Item, label = n)) + 
  geom_tile( fill = "white", alpha = 5)+ geom_text(size = 6)+
  facet_wrap(~Progetto)+
  theme_bw()+
  theme(legend.position = "blank", 
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=20))
