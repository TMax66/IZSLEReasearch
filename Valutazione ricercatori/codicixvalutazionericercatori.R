library(tidyverse)
library(here)
library(readxl)
library(openxlsx)
library(gt)
library(hrbrthemes)


dati <- read_excel("Valutazione ricercatori/ricercatori.xls")


dt <- dati %>% 
  mutate(score = as.numeric(ifelse(`PUNTEGGIO FINALE CONFERMATO DAL NV PRS` == "OK", 
                        `TOTALE PROPOSTO DAL DIRIGENTE ST COMPLESSA`,`PUNTEGGIO FINALE CONFERMATO DAL NV PRS` ))) %>% 
  rename_all(., .funs = tolower)
  



dt %>%
  #filter(categoria == "CAT.D") %>% 
  
  ggplot(
    aes(x = mansione,
        y = score, 
        group = mansione)
  )+
  geom_violin()+
  geom_jitter(alpha = 0.3, width = 0.1)+
  stat_summary(fun = "median", color = "red")+
  #facet_wrap(~ OA, nrow = 1)+
  theme_classic()+
  theme(
    panel.grid.major = element_line()
  )+
  coord_flip()+
  
  labs(x= "", y= "valutazione", title = "Valutazione del Personale della Ricerca - 2022")

  
  
  
  
  
  
  
  
  
  



  ggplot(aes(x = score))+
  
  geom_histogram(bins = 15, col= "grey", fill= "steelblue")+ 
  geom_vline(aes(xintercept = median(score)), color = "red")+
  xlim(50,100)+
  geom_text(aes(x = 80, y = 15, label = paste("Mediana = ", 97)))+
  labs(x= "valutazione", y= "n. personale dirigente", 
       subtitle = "Distribuzione della valutazione dei collaboratori della ricerca")+
  theme_ipsum_rc()


ggplot() + 
  aes(x = number12m, fill = treatment) +
  geom_dotplot(binwidth = 1) +
  scale_y_continuous(NULL, breaks = NULL) + 
  # Make this ratio = (tallest column * binwidth * 1.5)
  coord_fixed(ratio = 6) +
  labs(title = "Distribution of Participants binned by number of Polyps", subtitle = "Colored by Treatment") +
  theme(legend.position = "top")




dt %>% 
  filter(categoria == "CAT.D") %>% 
  ggplot(aes(y = score)
  )+
  geom_boxplot()+
 
  geom_jitter(alpha = 0.1)

  
  stat_summary(fun = "median", color = "red")+
  #facet_wrap(~ categoria, scales = "free")+
  theme_ipsum_rc()+
  labs(x= "Anno", y= "valutazione in punti %")
