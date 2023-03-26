library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(here)
library(lubridate)
library(DBI)
library(RODBC)
library(odbc)
library(openxlsx)

options(scipen = 999)

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it",
                      Database = "ProgettiAccordi")

source(here("sql.R"))

queryStato <- "SELECT *
               FROM Stato"
queryTipologia <- "SELECT *
                   FROM TipologieProgetto"

stato <- con %>% tbl(sql(queryStato)) %>% as_tibble() %>% select(Codice, Stato = Descrizione)

tipologia <- con %>% tbl(sql(queryTipologia)) %>% as_tibble() %>% select(Codice, Tipologia = Descrizione)

prj <- con %>% tbl(sql(queryPA)) %>% as_tibble() 

prj <- prj %>%
  filter(Tipo_P_A == "P") %>% 
  left_join(stato, by = c("Stato" = "Codice")) %>% 
  left_join(tipologia, by = c("Tipologia" = "Codice")) 


prj %>% mutate(
  anno_inizio = year(DataInizio),
  anno_fine = year(DataFine)
  ) %>%
  filter(anno_inizio >= 2019 & anno_inizio < 2023) %>% 
  group_by(anno_inizio) %>% 
  count() %>% 
  rename(anno = anno_inizio) %>% 
  mutate(progetti = "nuovi progetti") %>% 
  
  bind_rows(
    
    prj %>% mutate(
      anno_inizio = year(DataInizio),
      anno_fine = year(DataFine)
    ) %>%
      filter(anno_fine >= 2019 & anno_fine <2023) %>% 
      group_by(anno_fine) %>% 
      count() %>% 
      rename(anno = anno_fine) %>% 
      mutate(progetti = "progetti conclusi")
    
    
  ) %>%  
  
  ggplot()+
  aes(x = anno, y = n,
      label = n)+
  #geom_smooth(size = 1.5, se = FALSE, ) 
  geom_point(size = 3)+
  geom_line(size = 1.5, color = "blue")+
  geom_label(size = 3)+
  facet_wrap(~progetti, scales = "free_x")+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N. di nuovi progetti  e progetti conclusi per anno")
  
  
  

prj %>% mutate(
  anno_inizio = year(DataInizio),
  anno_fine = year(DataFine)
) %>%
  filter(anno_fine >= 2019) %>% 
  group_by(anno_fine) %>% 
  count()









prj %>% mutate(
  anno_inizio = year(DataInizio),
  anno_fine = year(DataFine)
) %>%
  filter(anno_inizio >= 2019 & anno_inizio< 2023) %>% 
  group_by(anno_inizio, Tipologia.y) %>% 
  count() %>% 
  
  ggplot()+
  aes(x = anno_inizio, y = n,
      label = n)+
  #geom_smooth(size = 1.5, se = FALSE, ) 
  geom_point(size = 3)+
  geom_line(size = 1.5, color = "blue")+
  geom_label(size = 3)+
  facet_wrap(~Tipologia.y, scales = "free_x")+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N. di nuovi progetti per anno e tipologia")


  
  





prj %>% mutate(
  anno_inizio = year(DataInizio),
  anno_fine = year(DataFine)
) %>%
  filter(anno_fine >= 2019) %>% 
  group_by(anno_fine, Tipologia.y) %>% 
  count()
