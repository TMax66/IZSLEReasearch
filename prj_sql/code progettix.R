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

prjuo <- con %>% tbl(sql(queryUO)) %>% as_tibble() 

prjuo <- prjuo %>%
  filter(Tipo_P_A == "P")

prj <- prj %>%
  filter(Tipo_P_A == "P") %>% 
  left_join(stato, by = c("Stato" = "Codice")) %>% 
  left_join(tipologia, by = c("Tipologia" = "Codice")) 


prjx <- prj %>%
  select(Codice, CodIDIzsler, "Stato" = Stato.y, "Tipologia" = Tipologia.y, DataInizio, DataFine, Descrizione, MatrRespScientifico, 
         RespScientifico, FinCompApprovato, MatrRespScientifico) %>% 
  left_join(prjuo, by = c("Codice" = "CodProgetto")) %>% 
  mutate(MatrRespScientifico = ifelse(is.na(MatrRespScientifico), 0, MatrRespScientifico)) %>%  
 filter(MatrRespScientifico > 0)

prjx %>% 
  mutate(
    budgetUO = QuotaSpeseGenerali + QuotaSpeseCoordinamento + QuotaApparecchiature + QuotaReagenti +
      QuotaMissioniConv + QuotaPersContratto + QuotaPersStrutturato,
    anno_inizio = year(DataInizio),
    anno_fine = year(DataFine)
  ) %>%
  filter(anno_inizio >= 2019,
         Tipologia == "Corrente") %>%
  group_by(CodIDIzsler) %>% 
  mutate(totBudgetPrj = sum(budgetUO)) %>%  distinct(CodIDIzsler, .keep_all = TRUE) %>% 
  select(anno_inizio, CodIDIzsler, FinCompApprovato, totBudgetPrj) %>%  
  group_by(anno_inizio) %>% 
  summarise(totBudgetAnno = sum(totBudgetPrj)) %>% 
  
  left_join(
    prj %>% 
      mutate(
        anno_inizio = year(DataInizio),
        anno_fine = year(DataFine)
      ) %>%
      filter(anno_inizio >= 2019, 
             Tipologia.y == "Corrente") %>% 
      group_by(anno_inizio) %>% 
      count(), by = "anno_inizio") %>% 
  mutate(budget_medio = round(totBudgetAnno/n, 2)) %>%  
        # n = paste0(n, "/", totBudgetAnno))%>%  
  select(-totBudgetAnno) %>% 
  pivot_longer(cols = 2:3, names_to = "param", values_to = "values") %>%  
  mutate(param = factor(param, levels= c("n", "budget_medio")), 
         param = recode(param, 
                        "n" = "n. nuovi progetti", 
                        "budget_medio" = "budget medio per progetto")) %>% 
  
  ggplot()+

  aes(x = anno_inizio, y = values,
      label = values)+
  #geom_smooth(size = 1.5, se = FALSE, ) 
  geom_point(size = 3)+
  geom_line(size = 1.5, color = "blue")+
  geom_label(size = 3)+
  facet_wrap(~param, scales = "free", ncol = 1)+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N. di nuovi PRC/anno  e budget medio ")







# prj %>% mutate(
#   anno_inizio = year(DataInizio),
#   anno_fine = year(DataFine)
# ) %>%
#   filter(anno_fine >= 2019) %>% 
#   group_by(anno_fine) %>% 
#   count()
# 
# 
# prj %>% mutate(
#   anno_inizio = year(DataInizio),
#   anno_fine = year(DataFine)
# ) %>%
#   filter(anno_inizio >= 2019) %>% 
#   group_by(anno_inizio, Tipologia.y) %>% 
#   count()
# 
# 
# prj %>% mutate(
#   anno_inizio = year(DataInizio),
#   anno_fine = year(DataFine)
# ) %>%
#   filter(anno_fine >= 2019) %>% 
#   group_by(anno_fine, Tipologia.y) %>% 
#   count()
