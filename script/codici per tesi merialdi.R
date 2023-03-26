library(tidyverse)
library(here)
library(readxl)
library(knitr)
library(kableExtra)
library(janitor)



#prj <- read_excel(sheet = "PRJ", here("data", "prj2021.xlsx"))

prj <- readRDS(here("data", "prjxx.RDS"))

prj <- prj %>% 
  filter(Tipo_P_A == "P")

dt1 <- c( "2019-01-01","2020-01-01","2021-01-01", "2022-01-01" )

dt2 <-  c(  "2019-12-31","2020-12-31","2021-12-31" , "2022-12-31")
anno <- seq(from = 2019, to = 2022, by=1)

x <- data.frame(dt1, dt2, anno)


##funzione per progetti in corso
# prj_func <- function( dt1, dt2, anno)
# { prj %>%
#     filter(Tipologia!= "Istituzionale") %>% 
#     group_by( Tipologia) %>% 
#     mutate("Stato" = ifelse(DataFine < as.Date(dt1), "Archiviato", "Attivo")) %>% 
#     filter(Stato == "Attivo" & DataInizio <= as.Date(dt2)) %>%
#     # mutate("Statoanno" = ifelse(DataFine <=as.Date(dt1), "Concluso", "Aperto")) %>%
#     # filter(Statoanno == "Aperto") %>% 
#     summarise( 
#       "N.Progetti"=nlevels(factor(Codice)), 
#       Budget = sum(Budget, na.rm = T)) %>% 
#     mutate(anno = anno)
#   
# }


prj_func2 <- function(dt1, dt2,  anno)
{ prj %>%
    filter(Tipologia!= "Istituzionale") %>% 
    filter(DataInizio >= as.Date(dt1) & DataInizio <=as.Date(dt2)) %>% 
    group_by( Tipologia) %>% 
    summarise(Bdg = sum(Budget), 
              "N.Progetti"=nlevels(factor(Codice)))%>% 
    mutate(anno = anno)
  
}





z <- list()

for (i in 1:4) { 
  z[[i]]<- prj_func2(  dt1 = x[i, 1], dt2 = x[i, 2], anno = x[i, 3])
  
}
progetti <- do.call(rbind, z)

progetti <- progetti %>%
  
mutate(Ricerca = ifelse(Tipologia == "Corrente", "Non Competitiva",
                                                   ifelse(Tipologia == "Autofinanziato", "Non Competitiva",
                                                          "Competitiva"))) %>%
select("Anno" = anno, Ricerca,   Tipologia, N.Progetti , "Budget" = Bdg)  %>% 
  arrange(Anno, Ricerca)
  
pr <- progetti %>% 
  select(-Budget, -Ricerca) %>% 
  pivot_wider(names_from = c("Tipologia"), values_from = c("N.Progetti"), values_fill = 0) %>% 
  adorn_totals(where = "row") %>% 
  adorn_totals(where = "col")
   
  
pr2 <- progetti %>% 
  select(-N.Progetti, -Ricerca) %>% 
  pivot_wider(names_from = c("Tipologia"), values_from = c("Budget"), values_fill = 0) %>% 
  adorn_totals(where = "row") %>% 
  adorn_totals(where = "col")

p3 <- progetti %>% 
  select(-N.Progetti) %>% 
  mutate(Anno = as.character(Anno)) %>% 
  group_by(Anno, Ricerca) %>% 
  summarise(Budget = sum(Budget)) %>% 
 pivot_wider(names_from = c("Ricerca"), values_from = c("Budget"), values_fill = 0) %>% 
  adorn_totals(where = "col") %>% 
  mutate(rapp = Competitiva/`Non Competitiva`) %>% 
  select(Anno, Total, "Competitiva(A)" = Competitiva,  "Non Competitiva (B)"=`Non Competitiva`, "A/B" = rapp)

p3m <- summarize_all(p3[,-1], mean)
 
p3 <- p3 %>% 
  add_row(Anno= "Media", p3m)

 
  
  
  



# pivot_wider(names_from = "Tipologia", values_from = c("N.Progetti", "Budget"), values_fill = 0) %>%)
  # rename(N.Progetti)
  
library(gt)

gt(pr) %>%  
  tab_header(
    title = "Progetti di ricerca finanziati da Enti terzi in IZSLER (  2018-2021)"
  ) %>%
   tab_spanner(
     label = "Ricerca Competitiva",
     columns = c(Finalizzato, Europeo, Regionali, CCM,`Altro tipo` )
   ) %>%
     tab_spanner(
       label = "Ricerca Non Competitiva",
       columns = c(Corrente, Autofinanziato)
     ) %>% 
 
  gtsave("tab_1.rtf")
 


 
gt(pr2) %>%  
  tab_header(
    title = "Finanziamenti dei progetti di ricerca per Anno e Tipologia"
  ) %>%
  tab_spanner(
    label = "Ricerca Competitiva",
    columns = c(Finalizzato, Europeo, Regionali, CCM,`Altro tipo` )
  ) %>%
  tab_spanner(
    label = "Ricerca Non Competitiva",
    columns = c(Corrente, Autofinanziato)
  ) %>% 
  gtsave("tab_2.rtf")
 
gt(p3) %>%  
  tab_header(
    title = "Finanziamenti dei progetti di ricerca per Anno e Tipologia"
  ) %>%
  
   
  gtsave("tab_3.rtf")


         
         
#   # pivot_wider(names_from = "Tipologia", values_from = "N.Progetti") %>% 
#   #pivot_wider(names_from = "Tipologia", values_from = "Budget") %>%  
#  # select(anno, Corrente, Finalizzato, Europeo, Regionali, Autofinanziato, CCM, "Altro tipo" ) %>% 
#   
#   
#   
#   
#   
#   
#   
#   
#   knitr::kable(digits = 2, caption = "Progetti in corso") %>%
#   kable_styling()  
# 
# 
# 
# dt1 <- c( "2017-01-01","2018-01-01","2019-01-01","2020-01-01", "2021-01-01")
# 
# dt2 <-  c("2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-12-31" )
# anno <- seq(from = 2017, to = 2021, by=1)
# 
# x <- data.frame(dt1, dt2, anno)
# 
# prj_func2 <- function(  dt1, dt2, anno)
# {  prj %>% 
#     group_by( Tipologia) %>% 
#     filter(DataInizio >= as.Date(dt1) & DataInizio <=as.Date(dt2)) %>% 
#     summarise( 
#       # "N.Progetti"=nlevels(factor(Codice)), 
#       Budget = sum(Budget, na.rm = T)) %>% 
#     mutate(anno = anno) 
#   
# }
# 
# z <- list()
# 
# for (i in 1:5) { 
#   z[[i]]<- prj_func2(  dt1 = x[i, 1], dt2 = x[i, 2], anno = x[i, 3])
#   
# }
# nprogetti <- do.call(rbind, z)
# 
# 
# nprogetti %>% 
#   # pivot_wider(names_from = "Tipologia", values_from = "N.Progetti") %>% 
#   pivot_wider(names_from = "Tipologia", values_from = "Budget") %>%  
#   select(anno, Corrente, Finalizzato, Europeo, Regionali, Istituzionale, Autofinanziato, CCM, "Altro tipo" ) %>% 
#   rowwise() %>% 
#   #mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale, CCM,`Altro tipo`)) %>% 
#   mutate(Competitivi = sum(Finalizzato, Europeo, Regionali, Istituzionale, CCM,`Altro tipo`, na.rm = T)) %>% 
#   select(Anno = anno, "Ricerca Corrente" = Corrente, "Ricerca Competitiva" = Competitivi) %>% 
#   knitr::kable(digits = 2, caption = "Nuovi Progetti") %>%
#   kable_styling()  
#  
# 
