######BIBLIOMETRIX##############
###############################
library(tidyverse)
library(bibliometrix)
library(Matrix)
library(stringr)
library(igraph)
library(FactoMineR)
library(factoextra)
library(scholar)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(here)
library(hrbrthemes)
library(cowplot)
library(ggrepel)
library(stringi)

# 
# pubblicazioni <- read_excel("C:/Users/vito.tranquillo/Desktop/Git Projects/COGEPERF/data/raw/pubblicazioni.xlsx")
# 
# pubblicazioni %>% 
#   group_by(OA) %>% 
#   distinct(NR) %>% 
#   count()



# Last.Name <- c("Tranquillo", "Merialdi", "Lavazza", "Pongolini", "Varisco")
# First.Name <- c("Vito", "Giuseppe", "Antonio", "Stefano", "Giorgio")
# df <- data.frame(Last.Name, First.Name)
# 
# 
# library(purrr)
# library(scholar)
# p_get_scholar_id <- possibly(get_scholar_id, otherwise = NA_character_)
# scholars <- character(nrow(df))
# for(i in seq_along(scholars)) {
#   scholars[i] <- p_get_scholar_id(last_name = df$Last.Name[i], 
#                                   first_name = df$First.Name[i])
# }
# 
# id <- "tthQ_DQAAAAJ&hl"http://127.0.0.1:43025/graphics/plot_zoom_png?width=1509&height=570
# 
# p <- get_publications(id)

source( here("script", "dati.R"))
library(cowplot)

p <- prod %>% 
  filter(PY< 2023 & PY > 2018   & Istituto == "izsler" ) %>%  
 mutate(
  #n = c(86, 89, 88, 90, 88, 123, 141, 132), 
   PY = factor(PY, levels = c("2019", "2020", "2021","2022"))) %>% View()
       #  PY = factor(PY, levels = c("2015","2016", "2017", "2018", "2019", "2020", "2021","2022") )) %>%  
  ggplot(aes(x=PY, y=n, group=Istituto, label=n))+  
  geom_label(nudge_y = 5)+
  ylim(50, 150)+
  labs(y = "", x = "")+
  geom_smooth(size = 2.5, se = FALSE) + geom_point(size = 3)+
  theme_classic()+
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(), 
        axis.text.x = element_text(size=15),
        title = element_text(size = 20), 
        plot.caption = element_text(size = 15))+
  #geom_text(aes(x= 2020, y = 90), label = "Growh rate = +6.3%", size = 5)+
  labs(title = "Produzione scientifica dell'IZSLER \n (tasso di crescita +14.5%)", 
      caption = "N. di pubblicazioni/anno su riviste peer-review \n indicizzate da Web of Science (fonte: base dati Biblioteca IZSLER)", 
       x = "", y = "") 
  

  
  pubrate <- function(istituto)
  {
    ist <- istituto %>% 
      filter(  PY < 2023 & PY >2019) 
    
    M <- biblioAnalysis(ist, sep = ";" )
    
    Y <- data.frame(table(M$Years))
    
    ny <- max(as.numeric(levels(Y[,1])),na.rm=TRUE)-min(as.numeric(levels(Y[,1])),na.rm=TRUE)
    
    GR <- ((Y[nrow(Y),2]/Y[1,2])^(1/(ny))-1)*100
    
    GR
    
    
  }
  
  M <- biblioAnalysis(izsler, sep = ";" )
  
  
  
  Y <- data.frame(table(M$Years))
  
  Y$Var1 <- as.character(Y$Var1)
  
  Y <- Y %>% filter(Var1 !=  2023) %>% 
    mutate(Freq =  c(86, 89, 88, 90, 88, 123, 141, 132))
  Y <- Y %>% 
    filter(Var1 > 2018)
  
  ny <- max(as.numeric(levels(factor(Y[,1]))),na.rm=TRUE)-min(as.numeric(levels(factor(Y[,1]))),na.rm=TRUE)
  
  GR <- ((Y[nrow(Y),2]/Y[1,2])^(1/(ny))-1)*100
  
  
  
  library(readxl)
  
  pubblicazioni <- read_excel(here( "data","pubblicazioni.xlsx"))
  pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
  pubblicazioni$AU <- str_remove(pubblicazioni$AU, " ")
  pubblicazioni$AU <- gsub("_", " ", pubblicazioni$AU)
  pubblicazioni$Nome <- str_extract( pubblicazioni$AU, ",.*$")
  pubblicazioni$Nome <- str_remove(pubblicazioni$Nome, ",")
  pubblicazioni$Nome <- gsub("\\s.*$", "", pubblicazioni$Nome)
  pubblicazioni$Cognome <- gsub(",.*$", "", pubblicazioni$AU)
  
  
  pub <- pubblicazioni %>% 
    mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
           INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
           NAZ = ifelse(Congr == "Naz", "Naz", NA), 
           Oth = ifelse(Congr == "Others" , "Others", NA), 
           IF = as.numeric(IF))  
  
  
  
  
  
  
  
  
  
  
  ##grafico 2 presentazione cda----
  
  pub  %>%
    filter(articoliif == "IF") %>% 
    select(OA, NR, IF) %>% 
    unique() %>%   
    group_by(OA) %>% 
    summarise("Pubblicazioni" = nlevels(factor(NR)), 
              "sIF" = round(sum(IF, na.rm = TRUE),0), 
              "mIF" = round(mean(sIF/Pubblicazioni),2)) %>% 
    pivot_longer(cols = 2:4, names_to = "Parameter", values_to = "Value") %>%  
    mutate(Parameter = factor(Parameter, levels= c("Pubblicazioni", 
                                                   "sIF", 
                                                   "mIF")), 
           Parameter = recode(Parameter, 
                              "sIF" = "somma IF", 
                              "mIF" = "IF medio")) %>%  
    bind_rows(
      
      pub %>% 
        mutate(JO = casefold(JO, upper = TRUE)) %>% 
        filter(OA <2023 & OA >2018) %>% 
        select(OA, JO, NR) %>% 
        distinct() %>% 
        group_by(OA) %>% 
        count(JO) %>% 
        select(-n) %>%
        count() %>% 
        mutate(Parameter = rep("#Journal")) %>% 
        rename(Value = n) %>%  
        select(OA, Parameter, Value)   
      
    ) %>%   
    mutate(Parameter = factor(Parameter, levels = c("Pubblicazioni","#Journal","somma IF","IF medio"  ))) %>% 
    ggplot()+
    aes(x = OA, y = Value, label = Value)+
    geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
    geom_label(size = 3)+
    labs(title = "Impatto citazionale", x = "", y = "")+
    
    facet_wrap(~ Parameter, scales = "free", nrow = 1)+
    theme_classic()+
    theme(axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.text.x = element_text(size=10),
          title = element_text(size = 20), 
          plot.caption = element_text(size = 15))
  
  
  
  
  
#versione alternativa del grafico impatto citazionale
  # pub  %>%
  #   filter(articoliif == "IF") %>% 
  #   select(OA, NR, IF) %>% 
  #   unique() %>%   
  #   group_by(OA) %>% 
  #   summarise("Pubblicazioni" = nlevels(factor(NR)), 
  #             "sIF" = round(sum(IF, na.rm = TRUE),0), 
  #             "mIF" = round(mean(sIF/Pubblicazioni),2)) %>%  
  #   select(- Pubblicazioni) %>% 
  #   pivot_longer(cols = 2:3, names_to = "Parameter", values_to = "Value") %>%  
  #   mutate(Parameter = factor(Parameter, levels= c(
  #     "sIF", 
  #     "mIF")), 
  #     Parameter = recode(Parameter, 
  #                        "sIF" = "somma IF", 
  #                        "mIF" = "IF medio")) %>%   
  #   
  #   mutate(Parameter = factor(Parameter, levels = c("somma IF","IF medio"  ))) %>% 
  #   ggplot()+
  #   aes(x = OA, y = Value, label = Value)+
  #   geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
  #   geom_label(size = 3)+
  #   labs(title = "Impatto citazionale", x = "", y = "")+
  #   
  #   facet_wrap(~ Parameter, scales = "free", nrow = 1)+
  #   theme_classic()+
  #   theme(axis.text.y = element_blank(), 
  #         axis.ticks = element_blank(), 
  #         axis.line = element_blank(), 
  #         axis.text.x = element_text(size=10),
  #         title = element_text(size = 20), 
  #         plot.caption = element_text(size = 15))
  # 
  
  JO <- pub %>% 
    select(OA, JO, NR) %>% distinct() %>%  
    group_by(OA, JO) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    pivot_wider(names_from = "OA", values_from = "n", values_fill = 0) %>% 
    select(JO, `2019`, `2020`,`2021`,`2022`)
  
  
  
  j <-  pub %>% 
    mutate(JO = casefold(JO, upper = TRUE)) %>% 
    filter(OA <2023 & OA >2018) %>% 
    select(OA, JO, NR) %>% 
    distinct() %>% 
    group_by(JO) %>% 
    summarise(n=n()) %>%  
    top_n(20, n) %>% 
    arrange(n) %>% 
    mutate(JO = factor(JO, unique(JO))) %>% 
    ggplot(aes(x=JO, y=n, label = n))+geom_bar(stat = "identity", fill="steelblue")+
    coord_flip()+ geom_label()+labs(title = "n.articoli/rivista  pubblicati nel periodo 2019-2022", x= "", y = "")+
    theme_classic()+
    theme(axis.text.y = element_text(size = 12), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.text.x = element_text(size=10),
          title = element_text(size = 18), 
          plot.caption = element_text(size = 15))
  

##obiettivi produzione scintifica 2022
  pub %>%  
    filter(articoliif == "IF") %>%
    count(OA,  NR) %>%  
    group_by(OA, ) %>% 
    count(NR) %>% 
    summarise("Pubblicazioni" = sum(n)) %>%   
    filter(!is.na(Pubblicazioni)) %>%    
    pivot_wider(names_from = "OA", values_from = "Pubblicazioni") %>%  ungroup() %>%  
    mutate(media = round(rowMeans(.[2:4], na.rm = T), 1), 
           atteso = round(0.10*media,1), 
           target = round(atteso+media, 1), 
           '% raggiungimento obiettivo' = round(100*(`2022`/target),1))
  
  # ggplot(aes(x=SO, y=n, label = n))+geom_bar(stat = "identity", fill="red")+coord_flip()+
  # geom_label()
  
  
  
  pub %>% 
    mutate(JO = casefold(JO, upper = TRUE)) %>% 
    select(OA, JO, NR, IF) %>% distinct() %>%  
    group_by(JO) %>% 
    summarise(IF = round(max(IF),1)) %>% 
    top_n(20, IF) %>% 
    arrange(IF) %>% 
    mutate(JO = factor(JO, unique(JO))) %>% 
    ggplot(aes(x=JO, y=IF, label = IF))+geom_bar(stat = "identity", fill="steelblue")+
    coord_flip()+ geom_label()+labs(title = "Top 20 riviste in base al valore di IF", x= "", y = "")+
    theme_classic()+
    theme(axis.text.y = element_text(size = 12), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.text.x = element_text(size=10),
          title = element_text(size = 18), 
          plot.caption = element_text(size = 15))
  
  
  
  
  ## numero di riviste 
  pub %>% 
    mutate(JO = casefold(JO, upper = TRUE)) %>% 
    filter(OA <2023 & OA >2018) %>% 
    select(OA, JO, NR) %>% 
    distinct() %>% 
    group_by(OA) %>% 
    count(JO) %>% 
    select(-n) %>%
    count() %>% 
    mutate(Parameter = rep("#Journal")) %>%  View()
  
  ggplot()+
    aes(x = OA, y = n, label = n)+
    geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
    geom_label(size = 3)+
    labs(title = "N. di riviste che pubblicano lavori IZSLER", x = "", y = "")+
    theme_classic()+
    theme(axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.text.x = element_text(size=10),
          title = element_text(size = 18), 
          plot.caption = element_text(size = 15))
  
  
#distribuzione IF
  pub  %>%
    filter(articoliif == "IF") %>% 
    select(OA, NR, IF) %>% 
    unique()  %>% 
    ggplot(
      aes(x = OA,
          y = IF, 
          group = OA)
    )+
    geom_violin()+
    geom_jitter(alpha = 0.3)+
    stat_summary(fun = "mean", color = "red")+
    #facet_wrap(~ OA, nrow = 1)+
    theme_classic()+
    theme(
      panel.grid.major = element_line()
    )+
    
    labs(x= "Anno", y= "IF", title = "Distribuzione IF nel periodo 2019-2022")
  
    
#N.di autori
  
  ricIZSLER <- readRDS(here("data", "pub.rds"))
  
  ricIZSLER %>% filter(!is.na(Matricola)) %>%
    select(OA, Cognome) %>% 
    distinct() %>%  
  #filter(!Dipartimento %in% c("COSTI COMUNI E CENTRI CONTABILI", "DIREZIONE GENERALE")) %>% 
    # mutate(Cognome = as.factor()) %>% 
    group_by(OA) %>% 
    count() %>% 
    ggplot()+
    aes(x = OA, 
        y = n, 
        label = n)+
    geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
    geom_label(size = 3)+
   # facet_wrap(~Dipartimento)+
    theme_classic()+
    theme(
       
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(), 
      axis.line = element_blank(), 
    )+
    labs(y = "", x = "Anno", title = "N. di autori di articoli scientifici per Anno")
  

  

    
ricIZSLER <- readRDS(here("data", "pub.rds"))
  
  ricIZSLER %>% filter(!is.na(Matricola)) %>%
    select(OA, Cognome, Dipartimento) %>% 
    distinct() %>%  
    filter(!Dipartimento %in% c("COSTI COMUNI E CENTRI CONTABILI", "DIREZIONE GENERALE")) %>% 
   # mutate(Cognome = as.factor()) %>% 
    group_by(OA, Dipartimento) %>% 
      count() %>% 
    ggplot()+
    aes(x = OA, 
        y = n, 
        label = n)+
    geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
    geom_label(size = 3)+
    facet_wrap(~Dipartimento)+
    theme_classic()+
    theme(
      strip.text = element_text(size = 6)
    )+
    labs(y = "n. autori", x = "Anno", title = "N. di autori di ariticoli scientifici per anno e Dipartimento")
    
    
  
  prj <-
    tibble(
      Anno = c(2019,2020,2021, 2022), 
      Prj = c(167, 158, 173, 142)
    ) %>% 
    ggplot()+
    aes(x = Anno, 
        y =Prj, 
        label = Prj)+
    geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
    geom_label(size = 3)+
    # facet_wrap(~Dipartimento)+
    theme_classic()+
    theme(
      
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(), 
      axis.line = element_blank(), 
    )+
    labs(y = "", x = "Anno", title = "N. di progetti di ricerca in corso per Anno")

    
  
  ##valutazione ex-ante prj
  
  prc22 <- read_excel(here("data", "PRJC22.xlsx"))
  prc21 <- readRDS(here("data", "prc21.rds"))
  
  prc22 <- prc22 %>% 
    mutate(minsal = ifelse(Progetto %in% c(22,14,9,8,24,2,1,13,10,25,15,4, 19,5,16,6,18,17), "Approvato", "Non Approvato"))
  
 
  
  prc21 %>%   
    rename(Item = area, 
           Giudizio = score) %>% 
    group_by(Progetto, Item) %>% 
    summarise(Giudizio = mean(Giudizio)) %>%
    mutate(Anno = "PRC 21") %>% 
    
    bind_rows(
      
      prc22 %>% 
        select(1, 7:10) %>% 
        pivot_longer(cols = 2:5, names_to = "Item", values_to = "Giudizio") %>%   
        group_by(Progetto, Item) %>% 
        summarise(Giudizio = mean(Giudizio)) %>% 
        mutate(Anno = "PRC 22")
    ) %>%   
    
    
    ggplot(
      aes(x = Item,
          y = Giudizio, 
          group = Item)
    )+
    geom_violin()+
    geom_jitter(alpha = 0.3, height = 0.1)+
    stat_summary(fun = "median", color = "red")+
    #facet_wrap(~ OA, nrow = 1)+
    theme_classic()+
    theme(
      panel.grid.major = element_line()
    )+
    coord_flip()+
    labs(x= "", y= "Giudizio", title = "Progetti di Ricerca Corrente: Valutazione ex-ante")+
      
      facet_wrap(~ Anno, nrow = 1)+
    ylim(0,5)
    

  
   #dumballplot
 p1 <-  prc22 %>% 
    mutate(Progetto = as.character(Progetto)) %>%  
    select(1, 7:10) %>% 
    pivot_longer(cols = 2:5, names_to = "Item", values_to = "Giudizio") %>%  
    group_by(Progetto, Item) %>% 
    summarise( Giudizio= mean(Giudizio)) %>% 
    group_by(Item) %>% 
    summarise( Giudizio= mean(Giudizio)) %>% 
      mutate(anno = 2022) %>% 
     # pivot_wider(names_from = "Item", values_from = "Giudizio") %>%   
    
    bind_rows(  
      
      
    prc21 %>% 
    mutate(Progetto = as.character(Progetto)) %>%   
      rename(Item = area, Giudizio = score) %>%   
    group_by(Progetto, Item) %>% 
    summarise( Giudizio= mean(Giudizio)) %>% 
    group_by(Item) %>% 
    summarise( Giudizio= mean(Giudizio)) %>%  
      mutate(anno = 2021) 
     # pivot_wider(names_from = "Item", values_from = "Giudizio") 
    ) %>% 
   mutate(Giudizio = round(Giudizio,1)) %>% 
   pivot_wider(names_from = "anno", values_from = "Giudizio") %>% 
   rename(prc21 = `2021`, prc22 = `2022`) %>% 
   
   ggplot() +
   geom_segment(
     aes(x = prc21, y = Item, xend = prc22,
         yend = Item),
     color = "#aeb6bf",
     size = 4.5,
     alpha = .5)+
   geom_point(aes(x = prc21, y = Item, color = "PRC 21"),
              size = 9, 
              alpha = 0.8) +
   geom_point(aes(x = prc22, y = Item, color = "PRC 22"),
              size = 9,   alpha = 0.6)+
   
   geom_text(
     aes(x = prc21, y = Item, label = prc21),
     size = 3.5, family = c("Montserrat")) +
   geom_text(
     aes(x = prc22, y = Item, label = prc22),
     size = 3.5, family = c("Montserrat"))+
   
   scale_color_manual(labels = c("PRC 21", "PRC 22"),
                      values = c("PRC 21" = "lightsalmon", "PRC 21" = "lightsteelblue3")) 
   
 
 
 ## n. di articoli per autore
 
 
pub %>% 
  select(AU, OA, NR) %>%  
  group_by(OA, AU) %>% 
  count() %>% 
  group_by(OA) %>% 
  ggplot(
    aes(x = OA,
        y = n, 
        group = OA)
  )+
  geom_violin()+
  geom_jitter(alpha = 0.3, height = 0.1)+
  stat_summary(fun = "median", color = "red")+
  #facet_wrap(~ OA, nrow = 1)+
  theme_classic()+
  theme(
    panel.grid.major = element_line()
  )+
  labs(x= "Anno", y= "n. articoli per autore/anno", title = "N. di articoli per Autore nel periodo 2019-2022")
  



# partecipazione a convegni
library(readxl)
library(lubridate)
library(openxlsx)
conv  <- read_excel("data/paetecipazioneeventi.xlsx")


tipi <- c("congresso","convegno", "conferenza", "forum", "meeting","conference", "symposium",
          "workshop", "congress", "colloquium")


convegni <- conv %>% 
  mutate(evento = casefold(`Titolo Evento`, upper = FALSE), 
  evento = ifelse(
    str_detect(evento, paste(tipi, collapse = "|")), "convegno", "corso"), 
  anno = year(`Data inizio`)) %>%
  select(anno, Tipo, evento,`Titolo Evento` ) %>%
  filter(anno > 2018 & anno < 2023, 
         evento != "corso")  
  


#numero di partecipanti ai convegni
convegni %>% 
group_by(anno, Tipo) %>%
  count() %>% 
  ggplot()+
  aes(x = anno, y = n,
      label = n)+
  geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
  geom_label(size = 3)+
  facet_wrap(~Tipo)+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N. di partecipanti a convegni nel periodo 2019-2022")


  

# numero di convegni a cui hanno parteciparto ricercatori IZSLER
convegni %>% 
  distinct() %>%
  group_by(anno, Tipo) %>%
  count() %>% 
  ggplot()+
  aes(x = anno, y = n,
      label = n)+
  geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
  geom_label(size = 3)+
  facet_wrap(~Tipo)+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N. di convegni nel periodo 2019-2022")


 
# articoli personale della ricerca----


dt %>% filter(categoria == "CAT.D LIV. SUPER") %>% 
  left_join(
    pub, by = c("cognome"= "Cognome")
  ) %>%  
  distinct(NR, .keep_all = TRUE) %>%
  group_by(OA, mansione, NR) %>% 
  count() %>%   
  group_by(OA, mansione) %>% 
  summarise(m.articoli  = sum(n)) %>% 
  filter(!is.na(OA)) %>% 
  
bind_rows(
    dt %>% filter(categoria == "CAT.D") %>% 
      left_join(
        pub, by = c("cognome"= "Cognome")
      ) %>%  
      distinct(NR, .keep_all = TRUE) %>%
      group_by(OA, mansione, NR) %>% 
      count() %>%   
      group_by(OA, mansione) %>% 
      summarise(m.articoli  = sum(n)) %>% 
      filter(!is.na(OA))
  ) %>%  
  ggplot()+
  aes(x = OA, y = m.articoli,
      label = m.articoli)+
  geom_smooth(size = 1.5, se = FALSE) + geom_point(size = 3)+
  geom_label(size = 3)+
  facet_wrap(~mansione)+
  theme_classic()+
  theme(
    strip.text = element_text(size = 10), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    
  )+
  labs(y = "", x = "Anno", title = "N.di articoli con la partecipazione del personale della ricerca")

  















     
  
  
  # ggplot()+
  # aes(x = n)+
  # geom_histogram(bins = 18, col = "blue")+
  #   facet_wrap(~OA)
  
  
 # summarise(m = mean(n))
 
 
 
 
 
 
 
 
 
 
 
 
 #column_to_rownames("anno")
    
# p1 <- rbind(rep(5,4) , rep(0,4) , p1)
  
    
    
    
    
    
  
  
    
    # ggplot(aes(x=PY, y=n, label = n))+  
    # labs(y="n.articoli", x="anno")+
    # ylim(c(0,150))+
    # geom_line()+ geom_point(size = 10, col = "lightgrey")+
    # geom_text()+
    # theme_ipsum(axis_title_size = 15)+
    # theme(legend.position = "none")+facet_wrap(~Istituto, scales = "free")+
    # labs(title = "Produzione scientifica degli IIZZSS periodo 2019-2021", 
    #      subtitle = "N. di pubblicazioni su riviste peer-review indicizzate da Web of Science (fonte dati IZSLER: base dati Biblioteca)", 
    #      x = " Anno di pubblicazione", y = "n. di articoli")
    # 
    # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# path <- "C:/Users/vito.tranquillo/Desktop/Git Projects/IZSLEReasearch/figure/jj"
# 
# img <- readPNG(path)
# 
# img_graph <- p +                  
#   inset_element(p = img,
#                 left = 0.05,
#                 bottom = 0.65,
#                 right = 0.5,
#                 top = 0.95)
# 
# 







# prod %>% 
#   mutate(Istituto= recode(Istituto, izsler = "IZSLER", 
#                           izsam = "IZSAM", 
#                           izsve = "IZSVE", 
#                           izsic = "IZS Sicilia", 
#                           izslt = "IZSLT", 
#                           izsmezz = "IZS Mezzogiorno", 
#                           izspiem = "IZSTO", 
#                           izspuglia = "IZS Puglia Basilicata", 
#                           izssard = "IZS Sardegna", 
#                           izsum = "IZSUM")) %>%  
#   filter( PY < 2023 & PY >2018 ) %>%  
  # mutate(n = ifelse(PY == 2019 & Istituto == "IZSLER", 88, 
  #                   ifelse(PY == 2020 & Istituto == "IZSLER",123, 
  #                          ifelse(PY == 2021 & Istituto == "IZSLER",135, n)))) %>% 
  
  
  #mutate(lab = if_else(PY == max(PY), as.character(Istituto), NA_character_)) %>%  View()
  
 
  
  
  
  
  
  
 
  # geom_text(aes(x= 2000, y = 95), label = "Tasso annuo di crescita  percentuale = 6.82%", size = 8)+
  # labs(title = "Produzione scientifica dell'IZSLER", 
  #      subtitle = "N. di pubblicazioni su riviste peer-review indicizzate da Web of Science", 
  #      x = " Anno di pubblicazione", y = "n. di articoli")


  #scale_x_continuous(breaks=c(2005:2018))


# izsler %>% 
#   filter( PY<2022) %>% 
#   group_by(PY) %>% 
#   summarise(n=n()) %>% 
#   ggplot(aes(x=PY, y=n))+geom_point(stat = "identity")+
#   geom_line(stat="identity")+
#   labs(x="Anno di pubblicazione", y="Numero articoli pubblicati")+
#   scale_x_continuous(breaks=c(2018:2020))





# H <- Hindex(izsler)
# 
# H <- H[["H"]]
# 
# H$Cognome <- str_to_upper(H$Element)
# 
# 
# H <-  H %>%   mutate(Cognome = stri_extract_first(Cognome, regex='\\S+'))
# 
# ricIZSLER <- readRDS(here("data", "pub.rds"))
# 
# 
# 
# 
# ricIZSLER %<>% filter(!is.na(Matricola)) %>% 
#   select(OA, Cognome) %>% 
#   distinct() 
 
# 
# H %>% 
#   right_join(
#     ricIZSLER, by = c("Cognome", "PY_start" = "OA")
#   ) %>%  
#   filter(!is.na(Element)) %>% 
#   group_by(PY_start, Cognome) %>% 
#   
#   summarise(Hindex = mean(h_index)) %>% 
#   select(PY_start, Cognome, Hindex) %>% 
#   group_by(PY_start) %>% 
#   summarise(HINDEX = mean(Hindex))










#izsler_res <-biblioAnalysis(izsler, sep = ";")
# izsve_res <- biblioAnalysis(izsve,  sep = ";")

#S_izsler <- summary(izsler_res, k = 10)
# S_izsve <- summary(izsler_res, k = 10, pause = FALSE)



# x <- izspuglia %>% 
#   filter(PY >= 2016 & PY < 2021 )
# 




# pubrate(istituto = izsler)
# 
# 
# 
# 
# 
# 
#   
# IZS <-list(izsam,izsic, izsler, izslt, izsmezz, izspiem,  izspuglia, izssard, izsum, izsve)
# 
# 
# 
# gr <- lapply(IZS, pubrate)
# 
# grizs <- do.call(rbind, gr)
# 
# gr.frame <- data.frame( "Istituto" = c("IZSAM","IZSIC", "IZSLER", 
#                                        "IZSLT", "IZSMEZZ", "IZSPIEM",  "IZSPUGLIA", "IZSSARD", 
#                                        "IZSUM", "IZSVE"), 
#                         grizs)
# 
#  
# 
# 
#  
# 
#  
# p <- gr.frame %>% 
#   mutate(grizs = ifelse(Istituto == "IZSLER", 14.5, grizs)) %>% 
#   mutate(Istituto = fct_reorder(Istituto, grizs)) %>%  
#   ggplot(aes(x = Istituto, y = grizs, label=paste(round(grizs, 1),"%")))+
#   geom_point(size = 14, col = "lightblue")+
#   geom_text()+
#   coord_flip()+
#   geom_segment(aes(y=0, yend=grizs, x=Istituto, xend=Istituto), col= "darkgrey")+
#   theme_ipsum(axis_title_size = 15)+
#    #theme(axis.text.y = element_blank())+
#   labs(title = "Produzione scientifica degli IIZZSS: Tasso annuo di crescita  percentuale nel periodo 2018-2021", 
#        subtitle = "fonte dati: Web of Science", 
#        y = " Tasso annuale di crescita ", x = "")
# 
#  
# pimage <- axis_canvas(p, axis = 'y') +
#   draw_image("Valutazione ricercatori/izsumb.png", y= -22, scale = 3.5)+
#   draw_image("Valutazione ricercatori/izspuglia.jpg", y = 26, scale = 2.8)+
#   draw_image("Valutazione ricercatori/izsve.png", y = 7.2, scale = 3)+
#   draw_image("Valutazione ricercatori/izspiem.jpg", y= -1.5, scale = 3)+
#   draw_image("Valutazione ricercatori/izsam.jfif", y = 1.9, scale = 2.2)+
#   draw_image("Valutazione ricercatori/izsler.png", y = 8.5, scale = 3)+
#   draw_image("Valutazione ricercatori/izslt.png", y = 8.6, scale = 1.5)+
#   draw_image("Valutazione ricercatori/izsmezz.jpg", y = 25, scale = 3)+
#   draw_image("Valutazione ricercatori/izsicilia.jpg", y = 0, scale = 2)+
#   draw_image("Valutazione ricercatori/izsardegna.jpg", y = -5.5, scale= 2)
# 
# 
# ggdraw(insert_yaxis_grob(p, pimage, position = "left"))
#   
#   
###################################################
#############da pubblicazioni biblioteca########



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

####TEXT MINING#####
tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}

key<-data.frame(doc_id=seq(1:nrow(izsler)),text=izsler$ID)
key<-na.omit(key)
custom.stopwords<-c(stopwords('english'))
corpus <- VCorpus(DataframeSource(key))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.key.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.key.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:50,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


izsve<- readFiles("izsve.bib")
izsve <- convert2df(izsve, dbsource = "scopus", format = "bibtex")

izsam<- readFiles("izsam.bib")
izsam <- convert2df(izsam, dbsource = "scopus", format = "bibtex")

izsto<- readFiles("izsto.bib")
izsto <- convert2df(izsto, dbsource = "scopus", format = "bibtex")

####produttività###




##################################
izsler %>% 
  filter(PY <2023 & PY >2018) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>%  
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% View()
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity", fill="red")+coord_flip()








# izsve %>% 
#   filter(PY < 2022) %>% 
#   group_by(SO) %>% 
#   summarise(n=n()) %>% 
#   top_n(10, n) %>% 
#   arrange(n) %>% 
#   mutate(SO = factor(SO, unique(SO))) %>% 
#   ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()
# 
# izsam %>% 
#   filter(PY < 2022) %>% 
#   group_by(SO) %>% 
#   summarise(n=n()) %>% 
#   top_n(10, n) %>% 
#   arrange(n) %>% 
#   mutate(SO = factor(SO, unique(SO))) %>% 
#   ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()


j<-izsler%>% 
  filter(PY <2023 & PY >2018) %>% 
  group_by(PY, SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) 


# jve<-izsve%>% 
#   filter(PY < 2022) %>% 
#   group_by(PY, SO) %>% 
#   summarise(n=n()) %>% 
#   arrange(n) %>% 
#   top_n(10, n)


Jizsler<-get_impactfactor(j$SO, max.distance = 3)

Jizve<-get_impactfactor(jve$SO, max.distance = 3)

a<-Jizsler %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()

b<-Jizve %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()



# 
# topAU <- authorProdOverTime(izsler, k = 10, graph = TRUE)
# 
# a<-biblioAnalysis(izsler)
# L <- lotka(a)
# 
# A<-cocMatrix(izsler, Field = "SO", sep = ";")
# sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

##########CO-CITATION ANALYSIS#######
izsler<- readFiles("izsler.bib")
izsler <- convert2df(izsler, dbsource = "scopus", format = "bibtex")



net<-NetMatrix <- biblioNetwork(izsler, analysis = "co-citation", 
                           network = "references", sep = ";")

networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=0.7,edgesize = 10, 
                edges.min=5)


M=metaTagExtraction(izsler,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)

######################################

library(rAltmetric)
acuna <- altmetrics(doi = "10.1179/135100002125000406")


izsler<-biblioAnalysis(M,sep=";")
S<-summary(object = izsler, k = 10, pause = FALSE)

DF<-dominance(izsler, k = 10)


authors<-gsub(","," ",names(izsler$Authors)[1:15])
indices<-Hindex(M, authors, sep = ";")
indices$H
####################################################################


