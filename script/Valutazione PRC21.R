library(tidyverse)
library(here)
library(ggrepel)
library(readxl)
library(hrbrthemes)
abstrEst <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                       sheet = "esterni")
abstrInt <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                                                                sheet = "interni")

finanziati <- c(15,	8,	10,	4,	17,	5,	18,	12,	19,	13,	14,	11,	1,2  )
deletedpr <- c(3,5, 6, 7, 9, 11, 13, 19, 26,29)



abstrEst %>% 
  pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>% 
  # select(Progetto, area, score) %>% 
  # saveRDS(here("data","prc21.rds"))
  
  bind_rows(
    abstrInt %>% 
      pivot_longer(cols = 7:11, names_to = "area", values_to = "score")) %>%   








mutate( Classificazione = ifelse(Progetto %in% finanziati, "Finanziati", "Non Finanziati" ),
       Classificazione = ifelse(`Vecchia numerazione` %in% deletedpr, "Non Approvati", Classificazione), 
       
       Classificazione = factor(Classificazione, levels = c("Finanziati", "Non Finanziati","Non Approvati" ))
       ) %>%   
group_by(Progetto, Classificazione) %>% 
  summarise(MScore = round(mean(score, na.rm = TRUE),2), 
            Score = sum(score, na.rm = TRUE) )%>%   
 # pivot_longer(cols = 2:3, names_to = "Valutazione", values_to = "Punteggio") %>%  
 # filter(Valutazione == "Score") %>%   
  mutate(Progetto = factor(Progetto), 
         Progetto = recode(Progetto, "15" = "Filipello",
                           "8" = "Villa", 
                           "10" = "Lorenzi", 
                           "4" = "Pongolini",  
                           "17" = "Accurso", 
                           "5" = "Rugna", 
                           "18" = "Biancardi", 
                           "12" = "Pacciarini", 
                           "19" ="Gasparini", 
                           "13" = "Pezzoni",
                           "14" = "Gamba", 
                           "11" = "Boniotti",	
                            "1" = "Andreoli",
                            "2" = "Bertoletti",  
                            "7"=  "Rota Nodari",
                            "6"=  "Barbieri",
                            "16"= "Bertasi",
                            "9"= "Cacciamali",
                            "3"= "Maisano",
                            "23"= "Pongolini(2)",
                            "28"= "Benevenia",
                            "25"= "Calzolari",
                            "24"= "Garbarino",
                            "21"= "Prati",
                            "20"= "Gibelli",
                            "22"= "Faccini",
                            "29"= "Dalzini",
                            "26"= "Defilippo",
                            "27"= "D'Incau"))%>% View() 
  
  ggplot(aes(x = fct_reorder(Progetto, Score), y = Score, label = Score))+
  geom_point(size = 9.5,  aes(colour = Classificazione), alpha = 0.8)+
  scale_colour_manual(values = c("steelblue","lightblue", "grey"))+
  geom_text(size = 4)+
  geom_segment(aes(y = 0, x = Progetto, yend = Score-1, xend = Progetto, colour = Classificazione), linetype = 3)+ 
  coord_flip()+ facet_wrap(Classificazione ~ ., ncol = 1, scales = "free_y",strip.position = "top")+#, space = "free_y")+
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), 
        legend.position = "blank", 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white")) +
  #theme_ipsum(axis_title_size = 15)+
  #theme_minimal() + 
  labs(title = "", y = "Punteggio", x = "Responsabile Scientifico")+
  theme(strip.placement = "outside")+ ylim (0, 90) + geom_hline(yintercept = 85, colour = "red", linetype = "dashed")+
  scale_y_continuous(breaks = c(seq(0, 80, by=20), 85))
 
 

 

rank <-  c("15",	"8",	"10",	"4",	"17",	"5",	"18",	"12",	"19",	"13",	"14",	"11",	"1","2",
           "7","6","16","9","3","23","28","25","24","21","20","22","29","26","27")
finanziati <- c(15,	8,	10,	4,	17,	5,	18,	12,	19,	13,	14,	11,	1,2  )
nonfinanziati <- c(7, 6, 16, 9, 3)
nonaprov <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29)

abstrEst %>% 
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
  mutate(Progetto = factor(Progetto, levels = rank), 
         Classificazione = ifelse(Progetto %in% finanziati, "Finanziato", 
                                  ifelse(Progetto %in%  nonfinanziati, "Non Finanziato", 
                                        "Non Approvato")), 
         Classificazione = factor(Classificazione, levels = c("Finanziato", "Non Finanziato","Non Approvato" )), 
         Progetto = recode(Progetto, "15" = "Filipello",
                           "8" = "Villa", 
                           "10" = "Lorenzi", 
                           "4" = "Pongolini",  
                           "17" = "Accurso", 
                           "5" = "Rugna", 
                           "18" = "Biancardi", 
                           "12" = "Pacciarini", 
                           "19" ="Gasparini", 
                           "13" = "Pezzoni",
                           "14" = "Gamba", 
                           "11" = "Boniotti",	
                           "1" = "Andreoli",
                           "2" = "Bertoletti",  
                           "7"=  "Rota Nodari",
                           "6"=  "Barbieri",
                           "16"= "Bertasi",
                           "9"= "Cacciamali",
                           "3"= "Maisano",
                           "23"= "Pongolini(2)",
                           "28"= "Benevenia",
                           "25"= "Calzolari",
                           "24"= "Garbarino",
                           "21"= "Prati",
                           "20"= "Gibelli",
                           "22"= "Faccini",
                           "29"= "Dalzini",
                           "26"= "Defilippo",
                           "27"= "D'Incau"))%>%   
  ggplot(aes(Giudizio, Item, label = n)) + 
  geom_tile( fill = "white", alpha = 5)+ geom_text(size = 6)+
  facet_wrap(Classificazione~Progetto)+
  theme_bw()+
  theme(legend.position = "blank", 
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=20))
  








# abstrInt %>% 
#   select(1, 7:11) %>% 
#   mutate_all(factor) %>%
#   pivot_longer(cols = 2:6, names_to = "Item", values_to = "Freq") %>% 
#   group_by(Item, Freq) %>% 
#   count() %>%  
#   summarise(S = n/29) %>% 
#   mutate(Freq = factor(Freq, levels =c("3", "3.5", "4", "4.5", "5"))) %>% 
#   ggplot(aes(x=Freq, y=S))+
#   geom_bar(stat = "identity")+
#   facet_wrap(~Item)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # abstrInt <- abstrInt %>% 
# #   rename("Chiarezza della proposta" = "Chiarezza obiettivi", 
# #          "Fattibilità" = "Congruità economica") %>% 
# #   select(-7)
# 
# 
# 
# 
# # dt <- abstrEst %>% 
# #     pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>%
# #     bind_rows(
# #       abstrInt %>%
# #         pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
# #     )
# 
# 
# 
# # library(GGally)
# x <- abstrEst[, 7:10]
# 
# x <- x %>%
#   mutate_if(is.numeric, factor)
# library(GDAtools)
# ggpairs(x,
#         axisLabels = "internal",
#         showStrips = TRUE,
#         upper = list( discrete = ggassoc_crosstab))
# 
# 
# dt <- abstrEst %>%
#   pivot_longer(cols = 7:10, names_to = "area", values_to = "score") %>%
#   mutate(score2 = ifelse(score == 1, "scarso",
#     ifelse(score == 2, "sufficiente",
#       ifelse(score == 3, "buono",
#         ifelse(score == 4, "molto buono", "eccellente")
#       )
#     )
#   )) %>%
#   mutate(score2 = factor(score2, levels = c("scarso", "sufficiente", "buono", "molto buono", "eccellente")))
# 
# #MCA----
# library("FactoMineR")
# library("factoextra")
# 
# # dt <- abstrEst %>% 
# #   mutate(across(7:10, ~recode(., "1" =  "scarso", 
# #                              "2" =  "sufficiente", 
# #                              "3" = "buono", 
# #                              "4" = "molto buono", 
# #                              "5" = "eccellente")),  
# #          Progetto = as.character(Progetto), 
# #          referee = as.character(referee)) %>% 
# #   select(1, 6:10)
# 
# 
# 
# # x <- MCA(dt[, -c(2)] , quali.sup = 1,   graph = FALSE)
# # fviz_mca_var(x, choice = "mca.cor", 
# #              repel = TRUE, # Avoid text overlapping (slow)
# #              ggtheme = theme_minimal())
# # 
# # var <- get_mca_var(x)
# # head(var$cos2, 4)
# # 
# # fviz_cos2(x, choice = "var")
# # fviz_mca_var(x, 
# #              repel = TRUE, # Avoid text overlapping (slow)
# #              ggtheme = theme_minimal(), m)
# # 
# # 
# # 
# # fviz_mca_var(x, col.var = "cos2",
# #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
# #              repel = TRUE, # Avoid text overlapping
# #              ggtheme = theme_minimal())
# # 
# # 
# # fviz_mca_var(x, col.var = "contrib",
# #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07", "#FC0000"), 
# #              repel = TRUE, # avoid text overlapping (slow)
# #              ggtheme = theme_minimal())
# # 
# # fviz_mca_biplot(x, repel = TRUE,
# #                 ggtheme = theme_minimal())
# # 
# # 
# # fviz_mca_ind(x, col.ind = "cos2", 
# #              #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
# #              repel = TRUE, # Avoid text overlapping (slow if many points)
# #              ggtheme = theme_minimal())
# # 
# 
# 
#  
#  
#    
#    
#    
#      
#      
#    
#   
#  
# 
# 
# 
# 
# 
# 
# dt %>%
#   group_by(area, score2, Progetto) %>% 
#   summarise(n= n()) %>% 
#   ggplot(aes(score2,area , label = n)) +
#   geom_tile( fill = "white")+
#   facet_wrap(~Progetto)+
#   geom_text()+
#   theme_ipsum_rc()
#   
# 
# 
# 
# 
#   # #filter(area == "Impatto" ) %>%
#   # ggplot(aes(x=score2))+
#   #    facet
#   # 
#   # geom_bar()+  coord_flip()+ labs(x = "")+
#   # facet_grid(area~Progetto)
# 
# 
# 
# x <- LETTERS[1:20]
# y <- paste0("var", seq(1,20))
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)
# 
# ggplot(data, aes(X, Y, fill= Z)) + 
#   geom_tile()
# 
# # 
# # ggpairs(tips[, 3:6], 
# #         axisLabels = "internal",
# #         showStrips = TRUE)
# # 
# # 
# # 
# # library("gplots")
# # 
# # library(fastDummies)
# # 
# # dummydt <- abstrEst %>%
# #   select(7:10) %>%
# #   dummy_cols(remove_selected_columns = TRUE)
# # 
# # 
# # 
# # df<-data.frame(dt[, 9],dummydt)
# # 
# # 
# # 
# # 
# # 
# # 
# # tabella <-  df %>%
# #   group_by(ageclass) %>%
# #   summarise_all(sum, na.rm = T)  %>%
# #   select(ageclass, ends_with("P")) %>% View()
# #   column_to_rownames(var="ageclass") %>%
# #   as.data.frame() %>%
# #   select(1:5)
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

# abstrInt %>% 
#   select(-`Attinenza linee ministeriali`) %>% 
#   rename()


# dt <- abstrEst %>% 
#   pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>%
#   bind_rows(
#     abstrInt %>% 
#       pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
#   ) %>% 
#   pivot_wider(names_from = "area", values_from = "score") %>% View()
#    



# dt <- abstrEst %>% 
#   select(7:10) %>% 
#   mutate_all(factor)



# pivot_longer(cols = 7:10, names_to = "area", values_to = "score") %>% 
# mutate(score2 = ifelse(score == 1, "scarso", 
#                        ifelse(score == 2, "sufficiente",
#                               ifelse(score == 3, "buono", 
#                                      ifelse(score == 4, "molto buono", "eccellente"))))) %>% 
#  mutate(id = seq(1:nrow(.))) %>% 
# select(id, area, score2) %>% 
#  pivot_wider(names_from = "area", values_from = score2) %>% distinct()

