library(tidyverse)
library(here)
library(ggrepel)
library(readxl)
library(hrbrthemes)
abstrEst <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                       sheet = "esterni")



abstrInt <- read_excel("data/valutazione abstract approvati da ministero.xlsx", 
                                                                sheet = "interni")





dt <- abstrEst %>% 
    pivot_longer(cols = 7:10, names_to = "area", values_to = "score")  %>%
    bind_rows(
      abstrInt %>%
        pivot_longer(cols = 7:11, names_to = "area", values_to = "score")
    )



library(GGally)
 x <- abstrEst[, 7:10]
 
x <- x %>% 
  mutate_if(is.numeric, factor)
library(GDAtools)
ggpairs(x, 
        axisLabels = "internal",
        showStrips = TRUE, 
        upper = list( discrete = ggassoc_crosstab))

dt <- abstrEst %>% 
pivot_longer(cols = 7:10, names_to = "area", values_to = "score") %>% 
  mutate(score2 = ifelse(score == 1, "scarso", 
                                                ifelse(score == 2, "sufficiente",
                                                       ifelse(score == 3, "buono",
                                                              ifelse(score == 4, "molto buono", "eccellente"))))) %>% 
  mutate(score2 = factor(score2, levels= c("scarso","sufficiente", "buono","molto buono" ,"eccellente" )))

dt %>% 
  filter(area == "Impatto" ) %>% 
  ggplot(aes(x=score2))+
  geom_bar()+  coord_flip()+
  facet_wrap(~Progetto)
 


ggpairs(tips[, 3:6], 
        axisLabels = "internal",
        showStrips = TRUE)



library("gplots")

library(fastDummies)

dummydt <- abstrEst %>%
  select(7:10) %>%
  dummy_cols(remove_selected_columns = TRUE)



df<-data.frame(dt[, 9],dummydt)






tabella <-  df %>%
  group_by(ageclass) %>%
  summarise_all(sum, na.rm = T)  %>%
  select(ageclass, ends_with("P")) %>% View()
  column_to_rownames(var="ageclass") %>%
  as.data.frame() %>%
  select(1:5)


















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

