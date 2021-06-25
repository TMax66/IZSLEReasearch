source("librerie.R")
 
###RATING RICERCATORI ####
##### da Incites ####
ricercatori <- read_excel(here("Valutazione ricercatori", "ricercatori_IZSLER_2016_2020.xls"))
# #anag <- readRDS(here("programmazione", "data", "processed", "ANAGRAFE.rds"))
# anag <- anag %>% 
#   select(-REPARTO, -CENTRO_DI_COSTO) 

ricercatori$Cognome <- gsub(",.*$", "", ricercatori$Name)
x <- ricercatori %>%
mutate(Cognome = recode(Cognome, "Moreno" = "Moreno Martin", 
                          "Martin" = "Moreno Martin", 
                          "Cosciani-Cunico" = "Cosciani Cunico", 
                          "Silvia" = "Dotti"
                          ))%>% 
select(58, 3, 5, 7, 19, 27, 29,24, 34,37,38,  40, 43 ,  4, 6, 10, 12, 14, 15, 18, 23, 28, 32, 33, 39, 41)


df <- x %>% 
group_by(Cognome) %>% 
  summarise_at(1:13, sum) %>% 
  left_join(
    (x %>% 
      group_by(Cognome) %>% 
      summarise_at(14:25, mean)), by= "Cognome"
  )

df <- df %>% 
  mutate_if(is.numeric, scale2) %>% 
  column_to_rownames( "Cognome")
 


res.pca <- PCA(df, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.5,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

head(res.hcpc$data.clust, 10)

res.hcpc$desc.var$quanti

res.hcpc$desc.ind$para


cor.mat <- round(cor(df),2)
head(cor.mat[, 1:6])
 
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(df, histogram=TRUE, pch=19)
