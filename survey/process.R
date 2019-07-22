#questo script serve per preparare il file archivio.csv che va nella cartella mydata###
#non deve girare con l'app shiny
####e' qui per promemoria nel caso fosse necessario ricostruire il file archivio#####
#####################ARCHIVIO.CSV##################################
###################################################################
#df<-read.csv("datiperarchivio.csv", sep=';', header=T, dec=',')  #
#df$dt<-as.Date(df$dt, format="%d/%m/%Y")                         #
#library(epitools)                                                #
#df$dt<-julian.Date(df$dt)                                        #
#write.csv(df, file='archivio.csv', row.names=FALSE, quote=TRUE)  #
###################################################################
#################################################################################################
#################################################################################################
# library(dplyr)
# 
# df<-read.csv("backup.csv")
# df%>%as.tibble()%>%
#   group_by(anno, pt)%>%
#   summarise(totaz=n())%>%
#   ggplot(aes(pt,totaz))+geom_bar(stat = "identity")+facet_grid(~anno)+coord_flip()
# 
# pt <- PivotTable$new()
# pt$addData(df)
# pt$addColumnDataGroups("anno",addTotal=FALSE)
# pt$addRowDataGroups("pt")
# pt$defineCalculation(calculationName="n", summariseExpression="n()")
# pt$renderPivot()
# 
# 
# df%>%as.tibble%>%
#    summarize(parziale=n())
#   
# df%>%as.tibble()%>%
# 
#   select(codaz, vetlp,dt,prev,pt)%>%
#   filter(codaz=="025BG001")

#############################################################################
library(googlesheets)
archivio<-read.csv("archivio.csv")
gs_new("paratbc", ws_title = "paratbc", input = archivio,trim = TRUE, verbose = FALSE)


