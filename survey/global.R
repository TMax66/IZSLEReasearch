library(shiny)
library(shinydashboard)
library (DT)
library(plotly)
library(reshape2)
library(data.table)
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(scales)
library(pivottabler)
library(htmlwidgets)
library(gridExtra)
library(forecast)
library(xts)
library(dygraphs)
library(datasets)
library(tidyverse)
library(googlesheets)
#library(MASS)


######################################################################
fieldsMandatory <- c("azienda", "numconf")
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsAll <- c("codaz", "dt","vetlp", "vetuff", "numconf", "numcamp", "numpos","pt")
responsesDir <- file.path("mydata")


# riskMandatory<-c("Data compilazione")
# 
# rlabelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "rmandatory_star")
#   )
# }

# riskfields<-c("riskazienda", "dtrisk", "A1", "A2","A3", "A4","A5","A6","A7","A8",
#               "B1", "B2","B3", "B4","B5","B6",
#               "C1", "C2","C3", "C4","C5",
#               "D1", "D2","D3", "D4","D5",
#               "E1", "E2","E3", "E4",
#               "F1", "F2","F3", "F4")
# 
# riskdir<-file.path("riskdata")


#####FUNZIONE LOAD DATA- CARICA IL FILE DOPO L'AGGIUNTA DI NUOVI RECORD#######
loadData <- function() {
    sheet <- gs_title("paratbc")
    data<-gs_read_csv(sheet)
    data$dt<-as.Date(data$dt, origin=as.Date("1970-01-01"))
    data$codaz<-casefold(data$codaz, upper=TRUE)
    data$vetlp<-casefold(data$vetlp, upper=TRUE)
    data$vetuff<-casefold(data$vetuff, upper=TRUE)
    data%>%as.tibble()%>%
      mutate(prev=(numpos/numcamp)*100,
             prev=round(prev,2),
             anno=year(dt))%>%
      arrange(desc(dt))
  }

  
  






###################FUNZIONE LOAD RISK -CARICA I FILES DELLA VALUTAZIONE DEL RISCHIO#############


# loadRisk<-function(){
#   rfiles<-list.files(file.path("riskdata"), full.names=TRUE)
#   rdata<-lapply(rfiles, read.csv, stringsAsFactors=FALSE)
#   rdata<-do.call(rbind, rdata)
#   rdata$dtrisk<-as.Date(rdata$dtrisk, origin=as.Date("1970-01-01"))
#   rdata$riskazienda<-casefold(rdata$riskazienda, upper=TRUE)
#   righe<-dim(rdata)[1]
#   
#   settore<-rep(c(rep("Sala Parto", 8), 
#                  rep("Vitelli pre-svezzamento", 6), 
#                  rep("Vitelle/Manzette svezzate",5), 
#                  rep("Manze gravide",5),rep("Vacche e tori",4), 
#                  rep("Animali acquistati", 4)), righe)
#   settore<-factor(settore, levels=c("Sala Parto","Vitelli pre-svezzamento","Vitelle/Manzette svezzate","Manze gravide","Vacche e tori" ,"Animali acquistati"))
#   
#   
#   
#     rdata %>% as.tibble() %>% 
#     gather(key = item, value = risk, -riskazienda, -dtrisk) %>%
#     arrange(riskazienda) %>% 
#     cbind(settore)
# 
# }  
###########FUNZIONE PER CREARE TABELLA RISULTATI RISCHIO##########
#





#maxpunti<-c(80,60,35,25,16,60,276)

# riSrisk<-function(){
#   ris<-read.csv("riskRis.csv")
#   ris %>% 
#   group_by(riskazienda,settore)%>%
#     summarise(punteggio=sum(risk)) 


#}







humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


shinyjs::useShinyjs()




