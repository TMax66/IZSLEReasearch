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
# id <- "tthQ_DQAAAAJ&hl"
# 
# p <- get_publications(id)

source( "dati.R")

prod %>% 
  filter(PY< 2022 & Istituto == "izsler" ) %>% 
  ggplot(aes(x=PY, y=n, group=Istituto))+  
  labs(y="n.articoli", x="anno")+
  ylim(0, 130)+
  geom_smooth()+ geom_line() +geom_point()+
  theme_ipsum(axis_title_size = 15)+
  # geom_text(aes(x= 2019, y = 120), label = "Tasso annuo di crescita  percentuale = 6.82%", size = 8)+
  labs(title = "Produzione scientifica dell'IZSLER", 
       subtitle = "N. di pubblicazioni su riviste peer-review indicizzate da Web of Science", 
       x = " Anno di pubblicazione", y = "n. di articoli")
  


prod %>% 
  mutate(Istituto= recode(Istituto, izsler = "IZSLER", 
                          izsam = "IZSAM", 
                          izsve = "IZSVE", 
                          izsic = "IZS Sicilia", 
                          izslt = "IZSLT", 
                          izsmezz = "IZS Mezzogiorno", 
                          izspiem = "IZSTO", 
                          izspuglia = "IZS Puglia Basilicata", 
                          izssard = "IZS Sardegna", 
                          izsum = "IZSUM")) %>%  
  filter( PY < 2022 ) %>%  
  #mutate(lab = if_else(PY == max(PY), as.character(Istituto), NA_character_)) %>%  View()
  
  ggplot(aes(x=PY, y=n, label = n))+  
  labs(y="n.articoli", x="anno")+
  ylim(c(0,150))+
  geom_line()+ geom_point(size = 10, col = "lightgrey")+
  geom_text()+
  theme_ipsum(axis_title_size = 15)+
    theme(legend.position = "none")+facet_wrap(~Istituto, scales = "free")+
  labs(title = "Produzione scientifica degli IIZZSS periodo 2018-2021", 
       subtitle = "N. di pubblicazioni su riviste peer-review indicizzate da Web of Science", 
       x = " Anno di pubblicazione", y = "n. di articoli")


 
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



izsler_res <-biblioAnalysis(izsler, sep = ";")
izsve_res <- biblioAnalysis(izsve,  sep = ";")

S_izsler <- summary(izsler_res, k = 10)
S_izsve <- summary(izsler_res, k = 10, pause = FALSE)



x <- izspuglia %>% 
  filter(PY >= 2016 & PY < 2021 )



pubrate <- function(istituto)
{
  ist <- istituto %>% 
    filter(  PY < 2022 ) 
  
  M <- biblioAnalysis(ist, sep = ";" )
  
  Y <- data.frame(table(M$Years))
  
  ny <- max(as.numeric(levels(Y[,1])),na.rm=TRUE)-min(as.numeric(levels(Y[,1])),na.rm=TRUE)
  
  GR <- ((Y[nrow(Y),2]/Y[1,2])^(1/(ny))-1)*100
  
  GR

 
}


pubrate(istituto = izsler)






  
IZS <-list(izsam,izsic, izsler, izslt, izsmezz, izspiem,  izspuglia, izssard, izsum, izsve)



gr <- lapply(IZS, pubrate)

grizs <- do.call(rbind, gr)

gr.frame <- data.frame( "Istituto" = c("izsam","izsic", "izsler", 
                                       "izslt", "izsmezz", "izspiem",  "izspuglia", "izssard", "izsum", "izsve"), 
                        grizs)
 
p <- gr.frame %>% 
  mutate(Istituto = fct_reorder(Istituto, grizs)) %>%
  ggplot(aes(x = Istituto, y = grizs, label=paste(round(grizs, 1),"%")))+
  geom_point(size = 14, col = "lightblue")+
  geom_text()+
  coord_flip()+
  geom_segment(aes(y=0, yend=grizs, x=Istituto, xend=Istituto), col= "darkgrey")+
  theme_ipsum(axis_title_size = 15)+
  theme(axis.text.y = element_blank())+
  labs(title = "Produzione scientifica degli IIZZSS: Tasso annuo di crescita  percentuale nel periodo 2018-2021", 
       subtitle = "fonte dati: Web of Science", 
       y = " Tasso annuo di crescita  percentuale", x = "")

 
pimage <- axis_canvas(p, axis = 'y') +
  draw_image("Valutazione ricercatori/izsumb.png", y= -22, scale = 3.5)+
  draw_image("Valutazione ricercatori/izspuglia.jpg", y = 26, scale = 2.8)+
  draw_image("Valutazione ricercatori/izsve.png", y = 7.2, scale = 3)+
  draw_image("Valutazione ricercatori/izspiem.jpg", y= -1.5, scale = 3)+
  draw_image("Valutazione ricercatori/izsam.jfif", y = 1.9, scale = 2.2)+
  draw_image("Valutazione ricercatori/izsler.png", y = 8.5, scale = 3)+
  draw_image("Valutazione ricercatori/izslt.png", y = 8.6, scale = 1.5)+
  draw_image("Valutazione ricercatori/izsmezz.jpg", y = 25, scale = 3)+
  draw_image("Valutazione ricercatori/izsicilia.jpg", y = 0, scale = 2)+
  draw_image("Valutazione ricercatori/izsardegna.jpg", y = -5.5, scale= 2)


ggdraw(insert_yaxis_grob(p, pimage, position = "left"))
  
  
  
  
  
  
  








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
  filter(PY>=2005 & PY<=2018) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>%
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity", fill="red")+coord_flip()

izsve %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()

izsam %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()





j<-izsler%>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n)


jve<-izsve%>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n)


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


