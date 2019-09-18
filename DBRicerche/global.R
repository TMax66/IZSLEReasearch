library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)
library(readxl)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(ggpubr)

ds <- read_excel("prizsler.xlsx")

ds2<-ds %>% 
  filter(Tipologia=="Corrente")

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



title<-data.frame(doc_id=seq(1:nrow(ds2)),text=ds2$Titolo)
custom.stopwords<-c(stopwords('italian'),"valutazione", "studio","animali", "animale","punto",
                    "messa","progetto","ricerca", "finanziamento", "specie", "test", "riferimento",
                    "particolare", "spp", "agenti", "mediante", "ceppi", "utilizzo", "protocolli",
                    "analisi")
corpus <- VCorpus(DataframeSource(title))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "prodotti", replacement = "produzione")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "metodiche", replacement = "metodi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "suini", replacement = "suino")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "diagnostici", replacement = "diagnosi")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "allevamenti", replacement = "allevamento")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "virali", replacement = "virus")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.title.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.title.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))


# ######TOPICS MODEL#####

dtm<-DocumentTermMatrix(corpus, control=list(weighting=weightTf))
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5

ldaOut <-LDA(dtm,k, method="Gibbs",
             control=list(nstart=nstart,
                          seed = seed, best=best,
                          burnin = burnin, iter = iter, thin=thin))

topics <- tidy(ldaOut, matrix = "beta")
ldaOut.topics <- as.matrix(topics(ldaOut))

ldaOut.terms <- as.matrix(terms(ldaOut,10))
# 
# 


# 
# rowTotals <- apply(dtm , 1, sum)
# dtm<-dtm.new<- dtm[rowTotals> 0, ]
# 
# 
# topic <- LDA(dtm, k = 5, control = list(seed = 1234))
# 
# topic <- tidy(topic, matrix = "beta")
# 
# ap_top_terms <- topic %>%
#   group_by(topic) %>%
#   top_n(20, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# ap_top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   ggplot(aes(term, beta)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# 
# beta_spread <- topic %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1)) %>%
#   group_by(log_ratio < 0) %>%
#   top_n(15, abs(log_ratio)) %>%
#   ungroup() %>%
#   mutate(word = reorder(term, log_ratio)) %>%
#   ggplot(aes(word, log_ratio)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   ylab("log odds ratio")
# 
# beta_spread %>%
#   group_by(log_ratio < 0) %>%
#   top_n(15, abs(log_ratio)) %>%
#   ungroup() %>%
#   mutate(word = reorder(term, log_ratio)) %>%
#   ggplot(aes(word, log_ratio)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   ylab("log odds ratio")
