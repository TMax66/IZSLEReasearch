library(RISmed)
library(dplyr)
library(ggplot2)
library(tm)
library(fpc)
library(wordcloud)
library(ggthemes)
library(igraph)
library(qdap)
library(dendextend)
library(circlize)






res1 <- EUtilsSummary("text mining",
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 5000,
                      mindate = 2000, 
                      maxdate = 2018)

fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")


abstracts <- data.frame(title = fetch@ArticleTitle,
                        #author=fetch@Author,
                        abstract = fetch@AbstractText, 
                        journal = fetch@Title,
                        DOI = fetch@PMID, 
                        year = fetch@YearPubmed)
## ensure abstracts are character fields (not factors)
abstracts <- abstracts %>% mutate(abstract = as.character(abstract))

journal <- MedlineTA(fetch)

jcount <- as.data.frame(table(journal))
j25 <-jcount[order(-jcount[,2]),][1:25,]

j25 %>% 
 mutate("%"=Freq/sum(Freq)) %>% 
  mutate(journal = factor(journal, unique(journal))) %>% 
  ggplot(aes(x=journal, y=Freq))+geom_bar(stat="identity")+coord_flip()


output$distPlot <- renderPlot({
  

  fetch <- EUtilsGet(res1, type="efetch", db="pubmed")
  count<-table(YearPubmed(fetch))
  count<-as.data.frame(count)
  names(count)<-c("Year", "Counts")
  cum <- data.frame(Year=count$Year, Counts=cumsum(count$Counts)) 
  cum$g <- "g"
  names(cum) <- c("Year", "Counts", "g")
 
  
  ggplot(count, aes(x=Year, y=Counts))+geom_bar(stat="identity")
  ggtitle(paste("PubMed articles containing \'", word1(), "\' ", "= ", max(num$Counts), sep="")) +
    ylab("Number of articles") +
    xlab(paste("Year \n Query date: ", Sys.time(), sep="")) +
    labs(colour="") +
    theme_bw() 
  
  
  
  
  q 
})






tally <- array()
x <- 1
for (i in 2000:2018){
  Sys.sleep(1)
  r <- EUtilsSummary('text mining', type='esearch', db='pubmed', mindate=i, maxdate=i)
  tally[x] <- QueryCount(r)
  x <- x + 1
}

names(tally) <- 1970:2013
max(tally)
barplot(tally, las=2, ylim=c(0,600), main="Number of PubMed articles containing retrotransposon")



r <- EUtilsSummary('retrotransposon', type='esearch', db='pubmed', retmax = 5000,mindate=2018, maxdate=2018)





library(qdap)
myFunc<-function(argument){
  articles1<-data.frame('Abstract'=AbstractText(fetch), 'Year'=YearPubmed(fetch))
  abstracts1<-articles1[which(articles1$Year==argument),]
  abstracts1<-data.frame(abstracts1)
  abstractsOnly<-as.character(abstracts1$Abstract)
  abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
  abstractsOnly<-as.vector(abstractsOnly)
  abstractsOnly<-strip(abstractsOnly)
  stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top100Words)
  ord<-as.data.frame(table(stsp))
  ord<-ord[order(ord$Freq, decreasing=TRUE),]
  head(ord,20)
}
#####################################
#####################################

tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

clean.corpus<-function(corpus){
  #corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus, PlainTextDocument)
  corpus<-tm_map(corpus, toSpace, "-")
  corpus<-tm_map(corpus, toSpace, ":")
  corpus<-tm_map(corpus,stemDocument)
 
  
  return(corpus)
}



q1<-data.frame(doc_id=seq(1:nrow(abstracts)),text=abstracts$abstract)


#q1$text <-as.character(q1$text)
  
  
custom.stopwords<-c(stopwords('english'))
corpus <- VCorpus(DataframeSource(q1))
corpus<-clean.corpus(corpus)
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.q1.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q1.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


