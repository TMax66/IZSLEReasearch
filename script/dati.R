library(tidyverse)
library(bibliometrix)
library(here)

izsler <- here("data", c("izsler.bib"))
izsler <- convert2df(izsler, dbsource = "wos", format = "bibtex")

izsve <- here("data", c("izsve1.bib", "izsve2.bib"))
izsve <- convert2df(izsve, dbsource = "wos", format = "bibtex")

izsam <- here("data", c("izsete.bib"))
izsam <- convert2df(izsam, dbsource = "wos", format = "bibtex")

izsplv <- here("data", c("izsplv.bib"))
izsplv <- convert2df(izsplv, dbsource = "wos", format = "bibtex")

izsum <- here("data", c("izsum.bib"))
izsum <- convert2df(izsum, dbsource = "wos", format = "bibtex")

izsicilia <- here("data", c("izsicilia.bib"))
izsicilia <- convert2df(izsicilia, dbsource = "wos", format = "bibtex")

izslt <- here("data", c("izslt.bib"))
izslt <- convert2df(izslt, dbsource = "wos", format = "bibtex")

izsmezz <- here("data", c("izsmezz.bib"))
izsmezz <- convert2df(izsmezz, dbsource = "wos", format = "bibtex")

izspubas <- here("data", c("izspubas.bib"))
izspubas <- convert2df(izspubas, dbsource = "wos", format = "bibtex")

izssardegna <- here("data", c("izssard.bib"))
izssardegna <- convert2df(izssardegna, dbsource = "wos", format = "bibtex")







A <- izsler %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
A$Istituto<-rep("izsler", dim(A)[1])

# A <- A %>%
#   mutate(n = c(88, 125, 141, 136,98))


B <- izsve %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>%  
  group_by(PY) %>%
  summarise(n=n())    
B$Istituto<-rep("izsve", dim(B)[1])

C <- izsam %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>%  
  group_by(PY) %>%
  summarise(n=n())
C$Istituto<-rep("izsam", dim(C)[1])


D <- izsplv %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
D$Istituto<-rep("izsplv", dim(D)[1])


E <- izsicilia %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
E$Istituto<-rep("izsicilia", dim(E)[1])


F <- izslt %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
F$Istituto<-rep("izslt", dim(F)[1])


G <- izsmezz %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
G$Istituto<-rep("izsmezz", dim(G)[1])

H <- izssardegna %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
H$Istituto<-rep("izssard", dim(H)[1])

I <- izspubas %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
I$Istituto<-rep("izspubas", dim(I)[1])

L <- izsum%>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
L$Istituto<-rep("izsum", dim(L)[1])

prod<-rbind(A,B,C,D, E, F, G, H, I, L)
prod


A %>% 
bind_rows(B) %>% 
  bind_rows(C) %>% 
  bind_rows(D) %>% 
  bind_rows(E) %>% 
  bind_rows(F) %>% 
  bind_rows(G) %>% 
  bind_rows(H) %>% 
  bind_rows(I) %>% 
  bind_rows(L) -> prod
