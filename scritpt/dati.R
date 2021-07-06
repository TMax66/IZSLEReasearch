izsler <- here("data", c("izsler1.bib","izsler2.bib","izsler3.bib"))
izsler <- convert2df(izsler, dbsource = "wos", format = "bibtex")


izsve <- here("data", c("izsve1.bib","izsve2.bib","izsve3.bib", "izsve4.bib"))
izsve <- convert2df(izsve, dbsource = "wos", format = "bibtex")


izsam <- here("data", c("izsam1.bib","izsam2.bib"))
izsam <- convert2df(izsam, dbsource = "wos", format = "bibtex")


izspiem <- here("data", c("izspiem1.bib","izspiem2.bib"))
izspiem <- convert2df(izspiem, dbsource = "wos", format = "bibtex")

izsumbmarch <- here("data", c("izsumbmarch1.bib","izsumbmarch2.bib"))
izsumbmarch <- convert2df(izsumbmarch, dbsource = "wos", format = "bibtex")

izsicilia <- here("data", c("izsicilia1.bib","izsicilia2.bib"))
izsicilia <- convert2df(izsicilia, dbsource = "wos", format = "bibtex")

izslt <- here("data", c("izslt1.bib","izslt2.bib"))
izslt <- convert2df(izslt, dbsource = "wos", format = "bibtex")

izsmezz <- here("data", c("izsmezz1.bib","izsmezz2.bib"))
izsmezz <- convert2df(izsmezz, dbsource = "wos", format = "bibtex")

izspuglia <- here("data", c("izspuglia.bib"))
izspuglia <- convert2df(izspuglia, dbsource = "wos", format = "bibtex")

izssard <- here("data", c("izssard.bib"))
izssard <- convert2df(izssard, dbsource = "wos", format = "bibtex")




A <- izsler %>% 
  group_by(PY) %>% 
  summarise(n=n())   
A$Istituto<-rep("izsler", dim(A)[1]) 

B <- izsve %>% 
  group_by(PY) %>% 
  summarise(n=n())   
B$Istituto<-rep("izsve", dim(B)[1])

C <- izsam %>% 
  group_by(PY) %>% 
  summarise(n=n())  
C$Istituto<-rep("izsam", dim(C)[1])


D <- izspiem %>% 
  group_by(PY) %>% 
  summarise(n=n())
D$Istituto<-rep("izspiem", dim(D)[1])


E <- izsicilia %>% 
  group_by(PY) %>% 
  summarise(n = n())

E$Istituto<-rep("izsicilia", dim(E)[1])


F <- izslt %>% 
  group_by(PY) %>% 
  summarise(n = n())
F$Istituto<-rep("izslt", dim(F)[1])


G <- izsmezz %>% 
  group_by(PY) %>% 
  summarise(n = n())
G$Istituto<-rep("izsmezz", dim(G)[1])

H <- izssard %>% 
  group_by(PY) %>% 
  summarise(n = n())
H$Istituto<-rep("izssard", dim(H)[1])

I <- izspuglia %>% 
  group_by(PY) %>% 
  summarise(n = n())
I$Istituto<-rep("izspuglia", dim(I)[1])  

L <- izsumbmarch%>% 
  group_by(PY) %>% 
  summarise(n = n())
L$Istituto<-rep("izsumbmarche", dim(L)[1])

prod<-rbind(A,B,C,D, E, F, G, H, I, L)