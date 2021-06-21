library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")
library("DT")
library("lubridate")
library("fmsb")
library("readr")
library("directlabels")
library("ggrepel")
library("broom")
library("forcats")
library("patchwork")
library("hrbrthemes")
##PROGETTI DI RICERCA####





###RATING RICERCATORI ####
##### da Incites ####
ricercatori <- read_excel(here("Valutazione ricercatori", "ricercatori.xlsx"))
ricercatori <- ricercatori[-c(1259:1268),]
anag <- readRDS(here("programmazione", "data", "processed", "ANAGRAFE.rds"))
anag <- anag %>% 
  select(-REPARTO, -CENTRO_DI_COSTO) 

ricercatori$Cognome <- gsub(",.*$", "", ricercatori$Name)
ricercatori <- ricercatori %>%
mutate(Cognome = recode(Cognome, "Moreno" = "Moreno Martin", 
                          "Martin" = "Moreno Martin", 
                          "Elisabetta" = "Caprai", 
                          "Cosciani-Cunico" = "Cosciani Cunico", 
                          ))


ricercatori <- ricercatori %>% 
  mutate(Cognome = toupper(Cognome)) %>% 
  left_join(
    (anag %>% 
       select(Dipartimento, Matricola, Cognome, Dirigente) %>% 
       na.omit()
    ), 
    by= "Cognome")


ric <- ricercatori %>%
  filter(!Dipartimento %in% c("DIREZIONE GENERALE", "DIPARTIMENTO AMMINISTRATIVO",
                              "DIREZIONE AMMNINISTRATIVA") &
           !is.na(Dipartimento)) %>%
  group_by(Dipartimento, anno =`Publication Year`, Cognome) %>%
  summarise(doc = sum(`Web of Science Documents`),
            cit = mean(`Times Cited`),
            citImp = mean(`Citation Impact`),
            NcitImp = mean(`Category Normalized Citation Impact`),
            Intcoll = sum(`International Collaborations`)) %>%
  group_by(Dipartimento, anno) %>%
  summarise(Docs = mean(doc),
            Cits = mean(cit),
            ImpCits = mean(citImp),
            Intcolls = mean(Intcoll),
            NcitImps = mean(NcitImp)) %>% 
  mutate(Dip = abbreviate(Dipartimento))







pdocs <- prj_plot(dati = ric, par = ric$Docs, par2 = "N.pubblicazioni", metodo = "glm")

pcits <- prj_plot(dati = ric, par = ric$Cits, par2 = "Citazioni", metodo = "glm")

pimpcits <- prj_plot(dati = ric, par = ric$ImpCits, par2 = "Impatto Citazionale", metodo = "glm")

Npimpcits <- prj_plot(dati = ric, par = ric$NcitImps, par2 = "Impatto Citazionale Normalizzato", metodo = "glm")

pintcoll <- prj_plot(dati = ric, par = ric$Intcolls, par2 = "Collaborazioni Internazionali", metodo = "glm")
  


pdocs/pcits/pimpcits |Npimpcits/pintcoll

c <-list()
dip <- c("DIPARTIMENTO TUTELA E  SALUTE ANIMALE", "DIPARTIMENTO SICUREZZA ALIMENTARE", 
         "AREA TERRITORIALE LOMBARDIA", "AREA TERRITORIALE EMILIA ROMAGNA", "DIREZIONE SANITARIA" )

cit_coef <- function(dati, dip)
{  
  c <- coef(lm(NcitImps~anno, data = subset(dati, dati$Dipartimento == dip)))[2]
}

for (i in 1:5) { 
  c[[i]]<- cit_coef(dati= ric, dip[i])
}

docs <- data.frame(dip,  do.call(rbind, c))
cits <- data.frame(dip,  do.call(rbind, c))
impcits <- data.frame(dip,  do.call(rbind, c))
intcolls <- data.frame(dip,  do.call(rbind, c))
nimpcits <- data.frame(dip,  do.call(rbind, c))

CIT <- data.frame("Npub"=docs, "Cits"=cits[,2], "Imp"=impcits[,2], "CollInt" = intcolls[,2], "NormImp" = nimpcits[,2] )
names(CIT) <- c("Dipartimento", "N.pubblicazioni", "Citazioni", "Impatto citazionale", "Coll Internazionale", "Impatto cit Norm")

CIT %>% 
  knitr::kable(digits = 2, caption = "Coefficienti di regressione") %>%
  kable_styling() %>%
  save_kable(file = "table_5.png", zoom = 1.5)



CIT <- cbind(CIT, scale(CIT[, 2:6]))

CIT %>%  select(-2,-3,-4, -5, -6) %>% 
  knitr::kable(digits = 2, caption = "Coefficienti di regressione") %>%
  kable_styling() %>%
  save_kable(file = "table_6.png", zoom = 1.5)
  
  
  
saveRDS(., file=here("programmazione" , "piramideR", "CIT.rds"))


#######tabella complessiva con i tre indicatori e i valori di z-score#####
PRJ <- readRDS( here("programmazione", "piramideR", "PRJ.rds"))
nPRJ <- readRDS( here("programmazione", "piramideR", "nPRJ.rds"))
CIT <- readRDS( here("programmazione", "piramideR", "CIT.rds"))


Dati <- cbind(PRJ, nPRJ[,-1], CIT[, -1])

names(Dati) <- c("Dip", "Nprj in corso", "Bdg in corso", "MdBdg in corso", "NuoviPrj", "BdgNuoviprj", 
                  "MdBdgNuoviprj", "Pubs", "Cits", "ImpCits", "CollInt", "ImpCitsNorm")

Dati %>%  
  knitr::kable(digits = 2, caption = "Z-score") %>%
  kable_styling() %>%
  save_kable(file = "table_7.png", zoom = 1.5)

Dati %>%
  select(-9, -10) %>% 
mutate(score = rowSums(select(., -1)), 
      # tscore = 50+10*score, 
       #   pscore = tscore/sum(tscore), 
       #   Npiram = 30*pscore
      d = max(score)-rev(score), 
      peso = d/sum(d)
       ) %>% 
  arrange(desc(score)) %>% 
  knitr::kable(digits = 2, caption = "Z-score") %>%
  kable_styling() %>%
  save_kable(file = "table_8.png", zoom = 1.5)
  



###grafico differenze### 


line <- tibble("score" = x$score, y = rep (0, 5))

ggplot(line, aes(x= score, y= y))+
  geom_point()

###distribuzione piramidati####
z <- max(x$score)-(x$score)
y <- 30*(z/sum(z))

z




  ###database coefreg progetti e citazioni###

x











##### da Biblioteca ####
pubblicazioni <- read_excel(here("programmazione", "piramideR", "pub2000-2020.xlsx"))
pubblicazioni$Cognome <- str_to_lower(pubblicazioni$AU)
pubblicazioni$Cognome <- gsub(",.*$", "", pubblicazioni$Cognome)
  
anag <- readRDS(here("programmazione", "data", "processed", "ANAGRAFE.rds"))
anag <- anag %>% 
  select(-REPARTO, -CENTRO_DI_COSTO) 


ricercatori <- pubblicazioni %>% 
  mutate(Cognome = toupper(Cognome)) %>% 
  left_join(
    (anag %>% 
       select(Dipartimento, Matricola, Cognome, Dirigente) %>% 
       filter(Dirigente == "TRUE") %>% 
       na.omit()
    ), 
    by= "Cognome") %>% View()





  

##### da WOS ####
  library(bibliometrix)
  izsler<- readFiles("wos1izsler.bib","wos2izsler.bib","wos3izsler.bib")
  
  
  
  
  
  
  
  
  
  
  


  
  
  # summarise(Pubblication = sum(`Web of Science Documents`), 
  #           TotCit = sum(`Times Cited`), 
  #           IntColl = sum(`International Collaborations` ), 
  #           NImpCit = median(`Category Normalized Citation Impact`), 
  #           ImpCit = median(`Citation Impact`)) %>% 
  # mutate(CitMYear = (TotCit/Pubblication)/(2020-Anno)) %>% 
  # mutate(label = abbreviate(Dipartimento)) %>% 
  # ggplot(aes(x = Anno, y = ImpCit, color = label))+
  # geom_line(alpha = 0.3)+
  # geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points"), cex= 0.6))+
  # theme(legend.position = "none")+
  # geom_smooth(se = FALSE, method = "lm")



# ###PUBBLICAZIONI
# pubblicazioni <- read_excel(here("programmazione", "data", "raw", "pubblicazioni2019.xlsx"))
# pubblicazioni$autore <- str_to_lower(pubblicazioni$autore)
# pubblicazioni$autore <- gsub(",.*$", "", pubblicazioni$autore)
# 
# 
# matricole <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))
# matricole <- matricole %>% 
#   filter(DECOMP != "COMPARTO SSN") %>% 
#   select(matricola = "CDMATR", cognome = COGNOME, nome = NOME, reparto) %>% 
#   mutate(cognome = str_to_lower(cognome), 
#          nome = str_to_lower(nome))
# 
# matricole$autore <- str_c(matricole$cognome, matricole$nome, sep=", ")
# matricole$autore <- gsub(",.*$", "", matricole$autore)
# 
# repMat <- readRDS( here("programmazione", "data", "processed", "matrperpubb.rds")) # carico i dati delle matricole per dip/rep/lab vedi preparazione dati.R in script
# 
# 
# pubblicazioni %>% 
#   right_join(matricole, by = "autore") %>%  
#   filter(!is.na(nr)) %>% 
#   select(nr, reparto, autore, tipologia, matricola, autori, titinglese, datibiblio,`TITOLO RIVISTA`, convegno, titoriginale, impf ) %>% 
#   right_join(repMat, by = "matricola") %>% 
#   filter(!is.na(nr)) %>% 
#   saveRDS(here("programmazione", "shinyapp", "ricerca.rds"))
# 
# ricerca <- readRDS(here("programmazione", "shinyapp-in-produzione",  "ricerca.rds"))
# ricerca <- ricerca %>% 
#   mutate(IF = ifelse(tipologia == "IF ; Int" | tipologia == "IF",  "IF", NA), 
#          INT = ifelse(tipologia == "IF ; Int" | tipologia == "Int",  "Int", NA ), 
#          NAZ = ifelse(tipologia == "Naz", "Naz", NA), 
#          Oth = ifelse(tipologia == "Others" , "Others", NA))
# 
#  
# 
# ricerca %>%
#   filter(IF == IF) %>%
#   filter (! duplicated(nr)) %>% 
#   group_by(Dipartimento, Reparto) %>% 
#   summarise(IF= mean(impf, na.rm = TRUE),
#             minIF = min(impf, na.rm = TRUE), 
#             maxIF = max(impf, na.rm = TRUE), 
#             MIF = median(impf, na.rm = TRUE),
#             sumIF = sum(impf, na.rm = TRUE), 
#             N.pub = n()) %>% View()
#  
#   #arrange(desc(MIF)) %>% 
#   ggplot(aes(x=Reparto, y=MIF))+
#   geom_bar(stat = "identity")+
#   coord_flip()
# 
# 
# x = fct_infreq(Position)
# 
#        
#        
#        


###codice per anagrafe#####
anag <- read_excel(here("programmazione", "piramideR", "anagrafe.xlsx"))
# #
# #

dip_rep <- anag %>%
  mutate(REPARTO = recode(dbo_AD_Anagrafe_GRU.REPARTO,
                          "SEDE TERRITORIALE BERGAMO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "SEDE TERRITORIALE DI BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "SEDE TERRITORIALE SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "SEDE TERRITORIALE DI CREMONA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "SEDE TERRITORIALE DI MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "SEDE TERRITORIALE DI LODI" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "SEDE TERRITORIALE DI MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "LAB. DI ISTOLOGIA (MI)" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "SEDE TERRITORIALE DI BOLOGNA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "SEDE TERRITORIALE DI FORLI'" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "SEDE TERRITORIALE DI RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "SEDE TERRITORIALE DI PIACENZA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "SEDE TERRITORIALE DI PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "LAB. CHIM. APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "LAB. BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "REP.TECNOLOGIE BIOLOGICHE APPLICATE" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "LAB. DIAGNOSTICA MOLECOLARE E OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "LAB. CONTAMINANTI AMBIENTALI (BRESCIA)" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "LAB. PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "SERVIZIO PREPARAZIONE TERRENI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "REP. PRODUZIONE E CONTR. MAT. BIOLOGICO" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "LABORATORIO BENESSERE ANIMALE, BIOCHIMICA CLINICA, IMMUNOLOGIA VETERINARIA E STABULARI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "REP. CHIMICA DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
                          "LAB. CONTAMINANTI AMBIENTALI (BO)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                          "REP. CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                          "REP.CONTROLLO DEGLI ALIMENTI" = "REPARTO CONTROLLO ALIMENTI",
                          "LAB. VIROLOGIA SIEROLOGIA SPEC. E MICROS. ELETT." = "REPARTO VIROLOGIA",
                          "REP. PROD. VACCINI E REAGENTI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "LAB. PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA",
                          "LABORATORIO COLTURE CELLULARI, BIOBANCA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "REP. SUBSTRATI CELLULARI E IMMUNOL.CELL." = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "REPARTO SUBSTRATI CELLULARI" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "FORMAZIONE" = "FORMAZIONE BIBLIOTECA COMUNICAZIONE",
                          "U.O. GESTIONE ECONOMICO FINANZIARIA" = "U.O. GESTIONE SERVIZI CONTABILI",
                          "U.O. ECONOMICO FINANZIARIA" = "U.O. GESTIONE SERVIZI CONTABILI",
                          "U.O. SERVIZI GENERALI" = "U.O. TECNICO PATRIMONIALE",
                          "U.O. GESTIONE DEL PERSONALE" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE",
                          "DIREZIONE GENERALE SANITARIA AMMIN.VA" = "Direzione Generale",
                          "CONTROLLO DI GESTIONE" = "Ufficio Controllo di Gestione e Performance",
                          "U.O. PROGETTI DI RICERCA" = "U.O. AFFARI GENERALI E LEGALI",
                          "SERVIZIO ASSICURAZIONE QUALITA' (2)" = "SERVIZIO ASSICURAZIONE QUALITA'",
                          "FORM.SIS.DOC.C.R.N.FORM.SAN.PUBB.VET." = "FORMAZIONE BIBLIOTECA COMUNICAZIONE"
  ),
  Dipartimento = recode (REPARTO, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale",
                         "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare",
                         "REPARTO CONTROLLO ALIMENTI" = "Dipartimento Sicurezza Alimentare",
                         "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare",
                         "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare",
                         "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia",
                         "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna",
                         "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna",
                         "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna",
                         "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna",
                         "ANALISI DEL RISCHIO E EPIDEMILOGIA GENOMICA" = "Direzione Sanitaria",
                         "Ufficio Controllo di Gestione e Performance" = "Direzione Generale",
                         "DIREZIONE AMMINISTRATIVA" = "Direzione Ammninistrativa",
                         "DIREZIONE GENERALE" = "Direzione Generale",
                         "DIREZIONE SANITARIA" = "Direzione Sanitaria",
                         "FORMAZIONE BIBLIOTECA COMUNICAZIONE" = "Direzione Sanitaria",
                         "GARE CONTRATTI PER ACQUISTO DI BENI E SERVIZI, MAGAZZINO E VENDITE, UFFICIO SERVIZI" = "Direzione Ammninistrativa",
                         "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA" = "Direzione Sanitaria",
                         "PROGETTAZIONE E DIREZIONE LAVORI MANUTENZIONI" = "Direzione Ammninistrativa",
                         "PROGETTI DI RICERCA" = "Direzione Generale",
                         "SERVIZIO ASSICURAZIONE QUALITA'" = "Direzione Generale",
                         "SISTEMI INFORMATIVI" = "Direzione Generale",
                         "SORVEGLIANZA EPIDEMIOLOGICA" = "Direzione Sanitaria",
                         "U.O. AFFARI GENERALI E LEGALI" = "Dipartimento Amministrativo",
                         "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Dipartimento Amministrativo",
                         "U.O. GESTIONE SERVIZI CONTABILI" = "Dipartimento Amministrativo",
                         "U.O. TECNICO PATRIMONIALE" =  "Dipartimento Amministrativo",
                         "U.O. PROVVEDITORATO ECONOMATO E VENDITE" = "Dipartimento Amministrativo",

  )

  )%>%
  mutate(Dipartimento = toupper(Dipartimento) ) %>% 
  select(Dipartimento, REPARTO, CENTRO_DI_COSTO, Dirigente,
         Matricola, Nome, Cognome, InizioRapporto, FineRapporto) %>% 
  filter(!is.na(Dipartimento)) %>% View()

#%>%
# saveRDS(here("programmazione", "data", "processed", "ANAGRAFE.rds"))
#

# "Sezione di Bergamo" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
# "Sezione di Binago" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
# "Sezione di Cremona" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
# "Sezione di Mantova" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
# "Sezione di Lodi" = "SEDE TERRITORIALE DI LODI - MILANO",
# "Sezione di Milano" = "SEDE TERRITORIALE DI LODI - MILANO",
# "Sezione di Bologna" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
# "Sezione di Modena" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
# "Sezione di Ferrara" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
# "Sezione di Forlì" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
# "Sezione di Ravenna" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
# "Sezione di Piacenza" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
# "Sezione di Parma" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
# "Sezione di Brescia" = "SEDE TERRITORIALE DI BRESCIA",
# "Sezione di Reggio Emilia" = "SEDE TERRITORIALE DI REGGIO EMILIA",
# "Sezione di Pavia" =  "SEDE TERRITORIALE DI REGGIO EMILIA",
# "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#"LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#"Direttore Sanitario" = "Direzione Sanitaria",



# 
# prj %>%
#   filter(DataInizio >= as.Date("2010-01-01") & DataInizio <=as.Date("2010-12-31")) %>% 
#   group_by(Dipartimento) %>% View()


  ###grafici prove di codice#####
  
  ggplot(progetti, aes(x=anno, y=N.Progetti, color = label))+
    geom_line(alpha = 0.4 )+ geom_point(alpha = 0.4)+
    # geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points"), cex= 0.6))+
    labs(y = "N.progetti")+
    geom_line(stat="smooth", se = FALSE, size=1.2, method = "lm")+
    scale_colour_manual(values = c("black", "blue", "brown", "green", "red"))
  
  
  progetti %>% 
    group_by(anno) %>% 
    summarise(N.proj = sum(N.Progetti)) %>% 
    ggplot(aes(x=anno, y=N.proj))+
    geom_line(alpha = 0.4 )+ geom_point(alpha = 0.4)+
    # geom_dl(aes(label = label), method = list(dl.combine("first.points", "last.points"), cex= 0.6))+
    labs(y = "N.prpgetti")+
    geom_line(stat="smooth", se = FALSE, size=1.2, method = "lm")+
    scale_colour_manual(values = c("black", "blue", "brown", "green", "red"))
  
  coef(lm(N.Progetti~anno, data = progetti)
  )      
  

