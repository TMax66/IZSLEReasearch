ui <- navbarPage("Piano Regionale Paratubercolosi Bovina",

##########################INSERIMENTO DATI#########################################              
tabPanel("Inserimento dati",
                          
        fluidPage(
             div(id = "form",
                                
                fluidRow(  
                                  
                     column(3,
                          textInput( "codaz",labelMandatory("Azienda"),value="")
                          ),
                     column(3,
                          dateInput("dt", "Data conferimento", value = Sys.Date(),format = "dd-mm-yyyy")
                          ),
                                  
                     column(3,
                          textInput( "vetlp","Veterinario LP", value = "")
                          ),
                                  
                     column(3,
                          textInput( "vetuff","Veterinario Uff", value = "")
                          )),
                                
               fluidRow(
                                
                                  
                     column(3,
                          textInput( "numconf",labelMandatory("N.conferimento"), value = "")
                          ),
                     column(3,
                          textInput("numcamp", "N.campioni", value = "")),
                     column(3,
                          textInput("numpos", "N.pos", value = "")),
                     column(3,
                          selectInput("pt", "Qualifica", 
                                     c("",  "PT0", "PT1", "PT2", "PT3", "PT4")))
                            ),
                                
               fluidRow(
                       column(9,div(align="center",actionButton("submit", "Salva", class = "btn-primary")
                                                
                       )),
                                  
                       column(3, shinyjs::hidden(
                            div(
                            id = "datainputed_msg",
                            "Dati inseriti",
                            actionLink("submit_another", "Inserisci altri dati")
                            )
                          ) 
                       )),
                                  
                                  
                fluidRow(
                         DT::dataTableOutput("responsesTable")
                        )
                            
                            
                            
                )#chiude la form
             )),

#####################################RISULATATI DEL PIANO#############################
# tabPanel("Risultati del Piano",
#                           fluidPage(
#                             fluidRow(
#                               column(6, div(align='center',
#                                             h3(infoBoxOutput("allad",width = 6))))
#                             ),
#                             hr(),
#                             br(),
#                             br(),
#                             
#                           fluidRow(column(12, div(align='center',
#                                       p("Distribuzione del numero di aziende aderenti al piano per anno e per qualifica" ),
#                                       plotOutput("summaryplot")))),
#                          fluidRow(
#                            column(12, div(align='center',
#                                         h5(textOutput("dataagg"))))
#                          )
#            
#        
#          
#        )),
#####################################RISULTATI PER AZIENDA#######################################?                 
tabPanel("Risultati per Azienda",
                          
                          fluidPage(
                            fluidRow(
                            
                            column(12, div(align="center", 
                                           column(12, div(align="center", 
                                                          selectInput("azienda", "Azienda",
                                                                      c(unique(as.character(loadData()$codaz))))))))),
                                            
                            hr(),
                            br(),
                            
                              
                            fluidRow(
                              column(6,div(align='center', 
                                           DT::dataTableOutput("taball") )),
                              
                             column(6, div(align='center',
                  
                                plotOutput("distPlot")))
                              
                                
                                )
                          )),
###################################VALUTAZIONE DEL RISCHIO###########################
# tabPanel("Valutazione del rischio (Inserimento dati)",
#          div(id = "form2",
#          fluidPage(
#            sidebarPanel(
#              
#              textInput("riskazienda", "Azienda", ""),
#              
#              dateInput("dtrisk","Data compilazione", value = Sys.Date(),format = "dd-mm-yyyy"),
#              
#              selectInput("settore", "Settore", 
#                        c("Sala Parto","Vitelli prima dello svezzamento",
#                           "Vitelle svezzate/manzette","Manze Gravide","Vacche e Tori",
#                          "Animali acquistati o introdotti negli ultimi 12 mesi"),""),
#                         br(),
#                         br(),
#                         hr(),
#              
#              shinyjs::hidden(
#                div(
#                  id = "rdatainputed_msg",
#                  "Dati inseriti",
#                  actionLink("rsubmit_another", "Inserisci un'altra scheda")
#                )),
# 
#              tableOutput("tabrisk")
#          
#              )
#            
#            ,
#          mainPanel(
#            wellPanel(
#            conditionalPanel(
#            condition = "input.settore == 'Sala Parto'",
#              sliderInput("A1", "Utilizzo multiplo",min=0, max=10, step=1,value=0),
#              sliderInput("A2", "Igiene della lettiera",min=0, max=10, step=1,value=0),
#              sliderInput("A3", "Utilizzo come infermeria",min=0, max=10, step=1,value=0),
#              sliderInput("A4", "Presenza di capi infetti",min=0, max=10, step=1,value=0),
#              sliderInput("A5", "Stato igienico delle mammelle al parto",min=0, max=10, step=1,value=0),
#              sliderInput("A6", "Vitelli nati in altre aree",min=0, max=10, step=1,value=0),
#              sliderInput("A7", "Tempo di permanenza con la madre",min=0, max=10, step=1,value=0),
#              sliderInput("A8", "Vacche nutrici",min=0, max=10, step=1,value=0)
#                             ),
#            conditionalPanel(
#            condition = "input.settore == 'Vitelli prima dello svezzamento'",
#              sliderInput("B1", "Somministrazione di pool di colostro",min=0, max=10, step=1,value=0),
#              sliderInput("B2", "Somministrazione di colostro di singole bovine a più vitelli",min=0, max=10, step=1,value=0),
#              sliderInput("B3", "Somministrazione di pool di latte di vacca non pastorizzato",min=0, max=10, step=1,value=0),
#              sliderInput("B4", "Contaminazione fecale di latte o colostro",min=0, max=10, step=1,value=0),
#              sliderInput("B5", "Contaminazione di alimenti ed acqua con feci di animali adulti",min=0, max=10, step=1,value=0),
#              sliderInput("B6", "Contatto diretto o indiretto con animali adulti e/o loro feci ",min=0, max=10, step=1,value=0)
#                              ),
#            conditionalPanel(
#              condition = "input.settore == 'Vitelle svezzate/manzette'",
#              sliderInput("C1", "Contatto con animali adulti o loro feci",min=0, max=7, step=1,value=0),
#              sliderInput("C2", "Contaminazione degli alimenti con feci di animali adulti",min=0, max=7, step=1,value=0),
#              sliderInput("C3", "Contaminazione dell’acqua di bevanda con feci di animali adulti ",min=0, max=7, step=1,value=0),
#              sliderInput("C4", "Pascolo promiscuo con animali adulti",min=0, max=7, step=1,value=0),
#              sliderInput("C5", "Alimentazione con foraggi contaminati da letame e/o liquame ",min=0, max=7, step=1,value=0)
#                             ),
#            conditionalPanel(
#              condition = "input.settore == 'Manze Gravide'",
#              sliderInput("D1", "Contatto con animali adulti o loro feci",min=0, max=5, step=1,value=0),
#              sliderInput("D2", "Contaminazione degli alimenti con feci di animali adulti",min=0, max=5, step=1,value=0),
#              sliderInput("D3", "Contaminazione dell’acqua di bevanda con feci di animali adulti ",min=0, max=5, step=1,value=0),
#              sliderInput("D4", "Pascolo promiscuo con animali adulti",min=0, max=5, step=1,value=0),
#              sliderInput("D5", "Alimentazione con foraggi contaminati da letame e/o liquame",min=0, max=5, step=1,value=0)
#                             ),
#            conditionalPanel(
#              condition = "input.settore == 'Vacche e Tori'",
#              sliderInput("E1", "Contaminazione fecale degli alimenti ",min=0, max=4, step=1,value=0),
#              sliderInput("E2", "Contaminazione fecale dell’acqua di bevanda ",min=0, max=4, step=1,value=0),
#              sliderInput("E3", "Accesso a zone di accumulo/stoccaggio di letame/liquame ",min=0, max=4, step=1,value=0),
#              sliderInput("E4", "Alimentazione con foraggi contaminati da letame e/o liquame",min=0, max=4, step=1,value=0)
#                             ),
#            conditionalPanel(
#              condition = "input.settore == 'Animali acquistati o introdotti negli ultimi 12 mesi'",
#              sliderInput("F1", "Acquisto da allevamenti certificati (livelli 3-5) ",min=0, max=8, step=2,value=0),
#              sliderInput("F2", "Acquisto da allevamenti a basso rischio (livello 1-2)",min=0, max=14, step=1,value=0),
#              sliderInput("F3", "Acquisto da un solo allevamento di stato sanitario sconosciuto",min=0, max=28, step=1,value=0),
#              sliderInput("F4", "Acquisto da più allevamenti di stato sanitario sconosciuto",min=0, max=40, step=1,value=0),
#              hr(),
#              
#              actionButton("rsubmit", "Salva", class = "btn-primary")
#              
#              
#              
#              
#            )
#            
#            
#            )#wellpanel
#            )#mainpanel
#            )#fluidpage
#            )#div form2
#            ),#chiude il tabelPanel
# 
# ##########################VALUTAZIONE DEL RISCHIO-RISULTATI
# tabPanel("Valutazione del Rischio-RISULTATI",
#         
#              div(id="form3",
#          fluidPage(
#            
#            # fluidRow(column(12, div(align="center",
#            #                         actionButton("Aggiorna", "Aggiorna",class = "btn-primary")))),
#            # hr(),
#            
#            fluidRow(
#              
#              column(12, div(align="center", 
#                             column(12, div(align="center", 
#                                            selectInput("razienda", "Azienda",
#                                                        c(unique(as.character(loadRisk()$riskazienda))))))))),
#            
#            hr(),
#            br()
#          
#          ), 
#       
#            
#            
#            fluidPage(
#              
#   
#              
#              fluidRow(
#                column(12, div(align="center",
#                               tableOutput("riS")
#                ))
#              )
#            
#            
#            
#          )
#          
#          )
#          ),

     
shinyjs::useShinyjs()
)


