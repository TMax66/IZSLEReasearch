

ui <- dashboardPage(
  dashboardHeader(title = "Dbase Progetti di Ricerca IZSLER", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "risp", icon = icon("dashboard")),
      menuItem("Ricerca Corrente", tabName = "corr", icon = icon("th")),
      menuItem("Consulta database", tabName = "dati", icon = icon("th"))#,
      #menuItem("Text mining Analysis", tabName="tm", icon = icon("edit"))
    )
  ),
  dashboardBody(
    # tags$head(tags$style(HTML('
    #                           .modal-lg {
    #                           width: 88%;
    #                           
    #                           }
    #                           '))),
    tabItems(
  #####primo tab item#####
      tabItem(
        tabName = "risp",
        fluidPage(
          
          
         
          
          
          # box(width=12, solidHeader = TRUE,
            fluidRow(
          
              column(3,
          valueBoxOutput("Quest", width = NULL),
          valueBoxOutput("corr", width=NULL),
          valueBoxOutput("fin", width=NULL),
          valueBoxOutput("eu", width=NULL),
          valueBoxOutput("reg", width=NULL),
          valueBoxOutput("af", width=NULL),
          valueBoxOutput("ccm", width=NULL),
          valueBoxOutput("at", width=NULL)),
          
         
          

          
          column(8,
                 selectInput("tipo", "Seleziona la tipologia di ricerca",
                             c("Tutte",unique(as.character(ds$Tipologia)))),
                 plotOutput("trend", height = 500))
         
          
           

          
        )
        
    
        
        
        #),
       
        )
        )
        ,
#####secondo tab item########
      tabItem(
        tabName = "corr",
        
        fluidPage(    
        tabBox( width = 12,
               
                #####panel ricerca corrente
      tabPanel("Ruolo dell'IZSLER",
        fluidRow(
          column(3,br(),br(),br(),
                 valueBoxOutput("capo", width = NULL),
                 valueBoxOutput("uo", width = NULL),
                 valueBoxOutput("solo", width = NULL)), 
          
          column(9,
          
          box(title="Responsabili scientifici e coinvolgimento dell'IZSLER", width = 9, 
              solidHeader = TRUE, status = "primary", background = "black",
              plotOutput("rs", height=650)
          ))
        )
      ), 
      
           #####panel topic####
      tabPanel("Topic", 
               fluidRow(
               
                 box(title="Termini maggiormente usati nei titoli",
                     sliderInput("nterm","# termini", min=5, max=50,value = 20),
                            status = "primary",solidHeader = TRUE,height=650, background = "black",
                        plotOutput("w")),
                 
                 box(title="Cluster termini", status = "primary",solidHeader = TRUE,height=600, background = "black",
                     sliderInput("sparse","sparse", min=0.8, max=0.99,value = 0.956),
                     plotOutput("clust")),
                 br(), 
                 
                 
                 box( title="Associazione tra termini", status = "primary",solidHeader = TRUE, height=650, width=9,
                       background = "black",
                      textInput("term", "Inserisci una parola chiave", "virus"),
                      sliderInput("ass", "correlazione", min=0.1, max=0.5, value=0.2),
                       plotOutput("asso" ))
                 
                 # 
                 # box(title="Word Cloud", status="danger", solidHeader=TRUE,height=650, 
                 #                sliderInput("size","Cloud Size", min=0.3, max=1,value = 0.4), 
                 #                wordcloud2Output('wordcloud2'))
                   
                   
                   )
                 
                 
                 
               ))), 
      hr(), br()
      
      # actionButton("tabBut", "Word Cloud"),
      # 
      #          
      # bsModal("modalExample", "Word Cloud", "tabBut", size = "large",
      #         box(title="Word Cloud", width = NULL, status="danger", solidHeader=TRUE,
      #                       # sliderInput("size","Cloud Size", min=0.3, max=1,value = 0.4), 
      #                       wordcloud2Output('wordcloud2')))
      
      
      ),
               

      
#####terzo tab item######
      tabItem(
        tabName = "dati",
        
        fluidRow(
        box(width = 12,
            DT::dataTableOutput("db")
            )
                )
            )
      )
  )

)
      
      

 