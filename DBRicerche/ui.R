

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
    tabItems(
      
      tabItem(
        tabName = "risp",
        fluidRow(
          box(width=12, solidHeader = TRUE,
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
        
        )
        ))
        ,
      
      tabItem(
        tabName = "corr",
        tabBox( width = 12,
                tabPanel("Ricerca Corrente",
        fluidRow(
          column(3,br(),br(),br(),
                 valueBoxOutput("capo", width = NULL),
                 valueBoxOutput("uo", width = NULL),
                 valueBoxOutput("solo", width = NULL)), 
          
          column(9,
          
          box(title="Responsabili scientifici", width = 9, 
              solidHeader = TRUE, status = "primary",
              plotOutput("rs", height=650)
          ))
        )
      ), 
      tabPanel("Topic", 
               fluidRow(
                 box(title="Termini maggiormente usati nei titoli (top 20)",
                            status = "primary",solidHeader = TRUE,
                        plotOutput("w",height = 500))
                        ,
                 box(title="Word Cloud", status="danger", solidHeader=TRUE, 
                           sliderInput("size","Cloud Size", min=0.3, max=1,value = 0.4), 
                           wordcloud2Output('wordcloud2'))
               ), 
               
               fluidRow(
                 
                 box(title="Network termini", width=12, status = "primary",solidHeader = TRUE,
                     plotOutput("net", height = 800))
                 
               )
               
               
      )
      # tabPanel("Topics Model",
      #          fluidRow(
      #            
      #            
      #          )
      # 
      # 
      # 
      )),

      
   
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

      
      

 