

ui <- dashboardPage(
  dashboardHeader(title = "Project Ricerca"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Risultati", tabName = "risp", icon = icon("dashboard")),
      menuItem("Dati", tabName = "dati", icon = icon("th"))#,
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
          valueBoxOutput("Quest", width = NULL)),
          column(3,
          valueBoxOutput("age", width = NULL)),
          column(3, 
          valueBoxOutput("serv", width = NULL)),
          column(3, 
          valueBoxOutput("est", width = NULL))
        ))
        ),
        
        fluidRow(
          box(
            title = "Professione", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("prof", height = 250)
          ), 
          
          box(
            title = "Formazione", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("form", height = 250)
          ), 
          
          box(
            title = "Ruolo in IZSLER", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("ruolo", height = 250)
          )
          
        )
        
        ),
   
      tabItem(
        tabName = "dati",
        
        fluidRow(
        box(width = 12,
            DT::dataTableOutput("risp")
            )
                )
            ), 
      
      tabItem(
        tabName = "tm",
        fluidRow()
      )
      
      
      
    )
  )
)