
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Carbon Footprint", 
                  titleWidth = 250,
                  disable = FALSE),
  
  dashboardSidebar(),
  
  dashboardBody(
    tabsetPanel(
      id = "tabset",
      tabPanel(title = "Home",
               
      ),
      tabPanel(title = "Dashboard",
               fluidRow(  
                 box(
                   title = "Major causes of CO2 emission"
                   ,width = 12
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE

                 )),
               fluidRow(  
                 box(
                   title = "Global trend of CO2 by year"
                   ,width = 12
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                 )),
      ),
      tabPanel(title = "Forecast",
               
      ),
      
      
      tabPanel(title = "About Us",
       
      )#tabPanel
    )#tabsetPanel
  ),#dashboardBody
)#dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
