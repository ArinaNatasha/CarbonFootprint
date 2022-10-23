
#### --- Library ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr) 


#### --- Import data ----
cf_continents = read.csv("Datasets/continents.csv", fileEncoding="UTF-8-BOM")
cf_countries = read.csv("Datasets/countries.csv", fileEncoding="UTF-8-BOM")
cf_global = read.csv("Datasets/world.csv", fileEncoding="UTF-8-BOM")


#### --- Function ----
# function to plot global emission trend by year
global_trend_plot = function(db) {
  g = ggplot(db, aes(x = year, y = co2, group = 1, 
                     text = paste0("Year: ", year, "\n", "Emission: ", co2, " million tonnes" ) ))
  
  g1 = g +
    geom_area(fill="#69b3a2") +
    xlim(c(1750, 2020)) + 
    labs(title="Global emissions of CO2 per year",
          x ="Year", y = "Emission (million tonnes)")+
    theme(plot.title = element_text(size = 12), 
          axis.title = element_text(size = 8), 
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  ggplotly(g1, tooltip = c("text")) 
}

# function to plot top 10 CO2 contributor (per year & chosen source)
top_contributor_plot = function(db) {
  bp = ggplot(db,aes(x = reorder(country, -sources), y = sources, fill = reorder(country, -sources), 
                     text = paste0("Country: ", country, "\n", "Emission: ", sources, " %" ) )) +
    geom_bar(stat="identity")
  
  bp = bp + 
      labs(title="Top 10 CO2 contributor per year (based on chosen source of emission)",
            x ="Country", y = "Emission (percentage of global cumulative production-based emissions)") +
      scale_fill_discrete(name = "Country") +
      theme(plot.title = element_text(size = 12), 
            axis.title = element_text(size = 7),
            legend.title = element_text(size = 8),
            plot.margin = unit(c(1,1,1,1), "cm"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) 
  
  ggplotly(bp, tooltip = c("text")) 
  
}

# function to plot sources by region
sources_region_plot = function(db) {
  
  # cement, coal, gas, oil
  data_ggp <- data.frame(x = db$year,                            # Reshape data frame
                         y = c(db$cement_co2, db$coal_co2, db$gas_co2, db$oil_co2),
                         Sources = c(rep("cement", nrow(db)),
                                   rep("coal", nrow(db)),
                                   rep("gas", nrow(db)),
                                   rep("oil", nrow(db))))
  
  g = ggplot(data_ggp, aes( x, y, col = Sources, group = 1,
                            text = paste0("Year: ", x, "\n", "Emission: ", y, " million tonnes" ))) +     
    geom_line() + 
    ggtitle("Sources of CO2 emissions per year")+
    xlab("Year") + 
    ylab("Million tonnes")+
    theme(plot.title = element_text(size = 12), 
          axis.title = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          plot.margin = unit(c(1,1,1,1), "cm")) 
  
  ggplotly(g, tooltip = c("text"))
  
}

# function to plot cumulative sources by region
sources_region_cumulative_plot = function(db) {
  data_ggp <- data.frame(x = db$year,                           
                         y = c(db$cumulative_cement_co2, db$cumulative_coal_co2, db$cumulative_gas_co2, db$cumulative_oil_co2),
                         Sources = c(rep("cement", nrow(db)),
                                     rep("coal", nrow(db)),
                                     rep("gas", nrow(db)),
                                     rep("oil", nrow(db))))
  
  g = ggplot(data_ggp, aes(x, y, fill=Sources, group = 1,
                           text = paste0("Year: ", x, "\n", "Emission: ", y, " million tonnes" ))) + 
    geom_bar(position="stack", stat="identity") +
    ggtitle("Cumulative sources of CO2 emissions per year")+
    xlab("Year")+
    ylab("Million tonnes") +
    theme(plot.title = element_text(size = 12), 
          axis.title = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          plot.margin = unit(c(1,1,1,1), "cm")) 
  
  ggplotly(g, tooltip = c("text"))
}


#### --- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Carbon Footprint", 
                  titleWidth = 250,
                  disable = FALSE),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tabsetPanel(
      id = "tabset",
      tabPanel(title = "Home"
               
      ),
      tabPanel("Overview plots",
               sidebarLayout(
                 sidebarPanel(
                   span(tags$i(h6("Drop-down feature is only used to find the major CO2 contributor.")), style="color:#045a8d"),
                   span(tags$i(h6("Identify countries which emits CO2 the most based on the chosen year and source of emission.")), style="color:#045a8d"),
                   
                   pickerInput("year_select", "Year:",   
                               choices = as.numeric(unique(sort(cf_countries$year, decreasing = TRUE) )), 
                               multiple = FALSE),
                   
                   pickerInput("source_select", "Source of emission:",   
                               choices = c("Cement","Coal", "Gas", "Oil"), 
                               multiple = FALSE)
                   
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Global trend", plotlyOutput("global_plot")),
                     tabPanel("Major CO2 contributor", plotlyOutput("top_plot"))
                   )
                 )
               )

      ),
      tabPanel("Region plots",
                
                sidebarLayout(
                  sidebarPanel(

                    pickerInput("level_select", "Level:",   
                                choices = c("Country", "Continent"), 
                                selected = c("Country"),
                                multiple = FALSE),
                    
                    pickerInput("region_select", "Country/Region:",   
                                choices = as.character(unique(cf_countries$country)), 
                                multiple = FALSE)
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("CO2 Sources",plotlyOutput("country_plot")),
                      tabPanel("Cumulative CO2 Sources", plotlyOutput("country_plot_cumulative"))
                    )
                  )
                )
      ),
      
      tabPanel(title = "Forecast"
               
      ),
      
      
      tabPanel(title = "About Us"
       
      )#tabPanel
    )#tabsetPanel
  ),#dashboardBody
)#dashboardPage



#### --- SERVER ----
server <- function(input, output, session) {
  
  # update region selections
  observeEvent(input$level_select, {
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(unique(cf_countries$country))
                       )
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(unique(cf_continents$continent))
                        )
    }
    
  }, ignoreInit = TRUE)
  
  
  # create dataframe with selected year and type of emission
  top_reactive_db = reactive({
    
    db = cf_countries

    if(input$source_select == "Cement"){
      db$sources = db$share_global_cement_co2
    }
    if(input$source_select == "Coal"){
      db$sources = db$share_global_coal_co2
    }
    if(input$source_select == "Gas"){
      db$sources = db$share_global_gas_co2
    }
    if(input$source_select == "Oil"){
      db$sources = db$share_global_oil_co2
    }
    
    db %>% filter(year %in% input$year_select)%>% arrange(desc(sources)) %>% slice(1:10)
  })
  
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    
    if (input$level_select=="Country") { 
      db = cf_countries
      db$region = db$country
    }
    
    if (input$level_select=="Continent") { 
      db = cf_continents
      db$region = db$continent
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # world plots
  output$global_plot <- renderPlotly({
    global_trend_plot(cf_global)
  })
  
  # top emission contributor plots
  output$top_plot <- renderPlotly({
    top_contributor_plot(top_reactive_db())
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    sources_region_plot(country_reactive_db())
  })
  
  # cumulative country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    sources_region_cumulative_plot(country_reactive_db())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
