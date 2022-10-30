#### --- Library ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggcorrplot)
library(ggplot2)
library(dplyr) 
library(readr)
library(forecast)
library(Hmisc)


##### --- Import data ----
cf_continents = read_csv("C:\\Users\\Lenovo\\Documents\\R\\TechLympics\\continents.csv")
cf_countries = read_csv("C:\\Users\\Lenovo\\Documents\\R\\TechLympics\\countries.csv")
cf_global = read_csv("C:\\Users\\Lenovo\\Documents\\R\\TechLympics\\world.csv")



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

#function for forecast purpose (heatmap)
heatmap_plot = function(db) {
  
  corr_data <- subset(db, select = c("cement_co2", "coal_co2", "flaring_co2", 
                                     "gas_co2", "oil_co2", "other_industry_co2"))
  
  # create a corr matrix and corresponding p-value matrix
  corr_mat <- round(cor(corr_data),2) # round off to 2dp
  p_mat <- cor_pmat(corr_data)
  
  # plotting the interactive corr heatmap
  corr_mat <- ggcorrplot(
    corr_mat, hc.order = TRUE, type = "lower",
    outline.col = "white",
    p.mat = p_mat
  )
  ggplotly(corr_mat)
  
}

#function for forecast purpose (projection)
projection_plot = function(db) {
  
  # read in data
  df_map <- subset(db, select = c("year", "cumulative_co2", "country", "iso_code"))
  #df_map[,2] <- as.numeric(df_map[,2])
  #df_map[,2] <- log(df_map[,2])
  
  # define appearance of the map
  #Set country boundaries as light grey
  l <- list(color = toRGB("#d1d1d1"), width = 0.5)
  #Specify map projection and options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic'),
    resolution = '100',
    showcountries = TRUE,
    countrycolor = '#d1d1d1',
    showocean = TRUE,
    oceancolor = '#c9d2e0',
    showlakes = TRUE,
    lakecolor = '#99c0db',
    showrivers = TRUE,
    rivercolor = '#99c0db')
  
  # plot the map
  p <- plot_geo(df_map) %>%
    add_trace(z = ~cumulative_co2, color = ~cumulative_co2, colors = 'Reds', frame = ~year,
              text = ~country, locations = ~iso_code, marker = list(line = l)) %>%
    colorbar(title = 'Cumulative CO2') %>%
    layout(title = '', geo = g)
  ggplotly(p)
  
}

forecast_plot = function(db) {
  #in ts format
  tsdata <- ts(cf_global$co2, frequency = 1, start = c(1750))
  #plot initial graph
  autoplot(tsdata, xlab = "Year", ylab = "Number of CO2 Emission")
  #Use auto.arima function to get the optimal auto.arima model
  autoarima1 <- auto.arima(tsdata)
  forecast1 <- forecast(autoarima1, h=20) 
  plot(forecast1, xlab = "Year", ylab = "Number of CO2 Emission")
  minor.tick(nx = 5, ny = 3, tick.ratio = 0.5)
  grid(nx = NULL, ny = NULL,
       lty = 2,      # Grid line type
       col = "gray", # Grid line color
       lwd = 2)      # Grid line width
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
      tabPanel(title = "Home",
               
               fluidRow(
                 box(
                   title = "Introduction",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   h3("Carbon footprint is the total quantity of carbon dioxide (CO2) that a person, business, product, or event emits, either directly or indirectly. 
                     As we may already be aware, carbon dioxide is a form of greenhouse gas that has been significantly impacting our world, especially in terms of climate changes. 
                     Despite the fact that there is a growing awareness of carbon emissions, our society has only made modest to minimal efforts. 
                     Therefore, we created this website to highlight and remind people how important this issue is. 
                     It is our aim that this website will provide valuable insight based on our study and raise society's understanding of the need to preserve the environment. "), style="color:#06283D")
                 ),
               
               fluidRow(
                 box(
                   title = "Problem Statement",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   h4("The latest reports of abrupt climate changes around the world have been hot topics for some time. 
                      However, we haven't come across any websites that focus on analyzing and delivering data stories on carbon footprint in order to address this issue."), style="color:#06283D")
                   
                 ),
               
               fluidRow(  
                 box(
                   title = "Objectives",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   span(h4(tags$li("To provide an overall picture of the amount of CO2 emitted over time.")), style="color:#06283D"),
                   span(h4(tags$li("To display which regions emit the most CO2 based on year and source of emission.")), style="color:#06283D"),
                   span(h4(tags$li("To determine which sources contribute the most to CO2 emissions for each region.")), style="color:#06283D"),
                   span(h4(tags$li("To demonstrate how carbon footprint will evolve in the future.")), style="color:#06283D")
                   
                 ),
                 box(
                   title = "Solution proposed",
                   status = "primary",
                   solidHeader = TRUE, 
                   collapsible = TRUE,
                   span((h4("A website with two tabs: ")), style="color:#06283D"),
                   span(h4(tags$li("Plots "), style="color:#06283D")),
                   span(h4(tags$li("Advance "), style="color:#06283D", style = "size:20")),
                   span((h4("to achieve our objectives.")), style="color:#06283D")
                   
                 ))
               
      ),
      tabPanel("Plots",
               fluidRow(
                 box(
                   title = "Global Trend",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   plotlyOutput("global_plot")
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Country with the highest CO2 emissions",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   sidebarLayout(
                     sidebarPanel(
                       span(tags$i(h6("Identify countries which emits CO2 the most based on the chosen year and source of emission.")), style="color:#045a8d"),
                       
                       pickerInput("year_select", "Year:",   
                                   choices = as.numeric(unique(sort(cf_countries$year, decreasing = TRUE) )), 
                                   multiple = FALSE),
                       
                       pickerInput("source_select", "Source of emission:",   
                                   choices = c("Cement","Coal", "Gas", "Oil"), 
                                   multiple = FALSE)
                       
                     ),
                     
                     mainPanel(
                       plotlyOutput("top_plot")
                     )
                   )
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Sources with major contribution to CO2 emissions",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   sidebarLayout(
                     sidebarPanel(
                       span(tags$i(h6("Identify the sources that contribute the most to CO2 emissions based on the chosen location")), style="color:#045a8d"),
                       
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
                 )
               )
      ),
      
      tabPanel(title = "Advanced",
               fluidRow(  
                 box(
                   title = "Heat Map",
                   status = "primary",
                   width = 6,
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   plotlyOutput("heatmap", height = "500px"),
                   h4(
                     "This heatmap potrays the correlation between different types of 
                     sources of CO2 emission. White color indicates least to zero correlation between sources,
                     while red color indicates high correlation between sources."), style="color:#06283D"
                 ),
                 box(
                   title = "World's Projection",
                   status = "primary",
                   width = 6,
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   plotlyOutput("projection", height = "500px"),
                   h4(
                     "User can interact with this projection of CO2 emission around the world from the year 1750 to 2020."
                   ),style="color:#06283D"
                 )
               ),
               fluidRow(
                 box(
                   title = "Forecasting Global CO2 Emission For The Next 20 Years",
                   status = "primary",
                   width = 8,
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   plotOutput("forecast", height = "500px")
                 ),
                 box(
                   title = "Summary of Forecast",
                   status = "primary",
                   width = 4,
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   verbatimTextOutput("summary"),
                   span((h4("The percentage of accuray = 100 - MAPE")), style="color:#06283D")
                 )
               )
      ),
      
      
      tabPanel(title = "About Us",
               fluidRow(
                 box(
                   title = "Acknowledgement",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   h4("First and foremost, we would like to thank the Ministry of Science, Technology, and Innovation for hosting the Malaysia Techlympics. 
                      We are able to explore and learn more about how to use our knowledge to address challenges in the real world.
                      We would also like to convey our sincere gratitude to the lecturers 
                      at the Faculty of Computer Science (FCSIT) at the University of Malaya for guiding us 
                      to where we are now.  With their guidance, we are able to convey our ideas and solutions as effectively as possible. 
                      Last but not least, we would like to express our special thanks to our family and friends for their unwavering support to complete this project. "), style="color:#06283D"
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Meet our team members",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   box(
                     tags$img(src="Ice cream.jpg",height='130',width='100'),
                     span(h4(tags$b("Siti Norhidayah Abdul Bari Arbee")), style="color:#06283D"),
                     span((h5("3rd year Data Science Student | University Malaya")), style="color:#06283D"),
                     span(tags$a(h5("https://www.linkedin.com/in/norhidayah-arbee/")), style="color:#06283D")
                     
                     
                     
                   ),
                   box(
                     tags$img(src="dayah.jpg",height='130',width='100'),
                     span(h4(tags$b("Arina Natasha Houri")), style="color:#06283D"),
                     span((h5("2nd year Data Science Student | University Malaya")), style="color:#06283D"),
                     span(tags$a(h5("www.linkedin.com/in/arina-houri-00a972234")), style="color:#06283D")
                     
                   )
                 )
               )
               
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
  
  #advanced - heatmap
  output$heatmap <- renderPlotly({
    heatmap_plot(cf_global)
  })
  
  #advanced - projection
  output$projection <- renderPlotly({
    projection_plot(cf_countries)
  })
  
  #advanced - forecast
  output$forecast <- renderPlot({
    forecast_plot(cf_global)
  })
  
  #advanced - summary
  output$summary <- renderPrint({
    summary(autoarima1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)