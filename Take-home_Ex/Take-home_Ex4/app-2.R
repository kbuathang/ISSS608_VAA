##### Set up #####
pacman::p_load(shiny, shinydashboard, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, ggHoriPlot, dtwclust, factoextra)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

weather_data <- weather_data %>% 
  mutate(Date_mine = make_date(2023, month(Date), day(Date)),
         Month_Name = factor(months(Date), levels = month.name)
  )

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

##### Header and sidebar #####
header <- dashboardHeader(title = "Singapore Weather Analytics (2021-2023)")

sidebar <- dashboardSidebar(
  width = 100,
  tags$head(tags$style(HTML("
      .sidebar-menu > li > a {
        white-space: normal; 
        line-height: 1.2;
      }
    "))
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage"),
    menuItem("EDA & CDA", tabName = "EDACDA"),
    menuItem("Univariate Forecasting", tabName = "Univariate"),
    menuItem("Cluster & Group Forecasting", tabName = "Cluster")
    )
  )

##### EDAPLOT ##### 

EDAPlot <- fluidPage(
  # Row 1: Control
  fluidRow(
    box(
      title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("CompareAcross", "Compare across", c("Stations", "Years")),
      uiOutput("dynamicUIForEDA"),
      actionButton("updateEDAPlot", "Update Plot")
    ),
    # Output Plot
    box(
      title = "EDA Plot", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("edaPlot") 
    )
  )
)

##### CDAPLOT ##### 

CDAPlot <- fluidPage(
  # Row 1: Control
  fluidRow(
    box(
      title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("CompareAcross", "Compare across", c("Stations", "Years")),
      radioButtons("selectedStatApproach", "Statistical Approach",
                   choices = c("parametric", "nonparametric", "robust", "bayes")),
      uiOutput("dynamicUIForCDA"),
      actionButton("updateCDAPlot", "Update Plot")
    ),
    # Output Plot
    box(
      title = "CDA Plot", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("cdaPlot") 
    )
  )
)

# Define sub tabs needed for "EDA&CDA" tabName "EDACDA"
EDACDASubTabs <- tabsetPanel(
  tabPanel("Exploratory Data Analysis",
           EDAPlot),
  tabPanel("Confirmatory data analysis",
           CDAPlot)
)

# Body content
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "LandingPage",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "EDACDA",
            h2("EDACDA content"),
            EDACDASubTabs
            
    ),    
    # Second tab content
    tabItem(tabName = "Univariate",
            h2("Univariate content")
            
    ),    
    # Second tab content
    tabItem(tabName = "Cluster",
            h2("Clustering and Group Forecasting")
    )
  )
)

##### UI #####
ui <- dashboardPage(
  header,
  sidebar,
  body
)

##### server #####
server <- function(input, output, session) {
  
  ##### For EDA #####
  # Dynamic UI
  output$dynamicUIForEDA <- renderUI({
    if (input$CompareAcross == "Years") {
      list(
        checkboxGroupInput("stationSelectionForEDA", "Select Station",
                    choices = unique(weather_data$Station),
                    selected = unique(weather_data$Station)[1])
      )
    } else if (input$CompareAcross == "Stations") {
      list(
        checkboxGroupInput("stationSelectionForEDA", "Select Stations",
                    choices = unique(weather_data$Station),
                    selected = unique(weather_data$Station)[1]),
        selectInput("yearSelectionForEDA", "Select Year",
                    choices = unique(weather_data$Year), 
                    selected = unique(weather_data$Year)[1])
      )
    }
  })
  
  # Reactive update button
  reactiveDataEDA <- eventReactive(input$updateEDAPlot, {
    selected_var <- input$selectedVariable
    
    # Filter data based on user input
    if (input$CompareAcross == "Years") {
      selected_data <- weather_data %>%
        filter(Station %in% input$stationSelectionForEDA)
    } else if (input$CompareAcross == "Stations"){
      selected_data <- weather_data %>%
        filter(Year == input$yearSelectionForEDA,
               Station %in% input$stationSelectionForEDA)
    }
    
    facet_var = ifelse(input$CompareAcross == "Years", "Year", "Station")
    
    title <- ifelse(input$CompareAcross == "Years",
                    paste(selected_var, "for", "station(s)", "across the years 2021 to 2023"),
                    paste(selected_var, "for", input$yearSelectionForEDA, "across station(s)"))
    

    return(list(data = selected_data,var = selected_var, facet = facet_var, title = title))
  })
  
  # Output the plot
  output$edaPlot <- renderPlotly({
    # Get the reactive data
    res <- reactiveDataEDA()
    data_to_plot <- res$data
    selected_var <- res$var
    facet_var <- res$facet
    title <- res$title
    
    # Plot the data using Plotly
    if (input$selectedVariable == "Daily Rainfall Total (mm)") {
      ggplotly(
        ggplot(data_to_plot, aes(x = Station, y = `Daily Rainfall Total (mm)`)) +
          geom_point(aes(color = `Daily Rainfall Total (mm)`,
                         text = paste('Year:', Year, 'Month:', Month_Name, 'Day:', day(Date),
                                      'Rainfall:', `Daily Rainfall Total (mm)`)),
                     size = 2.5) +
          labs(title = title,
               xaxis = list(title = facet_var),
               yaxis = list(title = selected_var)) +
          theme_minimal() +
          scale_color_gradient(low = "lightblue", high = "darkblue") +
        
          if (input$CompareAcross == "Years") {
            facet_wrap(~Year)
          } else {
            NULL
          },
        tooltip = "text"
      ) %>%
        layout(hovermode = 'closest',
               xaxis = list(tickangle = 45,
                            tickfont = list(size = 12)),
               yaxis = list(title = "Daily Rainfall Total (mm)"),
               margin = list(l = 60, r = 60, b = 80, t = 80, pad = 4)) %>%
        config(displayModeBar = FALSE)
    }else{
    plot_ly(data = data_to_plot,
            x = ~get(facet_var),  
            y = ~get(selected_var),
            split = ~Station,
            type = 'violin',
            color = ~Station,
            box = list(visible = T),
            meanline = list(visible = T),
            span = I(1), 
            pointpos = 0) %>%
      layout(title = title, 
             xaxis = list(title = facet_var),
             yaxis = list(title = selected_var),
             violinmode = "group")
      }
  })
  
  
  # Output the CDA plot using the reactive data
  output$cdaPlot <- renderPlot({
    # Use the reactive data for plotting
    data_to_plot <- reactiveDataCDA()  # Correct usage as a reactive variable
    
    # Define the statistical approach type based on user selection
    type <- switch(input$selectedStatApproach,
                   "parametric" = "p",
                   "nonparametric" = "np",
                   "robust" = "robust",
                   "bayes" = "bf")
    
    # Generate the plot with ggbetweenstats
    ggbetweenstats(
      data = data_to_plot,
      x = Station,
      y = as.name(input$selectedVariable), 
      type = type,  # For non-parametric; replace with input logic if needed
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.annotation = "p.value",
      pairwise.display = "none", 
      sig.level = NA,
      p.adjust.method = "fdr",
      messages = FALSE
    )
  })
  
}


# Run App
##########
# Run the application 
shinyApp(ui, server)