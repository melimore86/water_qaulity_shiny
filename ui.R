

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Oyster Restoration Project Observations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Site", choices=c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
    ,selectInput("site2", "Comparison Site", choices=c("None","1", "2", "3", "4", "5", "6", "7", "8", "9"))
    ,
    dateRangeInput("date",
                   label = 'Date range input: yyyy-mm-dd',
                   start = "2017-08-01" , end = Sys.Date() + 14
    ),
    

    checkboxGroupInput("variable",
                        label = h3("Observation Variable"),
                        choices = list("Salinity (ppt)" = 1,
                                       "Temperature (C)" = 2,
                                       "Conductivity (mS/cm)"= 3,
                                       "River Discharge (cfs)"= 4), plotOutput("scatter")),
    checkboxGroupInput("format",
                       label = h3("Format"),
                       choices = list("Figure" = 1,
                                      " Data Table" = 2))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      (""),
      tableOutput("table")
      
      # mainPanel(
      #   (""),
      #   plotOutput("scatter2")
      
)
    )
  )
)
