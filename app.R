library(shiny)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(shinythemes)
library(ggplot2)
library(scales)

dat <- read_csv("data/full-data.csv") %>% 
  select(Site, Date, Salinity, Temperature, Conductivity) %>%
  separate(Date, c("Date", "Time"), sep = " ") %>% 
  mutate(Date = ymd(Date),
         Site = paste("Site", Site))

ui <- fluidPage(theme = shinytheme("yeti"),
  
  titlePanel("Water Quality Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Site", 
                  choices=unique(dat$Site)),
      selectInput("site2", "Comparison Site", 
                  choices=c("None" = 0,unique(dat$Site))),
      dateRangeInput("date",
                     label = 'Date range input: yyyy-mm-dd',
                     start = "2017-08-01" , end = Sys.Date() + 14),
      checkboxGroupInput("variable",
                         label = h3("Observation Variable"),
                         choices = list("Salinity (ppt)" = "Salinity",
                                        "Conductivity (mS/cm)"= "Conductivity",
                                        "Temperature (C)" = "Temperature"),
                         selected = c("Salinity")),
      downloadButton("downloadPlot", "Download Plot"),
      
      downloadButton("downloadData", "Download CSV")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Figure", plotOutput("plot")),
        tabPanel("Data Table", dataTableOutput("table"))
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  
  datInput <- reactive({
    dat <- dat %>% 
      filter(Site == input$site,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
      select(Site, Date, Time, input$variable)
    dat
  })
  
  plotInput <- reactive({
    dat <- dat %>% 
      filter(Site == input$site | Site == input$site2,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
      select(Site, Date, Time, input$variable) %>% 
      gather("Variable", "Measurement", input$variable) 
    
    ggplot(dat, aes(x = Date, y = Measurement)) +
      geom_point() +
      scale_x_date(
        breaks = date_breaks("month") ,
        labels = date_format("%m/%y")) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~ Variable + Site)
    if (input$site2 == 0) {
            ggplot(dat, aes(x = Date, y = Measurement)) +
               geom_point() +
        scale_x_date(
          breaks = date_breaks("month") ,
          labels = date_format("%m/%y")) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
              facet_wrap(~ Variable + Site, ncol = 1, scales = "free_y") +
              ylab("")
         } else {
            ggplot(dat, aes(x = Date, y = Measurement)) +
                 geom_point() +
             scale_x_date(
               breaks = date_breaks("month") ,
               labels = date_format("%m/%y")) +
             theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
                facet_wrap(~ Variable + Site, ncol = 2, scales = "free_y") +
              ylab("")
          }
  })

  output$table <- renderDataTable({
    datInput()
  })
  
  output$plot <-renderPlot({
    plotInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("oyster-data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(datInput(), file)
    })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("oyster-plot", ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plotInput(), device = "png")
    })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
