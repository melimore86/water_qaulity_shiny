library("shiny")
library("tidyverse")
library("shinythemes")
library("ggplot2")
library("scales")
library("gridExtra")

wq <- read.csv("data/wq.csv", header= T)
lab <- read.csv("data/lab.csv", header= T) 

wq$Date<- as.POSIXct(wq$Date, tz="EST",usetz=TRUE)
lab$Date <- as.Date(lab$Date)



#### Front 

ui <- fluidPage(
  
  theme = shinytheme("yeti"),

  sidebarLayout(
    sidebarPanel(
    
      h3("Continuous Data"),
    
      selectInput("site1", label= h4("Site"), 
                  choices=c("None" = 0, unique(wq$Site) %>% sort()), selected = "1"),
      
      selectInput("site2", label=h4("Comparison"), 
                  choices=c("None" = 0, unique(wq$Site) %>% sort()), selected = "6"),
      
      dateRangeInput("date",
                     label =h4('Date range'),
                     start = "2017-01-01" , end = Sys.Date() + 7),

      
      radioButtons("variable",
                         label = h4("Observations"),
                         choices = list("Salinity (ppt)" = "Salinity",
                                        "Conductivity (mS/cm)"= "Conductivity",
                                        "Temperature (C)" = "Temperature"),
                                         selected = "Salinity"),

    h3("Discrete Data"),
    
    
    #selectInput("site3", label= h4("Site"), 
    #            choices=unique(lab$Site)),
    
    #selectInput("site4", label=h4("Comparison"), 
    #            choices=c("None" = 0,unique(lab$Site))),
    
    radioButtons("variable2",
                       label = h4("Observations"),
                       choices = list("Salinity (ppt)" = "Salinity",
                                      "Conductivity (mS/cm)"= "Conductivity",
                                      "Temperature (C)" = "Temperature",
                                      "Phosphorus (ug/L)" = "Phosphorus",
                                      "Nitrogen (ug/L)" = "Nitrogen",
                                      "Color (Pt-Co Units)" = "Color",
                                      "Secchi (m)" = "Secchi"),
                       selected = c("Salinity"))),
    
    
  
    mainPanel(
      plotOutput("sensorplot"),
      plotOutput("labplot"))
  
     )
   )
  

server <- shinyServer(function(input, output) {
  
  #wq <- read.csv("data/wq.csv", header= T)
  #lab <- read.csv("data/lab.csv", header= T) 
  
  #wq$Date<- as.Date(wq$Date, tz="EST",usetz=TRUE)
  #lab$Date<- as.Date(lab$Date, tz="EST",usetz=TRUE)
  
 
  sensorplot <- reactive({

      wq <- wq %>% 
        filter(Site == input$site1 | Site == input$site2,
               Date >= input$date[1] & Date <= input$date[2]) %>% 
               select(Site, Date, input$variable) %>% 
                 gather("Variable", "Measurement", input$variable)
    
    ggplot(wq, aes(x = Date, y = Measurement)) +
      geom_point() +
      scale_x_datetime(
        breaks = date_breaks("month") ,
        labels = date_format("%m/%Y")) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~Site, ncol = 1)
    
    
    if (input$site2 == 0) {
      ggplot(wq, aes(x = Date, y = Measurement)) +
        geom_point() +
        scale_x_datetime(breaks = date_breaks("month") ,
                         labels = date_format("%m/%Y")) +
        theme(
          panel.border = element_rect(
            color = "black",
            size = 0.5,
            fill = NA,
            linetype = "solid"
          ),
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ),
          text = element_text(size = 12)
        ) +
        facet_wrap( ~ Site, ncol = 1) +
        ylab("")
      
    } else {
      ggplot(wq, aes(x = Date, y = Measurement)) +
        geom_point() +
        scale_x_datetime(breaks = date_breaks("month") ,
                         labels = date_format("%m/%Y")) +
        theme(
          panel.border = element_rect(
            color = "black",
            size = 0.5,
            fill = NA,
            linetype = "solid"
          ),
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ),
          text = element_text(size = 12)
        ) +
        facet_wrap( ~ Site, ncol = 1) +
        ylab("")
    }
    
    })
  
labplot <- reactive({
    
    lab <- lab %>% 
      filter(Site == input$site1 | Site == input$site2,
             Date >= input$date[1] & Date <= input$date[2]) %>% 
             select(Site, Date, input$variable2, Sensor_Type) %>% 
               gather("Variable", "Measurement", input$variable2)
  
    ggplot(lab, aes(x = Date, y = Measurement)) +
      geom_point() +
      scale_x_date(
        breaks = date_breaks("month") ,
        labels = date_format("%m/%Y")) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y")
    
    
    if (input$site2 == 0) {
      ggplot(lab, aes(x = Date, y = Measurement)) +
        geom_point() +
        scale_x_date(
          breaks = date_breaks("month") ,
          labels = date_format("%m/%Y")) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
              axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), text = element_text(size=12)) +
        facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y") +
        ylab("")
      
    } else {
      ggplot(lab, aes(x = Date, y = Measurement)) +
        geom_point() +
        scale_x_date(
          breaks = date_breaks("month") ,
          labels = date_format("%m/%Y")) +
        theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, 
                                          linetype="solid"),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), 
              text = element_text(size=12)) +
        facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y") +
        ylab("")
    }
  })
  
  
output$sensorplot<-renderPlot({sensorplot()})
  
  
output$labplot<-renderPlot({labplot()})

})

# Run the application 
shinyApp(ui = ui, server = server)
