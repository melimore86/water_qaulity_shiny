library("shiny")
library("tidyverse")
library("shinythemes")
library("ggplot2")
library("scales")
library("gridExtra")

wq <- read.csv("data/wq.csv", header= T) %>%
  filter(Site != 0)
lab <- read.csv("data/lab.csv", header= T) 

wq$Date<- ymd_hms(wq$Date, tz="EST") %>%
  round_date("hour")
# lab$Date <- as.Date(lab$Date)
lab$Date <- paste(lab$Date, "12:00:00") %>%
  ymd_hms(tz="EST")  # Assume data taken from midday

#### Front 

ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  
  sidebarLayout(
    sidebarPanel(
      
      width = 3,
      
      h3("Continuous Data"),
      
      selectInput("site1", label= h4("Site"), 
                  choices=c(unique(wq$Site) %>% sort()), selected = 1),
      
      selectInput("site2", label=h4("Comparison"), 
                  choices=c("None" = 0, unique(wq$Site) %>% sort()), selected = 6),
      
      dateRangeInput("date",
                     label =h4('Date range'),
                     start = "2017-01-01" , end = Sys.Date() + 7),
      
      checkboxInput("overlay", 
                    label = "Overlay point sample data?",
                    value = T),
      
      radioButtons("variable",
                   label = h4("Observations"),
                   choices = list("Salinity (ppt)" = "Salinity",
                                  "Conductivity (mS/cm)"= "Conductivity",
                                  "Temperature (C)" = "Temperature"),
                   selected = "Salinity"),
      
      radioButtons("temp_res",
                   label = "Temporal resolution",
                   choices = list("Hourly" = "Hourly",
                                  "Daily Mean" = "Daily"),
                   selected = "Hourly"),
      
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
      width = 9,
      plotOutput("sensorplot", height = "600px"),
      plotOutput("labplot"))
    
  )
)

#Debug
# input <- list()
# input$date <- c("2017-01-01", "2018-07-01")
# input$variable <- "Salinity"
# input$site1 <- 1
# input$site2 <- 6

server <- shinyServer(function(input, output) {
  
  sensorplot <- reactive({
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="EST")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="EST")
    
    # Filter WQ table
    wq1 <- wq %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      select(Site, Date, Measure = input$variable)
    
    # Build a data table based on input daterange and temporal resolution
    # Note: We're building a table with all possible times first and merge it with
    # WQ table so that NAs and daterange of plot is preserved.
    if (input$temp_res == "Hourly") {
      d <- seq(startDate, endDate, by = "hour")
      df <- data.frame(Site = rep(c(site1, site2), each = length(d)),
                       Date = rep(d, 2)) %>%
        distinct() %>% 
        left_join(wq1)
    } else if (input$temp_res == "Daily") {
      
      # Need to convert WQ from hourly to daily
      wq2 <- wq1 %>%
        mutate(Date1 = date(Date)) %>%
        group_by(Site, Date1) %>%
        summarise(Measure = mean(Measure)) %>%
        select(Site, Date=Date1, Measure)
      
      d <- seq(startDate, endDate, by = "day") %>% date
      df <- data.frame(Site = rep(c(site1, site2), each = length(d)),
                       Date = rep(d, 2)) %>%
        distinct() %>%
        left_join(wq2)
    }
    
    # Remove Site 0 from the df we built
    df <- df %>%
      filter(Site != 0)
    
    # Base version of the plot
    p <- ggplot(df, aes(x = Date, y = Measure)) +
      geom_line() +
      # scale_x_date(
        # breaks = date_breaks("month") ,
        # labels = date_format("%m/%Y")) +
      theme_gray(base_size = 14) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~Site, ncol = 1) +
      ylab(input$variable)
    
    # Add feature if we want to overlay the point sample data
    if (input$overlay) {
      lab1 <- lab %>% 
        filter(Site == site1 | Site == site2,
               Date >= startDate & Date <= endDate) %>% 
        select(Site, Date, Measure = input$variable, Sensor_Type)

      if (input$temp_res == "Hourly") {
        p <- p + 
          geom_point(data = lab1, aes(x = Date, y = Measure, colour = Sensor_Type), shape = 17, size = 3) +
          scale_color_manual(name = "Method", values = c("red", "blue"))
      } else if (input$temp_res == "Daily") {
        p <- p + 
          geom_point(data = lab1, aes(x = date(Date), y = Measure, colour = Sensor_Type), shape = 17, size = 3) +
          scale_color_manual(name = "Method", values = c("red", "blue"))
      }
      
    }
    
    p
  })
  
  # labplot <- reactive({
  #   
  #   lab <- lab %>% 
  #     filter(Site == input$site1 | Site == input$site2,
  #            Date >= input$date[1] & Date <= input$date[2]) %>% 
  #     select(Site, Date, input$variable2, Sensor_Type) %>% 
  #     gather("Variable", "Measurement", input$variable2)
  #   
  #   ggplot(lab, aes(x = Date, y = Measurement)) +
  #     geom_point() +
  #     scale_x_date(
  #       breaks = date_breaks("month") ,
  #       labels = date_format("%m/%Y")) +
  #     theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
  #     facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y")
  #   
  #   
  #   if (input$site2 == 0) {
  #     ggplot(lab, aes(x = Date, y = Measurement)) +
  #       geom_point() +
  #       scale_x_date(
  #         breaks = date_breaks("month") ,
  #         labels = date_format("%m/%Y")) +
  #       theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
  #             axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), text = element_text(size=12)) +
  #       facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y") +
  #       ylab("")
  #     
  #   } else {
  #     ggplot(lab, aes(x = Date, y = Measurement)) +
  #       geom_point() +
  #       scale_x_date(
  #         breaks = date_breaks("month") ,
  #         labels = date_format("%m/%Y")) +
  #       theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, 
  #                                         linetype="solid"),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), 
  #             text = element_text(size=12)) +
  #       facet_wrap(~ Site + Sensor_Type, ncol = 2, scales = "free_y") +
  #       ylab("")
  #   }
  # })
  
  
  output$sensorplot<-renderPlot({sensorplot()})
  
  
  # output$labplot<-renderPlot({labplot()})
  
})

# Run the application 
shinyApp(ui = ui, server = server)
