
library(shiny)
library(waterData)
library(hydroTSM)
library(scales)
library(marelac)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ1/CSV")
LC_WQ1 <- read.csv("LC_WQ1_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ2/CSV")
LC_WQ2 <- read.csv("LC_WQ2_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ3/CSV")
LC_WQ3 <- read.csv("LC_WQ3_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ4/CSV")
LC_WQ4 <- read.csv("LC_WQ4_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ5/CSV")
LC_WQ5 <- read.csv("LC_WQ5_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ6/CSV")
LC_WQ6 <- read.csv("LC_WQ6_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ7/CSV")
LC_WQ7 <- read.csv("LC_WQ7_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ8/CSV")
LC_WQ8 <- read.csv("LC_WQ8_All_Days_R.csv", header= T)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ9/CSV")
LC_WQ9 <- read.csv("LC_WQ9_All_Days_R.csv", header= T)

colnames(LC_WQ1) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
colnames(LC_WQ3) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
colnames(LC_WQ2) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ4) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ5) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ6) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ7) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ8) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")
colnames(LC_WQ9) <- c("DateTime_Serial", "Temperature", "Salinity", "Conductivity", "Pressure")

LC_WQ1$Date <- as.POSIXct(as.Date(LC_WQ1$DateTime_Serial,origin= "1899-12-30"))
LC_WQ2$Date <- as.POSIXct(as.Date(LC_WQ2$DateTime_Serial,origin= "1899-12-30"))
LC_WQ3$Date <- as.POSIXct(as.Date(LC_WQ3$DateTime_Serial,origin= "1899-12-30"))
LC_WQ4$Date <- as.POSIXct(as.Date(LC_WQ4$DateTime_Serial,origin= "1899-12-30"))
LC_WQ5$Date <- as.POSIXct(as.Date(LC_WQ5$DateTime_Serial,origin= "1899-12-30"))
LC_WQ6$Date <- as.POSIXct(as.Date(LC_WQ6$DateTime_Serial,origin= "1899-12-30"))
LC_WQ7$Date <- as.POSIXct(as.Date(LC_WQ7$DateTime_Serial,origin= "1899-12-30"))
LC_WQ8$Date <- as.POSIXct(as.Date(LC_WQ8$DateTime_Serial,origin= "1899-12-30"))
LC_WQ9$Date <- as.POSIXct(as.Date(LC_WQ9$DateTime_Serial,origin= "1899-12-30"))


standard= 42.914

LC_WQ1$Salinity <- convert_RtoS(LC_WQ1$Conductivity/standard, 
                                t= LC_WQ1$Temperature, p= 0)
LC_WQ2$Salinity <- convert_RtoS(LC_WQ2$Conductivity/standard, 
                                t= LC_WQ2$Temperature, p=0)
LC_WQ3$Salinity <- convert_RtoS(LC_WQ3$Conductivity/standard, 
                                t= LC_WQ3$Temperature, p= 0)
LC_WQ4$Salinity <- convert_RtoS(LC_WQ4$Conductivity/standard, 
                                t= LC_WQ4$Temperature, p=0)
LC_WQ5$Salinity <- convert_RtoS(LC_WQ5$Conductivity/standard, 
                                t= LC_WQ5$Temperature, p=0)
LC_WQ6$Salinity <- convert_RtoS(LC_WQ6$Conductivity/standard, 
                                t= LC_WQ6$Temperature, p=0)
LC_WQ7$Salinity <- convert_RtoS(LC_WQ7$Conductivity/standard, 
                                t= LC_WQ7$Temperature, p=0)
LC_WQ8$Salinity <- convert_RtoS(LC_WQ8$Conductivity/standard, 
                                t= LC_WQ8$Temperature, p=0)
LC_WQ9$Salinity <- convert_RtoS(LC_WQ9$Conductivity/standard, 
                                t= LC_WQ9$Temperature, p=0)

LC_WQ1$Site<- ("1")
LC_WQ2$Site<- ("2")
LC_WQ3$Site<- ("3")
LC_WQ4$Site<- ("4")
LC_WQ5$Site<- ("5")
LC_WQ6$Site<- ("6")
LC_WQ7$Site<- ("7")
LC_WQ8$Site<- ("8")
LC_WQ9$Site<- ("9")

Data<-rbind (LC_WQ1,LC_WQ2,LC_WQ3,LC_WQ4,LC_WQ5,LC_WQ6,LC_WQ7,LC_WQ8,LC_WQ9)



#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year", "Month")

#Changing the format of the dates to be able to plot against time
dis$Date <- as.POSIXct(as.Date(dis$oldDate,origin= "1899-12-30"))



shinyServer(function(input, output) {

  output$table <- renderDataTable({
    Data
  })
  
  # output$scatter <- renderPlot({ ggplot(data=(input$site), aes(x=(input$
  #                                                       date), y=(input$variable)) + geom_point())
  # })
  # 
  # output$scatter2 <- renderPlot({ ggplot(data=(input$site2), aes(x=(input$
  #                                                              date), y=(input$variable)) + geom_point())
  # })
  # 
  # output$format<-renderDataTable()
})


  
  
    

  

