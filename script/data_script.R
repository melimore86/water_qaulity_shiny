library(shiny)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(shinythemes)
library(ggplot2)
library(scales)
library(leaflet)
library(rsconnect)
library(lubridate)
library(dplyr)
library(zoo)
library(waterData)
library(marelac)


####Data
###Sensor Data

LC_WQ1 <- read.csv("data/LC_WQ1_All_Days_R.csv", header= T)
LC_WQ2 <- read.csv("data/LC_WQ2_All_Days_R.csv", header= T)
LC_WQ3 <- read.csv("data/LC_WQ3_All_Days_R.csv", header= T)
LC_WQ4 <- read.csv("data/LC_WQ4_All_Days_R.csv", header= T)
LC_WQ5 <- read.csv("data/LC_WQ5_All_Days_R.csv", header= T)
LC_WQ6 <- read.csv("data/LC_WQ6_All_Days_R.csv", header= T)
LC_WQ7 <- read.csv("data/LC_WQ7_All_Days_R.csv", header= T)
LC_WQ8 <- read.csv("data/LC_WQ8_All_Days_R.csv", header= T)
LC_WQ9 <- read.csv("data/LC_WQ9_All_Days_R.csv", header= T)

colnames(LC_WQ1) <- c("Date", "Sound/Pressure", "Temperature", "Conductivity")
colnames(LC_WQ3) <- c("Date", "Sound/Pressure", "Temperature", "Conductivity")
colnames(LC_WQ2) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ4) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ5) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ6) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ7) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ8) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")
colnames(LC_WQ9) <- c("Date", "Temperature", "Salinity", "Conductivity", "Sound/Pressure")

LC_WQ1$Date <- as.POSIXct(as.Date(LC_WQ1$Date,origin= "1899-12-30"))
LC_WQ2$Date <- as.POSIXct(as.Date(LC_WQ2$Date,origin= "1899-12-30"))
LC_WQ3$Date <- as.POSIXct(as.Date(LC_WQ3$Date,origin= "1899-12-30"))
LC_WQ4$Date <- as.POSIXct(as.Date(LC_WQ4$Date,origin= "1899-12-30"))
LC_WQ5$Date <- as.POSIXct(as.Date(LC_WQ5$Date,origin= "1899-12-30"))
LC_WQ6$Date <- as.POSIXct(as.Date(LC_WQ6$Date,origin= "1899-12-30"))
LC_WQ7$Date <- as.POSIXct(as.Date(LC_WQ7$Date,origin= "1899-12-30"))
LC_WQ8$Date <- as.POSIXct(as.Date(LC_WQ8$Date,origin= "1899-12-30"))
LC_WQ9$Date <- as.POSIXct(as.Date(LC_WQ9$Date,origin= "1899-12-30"))

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

LC_WQ1$Site<-(LC_WQ1$Site="1")
LC_WQ2$Site<-(LC_WQ2$Site="2")
LC_WQ3$Site<-(LC_WQ3$Site="3")
LC_WQ4$Site<-(LC_WQ4$Site="4")
LC_WQ5$Site<-(LC_WQ5$Site="5")
LC_WQ6$Site<-(LC_WQ6$Site="6")
LC_WQ7$Site<-(LC_WQ7$Site="7")
LC_WQ8$Site<-(LC_WQ8$Site="8")
LC_WQ9$Site<-(LC_WQ9$Site="9")

sensor<- rbind (LC_WQ1, LC_WQ2, LC_WQ3, LC_WQ4, LC_WQ5, LC_WQ6, LC_WQ7, LC_WQ8, LC_WQ9)


###Lakewatch Data
lab <- read.csv("data/discrete_measurement.csv", header= T)

colnames(lab) <- c("Site", "Date", "Time", "Sun_code", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Color", "DO", "Temperature","Conductivity", "Salinity", "Depth", "Sensor_Type")

#lab$Date<- paste(lab$oldDate, lab$Time)
#lab$Date <- with(lab, as.POSIXct(paste(lab$oldDate, lab$Time), format="%Y-%m-%d %H:%M"))
#lab$Date <- as.POSIXct(paste(lab$Date, lab$Time), format="%Y-%m-%d %H:%M")

lab$Secchi<- paste(lab$Depth,lab$Secchi)

lab$Secchi<-as.numeric(gsub('NA','',lab$Secchi))

lab$Date <- as.POSIXct(as.Date(lab$Date,origin= "1899-12-30"))

#lab$Date <- as.Date(lab$Date,format="%Y-%m-%d %H:%M")
#lab$Date <- as.Date(lab$Date, "%m/%d/%Y")
#lab$allconduct<-NA
#lab$allconduct<-paste(lab$SpecificConductancemilli,lab$SpecificConductancemicro)
#lab$allconduct<-as.numeric(gsub('NA','',lab$allconduct))

lab$Secchi<- (lab$Secchi/ 3.28)


#### River Discharge Data
#station = '02323500'   

#stinfo  = siteInfo(station)
#dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 
#dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
#dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#colnames(dis) <- c("StaID", "Discharge", "Date", "QualCode", "Year", "Month")

#dis$Date <- as.POSIXct(as.Date(dis$Date))

#dis$Month <- month(dis$Date, label=TRUE)
#dis$Month2 <- month(dis$Date, label=FALSE)


#dis_mean_year<-  aggregate( Discharge ~ Year, dis, mean )
#dis_mean_month<-  aggregate( Discharge ~ Month, dis, mean )


####Joining 

##Need to convert the site into numeric for both data.frames
#sensor$Site<- as.numeric(sensor$Site)
#lab$Site<- as.numeric(lab$Site)

#dat <- full_join(sensor, lab, by = c("Site", "Date","Conductivity", "Salinity", "Temperature"))

#dat$Date <- as.POSIXct(as.Date(dat$Date,origin= "1899-12-30"))

#write.csv(dat, file = "dat.csv")
#write.csv(dis, file= "dis.csv")

#dat2<- full_join(dis, dat, by= ("Date"))

#dat2 <- dat2%>% 
#  select(Site, Date, Salinity, Temperature, Phosphorus, Nitrogen, Color, Secchi, Discharge) %#>%
#  separate(Date, c("Date", "Time"), sep = " ") %>% 
#  mutate(Date = ymd(Date),
         #Site = paste("Site", Site))

#dat2$Date <- as.POSIXct(as.Date(dat2$Date,origin= "1899-12-30"))
