library(tidyverse)
library(gridExtra)
library(shinythemes)
library(scales)
library(rsconnect)
library(lubridate)
library(waterData)
library(marelac)



###Sensor Data

con <- dbConnect(MySQL(),
                 user="LCRoysterproject", 
                 password="HLLV6Pske0vTzhIZfSya",
                 dbname="LCRoysterproject", 
                 host="ict-prod-hosting05.mysql.osg.ufl.edu", 
                 port= 3359)

# Listing all of the columns in the database
dbListTables(con)

wq <- dbReadTable(conn = con, name = 'lcroyster_buoyobservation')


wq$date<- as.POSIXct(wq$observation_datetime, tz="EST",usetz=TRUE)

#standard=42.914
#wq$sal <- convert_RtoS(wq$conductivity_mS_cm/standard, 
                       #t= wq$temperature_c, p= 0)

colnames(wq) <- c("ID", "Obs_Date", "In_Service", "Pressure", "Temperature", "Conductivity", "Salinity_raw", "Sound_velo", "Notes", "Site","Sensor_ID", "Salinity", "gmt_time", "Date")


## Removing dates where sensors were malfunctioning

  
wq %>% 
  filter(wq$Site == 3)

wq %>%
  filter(Site == 2, date >= as.POSIXct("2017-10-27") & date <= as.POSIXct("2017-12-01")) 

###Lakewatch Data, in .csv
lab <- read.csv("data/discrete_measurement.csv", header= T)

colnames(lab) <- c("Site", "Date", "Time_org", "Time_UTC", "Sun_code", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Color", "DO", "Temperature","Conductivity", "Salinity", "Depth", "Sensor_Type")

lab$Date <- as.POSIXct(as.Date(lab$Date,origin= "1899-12-30"))

lab$Secchi<- (lab$Secchi/ 3.28)




