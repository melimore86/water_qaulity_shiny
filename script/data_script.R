library("tidyverse")
library("DBI")
library("RMySQL")
library("rsconnect")
library("lubridate")
library("marelac")

# In case if there are too many connections open
lapply(dbListConnections(MySQL()), dbDisconnect)


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

#Changing the format of a new date for ggplot, plotting, Posixct
wq$date<- as.POSIXct(wq$observation_datetime, tz="EST",usetz=TRUE)

#Changing the format for these date for filtere in dplyr
wq$observation_datetime<- as.Date(wq$observation_datetime, tz="EST",usetz=TRUE)

#standard=42.914
#wq$sal <- convert_RtoS(wq$conductivity_mS_cm/standard, 
                       #t= wq$temperature_c, p= 0)

colnames(wq) <- c("ID", "Obs_Date", "In_Service", "Pressure", "Temperature", "Conductivity", "Salinity_raw", "Sound_velo", "Notes", "Site","Sensor_ID", "Salinity", "gmt_time", "Date")


ggplot(data= wq,aes( x=Date, y= Salinity))+
  geom_point()+
  facet_wrap (~Site)

## Removing dates where sensors were malfunctioning in Site 2, oyster growth

wq<- wq %>% 
  filter(!(Site == 2 & Salinity < 0.5))

## Removing dates where sensors were malfunctioning in Site 3

wq<-wq %>% 
  filter(!(Site == 3 & Salinity < 4))


#Writting as a .csv for the Shiny App

write.csv(wq, file = "wq.csv")

###Lakewatch Data, in .csv
lab <- read.csv("wq_app/data/discrete_measurement.csv", header= T)

colnames(lab) <- c("Site", "Date", "Time_org", "Time_UTC", "Sun_code", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Color", "DO", "Temperature","Conductivity", "Salinity", "Depth", "Sensor_Type")

lab$Date <- as.Date(lab$Date)

lab$Secchi<- (lab$Secchi/ 3.28)

write.csv(lab, file = "lab.csv")




