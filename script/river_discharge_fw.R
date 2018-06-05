library(waterData)
library(hydroTSM)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(scales)

#station to analyze
station = '02323500'   

#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01") 

dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
#dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#Naming columns, using the Diver sensors, collects date, pressure, temp, conductivity
colnames(dis) <- c("StaID", "Discharge", "oldDate", "QualCode", "Year")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$oldDate)

dis$Month <- month(dis$Date, label=TRUE)

class(dis$Month)
class(dis$Date)



Jan<- dis[ which(dis$Month=='Jan'), ]
Feb<- dis[ which(dis$Month=='Feb'), ]
Marc<- dis[ which(dis$Month=='Mar'), ]
Apr<- dis[ which(dis$Month=='Apr'), ]
Mar<- dis[ which(dis$Month=='May'), ]
Jun<- dis[ which(dis$Month=='Jun'), ]
Jul<- dis[ which(dis$Month=='Jul'), ]
Aug<- dis[ which(dis$Month=='Aug'), ]
Sept<- dis[ which(dis$Month=='Sep'), ]
Oct<- dis[ which(dis$Month=='Oct'), ]
Nov <- dis[ which(dis$Month=='Nov'), ]
Dec <- dis[ which(dis$Month=='Dec'), ]




Jan$Season<- (Jan$Season = "Winter")
Feb$Season<- (Jan$Season = "Winter")
March$Season<- (March$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")
Jan$Season<- (Jan$Season = "Winter")





river<-ggplot(data=dis, aes(x= Month, y=Discharge/1000)) + 
  geom_boxplot(position = 'identity') +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Month", y="River Discharge (1,000 cfs)") +
  facet_wrap(~Year, ncol=8) 
ggsave('river.png', height = 12, width = 15, dpi=300)



#Boxplot of every year per month
ggplot(data=dis, aes(x= Month, y=Discharge/1000)) + 
  geom_boxplot(position = 'identity') +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Month", y="River Discharge (1,000 cfs)") #+
  #facet_wrap(~Year, ncol=8) 

