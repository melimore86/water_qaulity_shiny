dat <- read.csv("wq_app/data/wq.csv")
dat1 <- dat %>%
  filter(Site==3)
dat1$Date <- ymd_hms(dat1$Date)
startDate <- min(dat1$Date)
endDate <- max(dat1$Date)

d <- seq(startDate, endDate, by = "hour")
df <- data.frame(Date = d) %>%
  left_join(dat1, by=c("Date" = "Date")) %>%
  select(Date, Salinity)
df$d2 <- date(df$Date)
df1 <- df %>%
  group_by(d2) %>%
  summarise(meanSal=mean(Salinity), minSal=min(Salinity), maxSal=max(Salinity)) %>%
  arrange(d2)
df1

p <- ggplot(df1, aes(x=d2, y=meanSal)) +
  geom_line() +
  geom_ribbon(data=df1, aes(x=d2, ymax=maxSal, ymin=minSal), alpha=0.3)
p
