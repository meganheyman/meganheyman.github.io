################################################
### JISA Data Analysis/Graphics              ###
################################################
setwd('C:/Users/heyman/Desktop/JISA')
subDiv <- read.csv('Sub_Division_IMD_2017.csv', header=TRUE)
library(ggplot2)
library(tidyr)
library(viridis)
library(sf)
library(dplyr)
library(gridExtra)


#Figure 1:  Univariate plot of the June precipitation
png(filename="Figure1_Histogram.png", width=700)
subDiv %>%
  ggplot(aes(x=JJAS) ) +
  geom_histogram(color="black", fill="#238A8DFF") +
  ylab("Frequency") + 
  xlab("Total Precipitation in Monsoon Months (mm)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"))
dev.off()
median(subDiv$JJAS, na.rm=TRUE)
mean(subDiv$JJAS, na.rm=TRUE)
sd(subDiv$JJAS, na.rm=TRUE)




#Figure 2:  Scatterplot of June precipitation and year
png(filename="Figure2_Scatterplot.png", width=1000)
subDiv %>%
  ggplot(aes(x=YEAR, y=JJAS) ) +
  geom_point(color="#238A8DFF") +
  ylab("Total Precipitation in Monsoon Months (mm)") + 
  xlab("Year") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"))
dev.off()


#Figure 3:  Time Series plot of June precipitation and year, by subdivision
png(filename="Figure3_tsplot.png", width=1000)
subDiv %>%
  ggplot(aes(x=YEAR, y=JJAS, color=SUBDIVISION) ) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ylab("Total Precipitation in Monsoon Months (mm)") + 
  xlab("Year") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"),
        legend.position="none")
dev.off()

#Figure 4:  Scatterplot of JJAS vs lag(JJAS) in Arunachal Pradesh
AP <- subDiv[subDiv$SUBDIVISION=="Arunachal Pradesh",]
AP57to17 <- AP[AP$YEAR>=1957,] #only recent complete series
APLagData <- data.frame(PreviousYear=AP57to17$JJAS[-length(AP57to17$JJAS)],
                        CurrentYear=AP57to17$JJAS[-1])

png(filename="Figure4_Scatterplot2.png", width=700)
APLagData %>%
  ggplot(aes(x=PreviousYear, y=CurrentYear) ) +
  geom_point(color="#238A8DFF", size=2) +
  ylab("Precipitation Total During Monsoon Months (mm)") + 
  xlab("Previous Year Precipitation Total During Monsoon Months (mm)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"))
dev.off()


#Figure 5:  maps of India precipitation by subdivision
##ism <- readRDS("gadm36_IND_1_sf.rds") #administrative states

##Shape file source:  https://groups.google.com/g/datameet/c/12L5jtjUKhI
ism<- st_read("shapefiles/indian_met_zones v2.shp")
ShapeFileNameMatch <- read.csv("ShapeFileNameMatch.csv", header=TRUE)
subDiv2 <- merge(subDiv, ShapeFileNameMatch)

subdiv2017 <- subDiv2[subDiv2$YEAR==2017, c(18, 20)]
subdiv2007 <- subDiv2[subDiv2$YEAR==2007, c(18, 20)]


ismMerge2017 <- full_join(ism, subdiv2017)
ismMerge2007 <- full_join(ism, subdiv2007)

p1 <- ismMerge2007 %>%
  ggplot() +
  geom_sf(aes(fill=JJAS), size=0.1) + 
  scale_fill_viridis(limits=c(min(subdiv2017$JJAS, subdiv2007$JJAS),
                              max(subdiv2017$JJAS, subdiv2007$JJAS)))+ 
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        plot.title=element_text(size=14)) +
  labs(title="2007 Monsoon Precipitation")

p2 <- ismMerge2017 %>%
ggplot() +
  geom_sf(aes(fill=JJAS), size=0.1) + 
  scale_fill_viridis(limits=c(min(subdiv2017$JJAS, subdiv2007$JJAS),
                              max(subdiv2017$JJAS, subdiv2007$JJAS)),
                     name="mm")+ 
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.key.width=unit(0.25, "cm"),
        plot.title=element_text(size=14),
        legend.title=element_text(size=10)) +
  labs(title="2017 Monsoon Precipitation")

png(filename="Figure5_Maps.png", width=1000)
grid.arrange(p1,p2, nrow=1, widths=c(0.77,1))
dev.off()



#Figure 6:  Time Series plot of June precipitation and year, by subdivision
png(filename="Figure6_tsplot.png", width=1000)
AP %>%
  ggplot(aes(x=YEAR, y=JJAS) ) +
  geom_line(color="#238A8DFF") +
  ylab("Total Precipitation in Monsoon Months (mm)") + 
  xlab("Year") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"),
        legend.position="none")
dev.off()


###Univariate Inference
library(forecast)
auto.arima(AP57to17$JJAS, d=0)
arima(AP57to17$JJAS, order=c(2,0,0))
summary(lm(AP57to17$JJAS~1))

sd(AP57to17$JJAS)

###Linear Regression Inference
APLagMod <- lm(CurrentYear~PreviousYear, data=APLagData)
summary(APLagMod)
acf(residuals(APLagMod))







#####################################################
## Graphs not in paper currently

#Figure XX:  Barplot of year occurrence
subDivJUN <- subDiv[!is.na(subDiv$JUN), ]

png(filename="FigureXX_Barplot.png", width=1000)
subDivJUN %>%
  ggplot(aes(x=YEAR) ) +
  geom_bar(color="black", fill=rgb(128,0,0, max=255), 
           width=0.4, position = position_dodge(width=0.5)) +
  ylab("Frequency") + 
  xlab("Year") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"))
dev.off()

#Figure 5:  Time series plot of June precipitation vs MAY
png(filename="Figure5_Scatterplot2.png", width=700)
subDiv %>%
  ggplot(aes(x=JUN, y=JUL) ) +
  geom_point(color=rgb(128,0,0, max=255), size=.5) +
  ylab("July Precipitation Totals (millimeters)") + 
  xlab("June Precipitation Totals (millimeters)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"))
dev.off()

#Figure 6:  June precip vs. subdivision
png(filename="Figure6_Boxplot.png", width=500)
subDiv %>%
  ggplot(aes(x=SUBDIVISION, y=JUN) ) +
  geom_boxplot(color=rgb(128,0,0, max=255)) +
  ylab("June Precip. Totals (mm)") + 
  xlab("") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"),
        axis.text.x = element_text(angle = 90, hjust=1, size=5))
dev.off()

#Figure 7:  Time series plot of June precipitation vs JUL
png(filename="Figure7_Scatterplot3.png", width=700)
subDiv %>%
  ggplot(aes(x=JUN, y=JUL, color=SUBDIVISION) ) +
  geom_point(size=.5) +
  scale_color_viridis(discrete = TRUE) +
  ylab("July Precipitation Totals (millimeters)") + 
  xlab("June Precipitation Totals (millimeters)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey20"),
        panel.grid.major = element_line(colour = "grey90"),
        legend.position="none")
dev.off()

