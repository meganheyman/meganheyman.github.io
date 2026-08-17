################################
#### First import the .csv file to your R session ####
################################


################################
#### Load necessary R Packages
################################
library(stringr) # use to create year info in the dataset
library(ggplot2) # use for graphing
library(forcats) # use to reverse order of factor when plotting


################################
#### Filter the data
################################
#rename the dataset object to have a shorter name
allDat <- Hub_Microplastics_Replace_6120810666638033289

#just keep the shorter name version
remove(Hub_Microplastics_Replace_6120810666638033289)

#only keep data rows with the same units of measure
subDat <- allDat[allDat$Unit == "pieces/m3", ]

#ensure that the measurement is not missing
subDat <- subDat[!is.na(subDat$Microplastics.Measurement), ]

#create a date variable with just the day
subDat$Date <- str_remove(subDat$Sample.Date, " .*")
#create a year variable with just the year
subDat$Year <- str_sub(subDat$Date, start=-4)

#keep observations from year 2000 on
subSubDat <- subDat[subDat$Year>1999, ]

#keep observations collected via net methods
MicroPlastics <- subSubDat[subSubDat$Sampling.Method %in% c("Manta net", "Neuston net"), ]

#remove any extra objects from computing session
remove(subDat); remove(allDat); remove(subSubDat);



##################################
#### Time Series Graph (connected scatterplot)
##################################
ggplot(MicroPlastics, aes(x = factor(Year), y = Microplastics.Measurement)) +
  stat_summary(fun = "mean", geom = "line", color="gray", aes(group=1)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(x = "", y = "Mean Microplastics per m^3") +
  scale_x_discrete(breaks=c("2000", "2005", "2010", "2015", "2020", "2024")) +
  theme_classic()


#################################
#### Bar Graph
#################################
# default output from ggplot 
ggplot(MicroPlastics, aes(x = factor(Year), y = Microplastics.Measurement)) +
  stat_summary(fun = "mean", geom = "bar") 

#fix background and axis titles
ggplot(MicroPlastics, aes(x = factor(Year), y = Microplastics.Measurement)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(x = "", y = "Mean Microplastics per m^3") +
  theme_classic()

#flip the x and y axes to fix labeling
ggplot(MicroPlastics, aes(y = fct_rev(factor(Year)), x = Microplastics.Measurement)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(y = "", x = "Mean Microplastics per m^3") +
  theme_classic()

#look at side-by-side boxplots to incorporate variability
ggplot(MicroPlastics, aes(y = fct_rev(factor(Year)), x = Microplastics.Measurement)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", color="red") +
  labs(y = "", x = "Microplastics per m^3") +
  theme_classic()

#transform the count variable
ggplot(MicroPlastics, aes(y = fct_rev(factor(Year)), x = sqrt(Microplastics.Measurement))) +
  geom_boxplot() +
  labs(y = "", x = "Square Root of Microplastics per m^3") +
  theme_classic()
