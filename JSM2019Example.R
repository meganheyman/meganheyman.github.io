myDataFolder <- 'C:/Users/heyman/Desktop/Research/NESS2019/'

cruise <- read.csv(paste(myDataFolder, 'cruise_ship.csv', sep=''), header=TRUE)



library(lmboot)
bootcruiseMod <- wild.boot(cruise$Passengers~cruise$Age.2013+cruise$Crew+
                 cruise$Length+cruise$Tonnage, B=10000)$bootEstParam


cruiseMod <- lm(Passengers~Age.2013+Crew+Length+Tonnage, data=cruise)


hist(bootcruiseMod[,3], main="Sampling Distribution for the Crew Coefficient", xlab="Wild Bootstrap Estimates",
     freq=FALSE, ylim=c(0, 2.5))

mea <- 0.71858
sed <- 0.18984
denst <- rt(500000, df=153)
tY <- mea + denst*sed

lines(density(tY)$x, density(tY)$y, col="blue", lty=2)
legend("topright", c("Reference t-distn"), col=c("blue"), lty=2)

mea + qt(.975, df=153)*sed
mea - qt(.975, df=153)*sed