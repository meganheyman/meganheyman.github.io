###Periodic signal
X <- seq(0, 4*pi, length=2^10)

Y1 <- sin(6*X)
Y2 <- 1/2*cos(2*X)
noise <- rnorm(length(X), sd=0.2)

Y <- Y1 + Y2 + noise 


library(wavethresh)
Ywd <- wd(Y)
levels <- seq(0, 9)
CoefSum <- rep(NA, 10)
for(i in 1:length(levels)){
  CoefSum[i] <- mean(accessD(Ywd, level=levels[i])^2)
}
plot(levels, CoefSum, pch=20, main="Wavelet Filtering", xlab="Level", ylab="Energy (coef. mean square)",
     type="b")



par(mfrow=c(4, 1))
plot(X, Y1, type="l", main="Signal Component 1:  sin(6x)", ylim=c(-2,2), ylab="Observation", xlab="time")
plot(X, Y2, type="l", main="Signal Component 2:  0.5cos(2x)", ylim=c(-2,2), ylab="Observation", xlab="time")
plot(X, noise, type="l", main="Noise in signal", ylim=c(-2,2), ylab="Observation", xlab="time")
plot(X, Y, type="l", main="Observed Signal", ylim=c(-2,2), ylab="Observation", xlab="time")



X <- seq(0, 3.5*pi, by=0.01)
Y4 <- sin(X) + 1/3*cos(X/2) + 1/6*cos(3*X) + 1/5*cos(3*X+0.5)
noise <- rnorm(length(X), sd=0.2)
Y <- Y4 + noise
par(mfrow=c(3,1))
plot(X, Y4, type="l", main="Non-Periodic Signal", ylim=c(-2,2), ylab="Observation", xlab="time")
plot(X, noise, type="l", main="Noise in signal", ylim=c(-2,2), ylab="Observation", xlab="time")
plot(X, Y, type="l", main="Observed Signal", ylim=c(-2,2), ylab="Observation", xlab="time")




X <- seq(1, 150, by=0.1)
respSeq3 <- (seq(0.5, 5, length=length(X))*sin(X/(seq(0.5, 5, length=length(X))))
             + seq(0.1, 1, length=length(X))*cos(X/(seq(0.1, 1, length=length(X)))) )
noise <- rnorm(length(X), sd=0.2)


par(mfrow=c(3,1))
plot(X, respSeq3, type="l", main="Signal with Changing Frequency", ylim=c(-6, 3), ylab="Observation", xlab="time")
plot(X, noise, type="l", main="Noise in signal", ylim=c(-6, 3), ylab="Observation", xlab="time")
plot(X, respSeq3+noise, type="l", main="Observed Signal", ylim=c(-6, 3), ylab="Observation", xlab="time")


par(mfrow=c(1,1))
plot(X, respSeq3+noise, type="l", main="Observed Signal", ylim=c(-6, 3), ylab="Observation", xlab="time")



setwd('C:/Users/heyman/Desktop/IRSA talk')
THClimate <- read.csv('THClimate.csv', header=TRUE)
par(mfrow=c(1,1))
plot(seq(1,2^10), THClimate$HOURLYRelativeHumidity[1:2^10], main="Hourly Relative Humidity in Terre Haute, Dec. 2008 - Jan. 2009",
     xlab="Hours after 12:53am on 2008-12-29", ylab="Relative Humidity", type="l")




##AR(1) process
rho <- 0.9
X0 <- 0
X <- c(X0, rep(NA, 2^10-1))
for(i in 2:(2^10) ){
  X[i] <- rho*X[i-1] + rnorm(1, sd=0.7)
}
par(mfrow=c(2,1))
acf(X, main="ACF for AR(1) with correlation 0.9")

Xwd <- wd(X)
acf(Xwd$D, main="ACF for wavelet coefficients of AR(1)")








#Changing amplitude & frequency
X3 <- seq(1, 150, length=2^12)
Y3 <- (seq(0.5, 5, length=length(X3))*sin(X3/(seq(0.5, 5, length=length(X3))))
             + seq(0.1, 1, length=length(X3))*cos(X3/(seq(0.1, 1, length=length(X3)))) 
             + rnorm(length(X3), sd=0.2) )

Y3wd <- wd(Y3)    #DWT for Y3
par(mfrow=c(1,2))
plot(X3, Y3, type="l", main="Observed Signal", ylim=c(-6, 3), ylab="Observation", xlab="time")
plot(seq(1, 2^12-1), Y3wd$D, main="Wavelet Coefficients",
     xlab="Vector index", ylab="Coef. size", pch=20, cex=0.75)



Y3wdsoft <- threshold(Y3wd)                                  #soft threshold
Y3wdhard <- threshold(Y3wd, type="hard", policy="universal") #hard threshold

newY3soft <- wr(Y3wdsoft)   #get the thresholded signal, soft
newY3hard <- wr(Y3wdhard)   #get the thresholded signal, hard


plot.ts(Y3, main="Soft Thresholding vs. True Signal (small error variance)", col="lightgray", ylim=c(-5, 3))
lines(seq(1, 2^12), newY3soft, col="red")
lines(seq(1, 2^12), (seq(0.5, 5, length=length(X3))*sin(X3/(seq(0.5, 5, length=length(X3))))
             + seq(0.1, 1, length=length(X3))*cos(X3/(seq(0.1, 1, length=length(X3))))), col="black")

plot.ts(Y3, main="Hard Thresholding vs. True Signal (small error variance)", col="lightgray")
lines(seq(1, 2^12), newY3hard, col="red")
lines(seq(1, 2^12), (seq(0.5, 5, length=length(X3))*sin(X3/(seq(0.5, 5, length=length(X3))))
             + seq(0.1, 1, length=length(X3))*cos(X3/(seq(0.1, 1, length=length(X3))))), col="black")









############################3
##Relative Humidity Example
##############################
RelHum <- THClimate$HOURLYRelativeHumidity[1:2^10]   #Grab the series we want, to make things easier
plot(seq(1,2^10), THClimate$HOURLYRelativeHumidity[1:2^10], main="Hourly Relative Humidity in Terre Haute, Dec. 2008 - Jan. 2009",
     xlab="Hours after 12:53am on 2008-12-29", ylab="Relative Humidity", type="l")



RelHumwd <- wd(RelHum)                #do the DWT
RelHumwdS <- threshold(RelHumwd)      #soft threshold relative humidity
RelHumwdH <- threshold(RelHumwd, type="hard", policy="universal") #hard threshold relative humidity

newRelHumS <- wr(RelHumwdS)  #get the soft threshold series in time domain
newRelHumH <- wr(RelHumwdH) #get the hard threshold series in the time domain


plot.ts(RelHum, main="Soft Thresholding vs. Observed Relative Humidity", col="lightgray")
lines(seq(1, 2^10), newRelHumS, col="red")


plot.ts(RelHum, main="Hard Thresholding vs. Observed Relative Humidity", col="lightgray")
lines(seq(1, 2^10), newRelHumH, col="red")
