setwd("E:/R/RStudio")
load('killersandmotives(2).Rdata')
createsample(40)
# Box plot to spot outliers 
boxplot(mysample$AgeFirstKill)
boxplot(mysample$AgeLastKill)
# Finding the Na values in the dataframe
colSums(is.na(mysample))
# Cleaning the outliers and Na values from the data
# Mean or Median values can be given but it contradicts with Age Last kill
data<-mysample[mysample$AgeFirstKill != 99999, ]
data<-data[!is.na(data$Sentence), ]
data<-data[!is.na(data$Motive), ]
data<-data[!is.na(data$InsanityPlea), ]
# Age of Killers who are born before 1900 are removed
data<-data[!(data$YearBorn+data$AgeFirstKill)<1900, ]
# Mean of the Age First Kill and Age Last kill
mf<-mean(data$AgeFirstKill)
ml<-mean(data$AgeLastKill)
# A career column was created
data$career<-(data$AgeLastKill-data$AgeFirstKill)
# Mean of the newly created columns career
mc<-mean(data$career)
# Standard deviation for the three columns are created
sf<-sd(data$AgeFirstKill)
sl<-sd(data$AgeLastKill)
sc<-sd(data$career)
# Variance of the three columns
vf<-var(data$AgeFirstKill)
vl<-var(data$AgeLastKill)
vc<-var(data$career)
# Plotting the density normal distribution curve
f <- seq(from = min(data$AgeFirstKill), to = max(data$AgeFirstKill), by = 1)
l<- seq(from = min(data$AgeLastKill), to = max(data$AgeLastKill), by = 1)
c<-seq(from = min(data$career), to = max(data$career), by = 1)
# Plotting the histogram and plotting the density curve on them
hist(data$AgeFirstKill,freq = FALSE)
lines(f, dnorm(f, mean = mf, sd = sf), lwd = 2, col = "blue")
hist(data$AgeLastKill,freq = FALSE)
lines(l, dnorm(l, mean = ml, sd = sl), lwd = 2, col = "blue")
hist(data$career,freq=FALSE)
lines(c, dnorm(c, mean = mc, sd = sc), lwd = 2, col = "blue")
plot(data$AgeFirstKill,dnorm(data$AgeFirstKill,mean = mf,sd = sf))
plot(data$AgeLastKill,dnorm(data$AgeLastKill,mean=ml,sd=sl))
plot(data$career,dnorm(data$career,mean=mc,sd=sc))
#let us assume rate= 1/mean() using method of moments method
dexp(data$AgeFirstKill,rate=(1/mf),log=FALSE)
# Plotting the density exponential curve graph
plot(data$AgeFirstKill,dexp(data$AgeFirstKill,rate=(1/mf),log=FALSE))
plot(data$AgeLastKill,dexp(data$AgeLastKill,rate=(1/ml),log=FALSE))
plot(data$career,dexp(data$career,rate=(1/mc),log=FALSE))
# Cummulative distribution function
ecdf(data$AgeFirstKill)
# Analysing whether the sample is a normal distribution or not using quantile plot
z <- (sort(data$AgeFirstKill) - mf)/sf
n <- length(data$AgeFirstKill)
r <- (1:n)
x<-qnorm(p = r/(n + 1), mean = mf, sd = sf)
plot(x,z)
qqnorm(z)
abline(a = 0, b = 1, col = "red")
zl<-(sort(data$AgeLastKill) - ml)/sl
zc <- (sort(data$career) - mc)/sc
qqnorm(zl)
abline(a = 0, b = 1, col = "red")
qqnorm(zc)
abline(a = 0, b = 1, col = "red")
zc <- (sort(data$career) - mc)/sc
nc <- length(data$AgeFirstKill)
rc <- (1:nc)
xc<-qexp(p=rc/(nc+1), rate = 1, lower.tail = TRUE, log.p = FALSE)
plot(xc,zc)
abline(a = 0, b = 1, col = "red")
summary(data)
xy<-data$YearBorn[data$Sentence=="Death penalty"]
hist(xy,freq = FALSE)
xd <- seq(from = min(xy), to = max(xy), by = 1)
lines(xd, dnorm(xd, mean = mean(xy), sd = sd(xy)), lwd = 2, col = "blue")
sample.quantiles.exp<-data$career
r<-1:1000
p<-ppoints(r)
theoretical.quantiles.exp<-qexp(p)
#QQ-plot comparing EXP to EXP (plot is linear, since sample matches population)
qqplot(theoretical.quantiles.exp,sample.quantiles.exp,main="Exponential Q-Q Plot")
abline(-3.5,8.5, col = "red", lwd = 2)
# Dividing the data based on three different motives
angerdata<-data[data$Motive=="Anger (including mission-oriented killers)",]
conveniencedata<-data[data$Motive=="Convenience (didn't want children/spouse)",]
mentalillnessdata<-data[data$Motive=="Mental illness (including paranoia, visionary or Munchausen's syndrome)",]
#skewness of the three columns
skewness(data$AgeFirstKill)
skewness(data$AgeLastKill)
skewness(data$career)
# Correlation in the anger data
library("corrgram")
corrgram(angerdata)
# small analysis
summary(data[data$AgeFirstKill<18,])
mean(conveniencedata$AgeFirstKill)
summary(data[data$AgeLastKill<18,])
summary(data[data$AgeFirstKill>50,])
