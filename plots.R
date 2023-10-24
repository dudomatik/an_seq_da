
library(fpp2) #yields the data sets a10 and other needed dependencies

#hint: clean your R session deleting any object with the shortcut
#command (or ctrl) + shift + f10
#so that your environment is clean

#generate three fake time series, each of lenght 30
mydata <- cbind(rnorm(30),rnorm(30),rnorm(30))

# Look at the first few lines of mydata
# Apply head() to mydata in the R console to inspect the first few lines of the data.
head(mydata)

#Create a ts object called myts
#ts will automatically add the date  field
#Its agruments are: start: first date, frequency= number of observations per year, i.e.,lenght of the seasonality
myts <- ts(mydata, start = c(1981, 1), frequency = 4)

# To find the number of observations per unit time, use frequency()
frequency(myts) #4

#notice that  myts now contains the dates and the quarter indication
myts

#three different plots, one for each time series
autoplot(myts, facets = TRUE)

#series overlapped on the same plot
autoplot(myts, facets = FALSE)

#plot an individual time series such as gold
autoplot(gold)
frequency(gold) #1: gold has no seasonality

##SEASONAL PLOTS
#the time series a10 is monthly; it covers 17 years.
frequency(a10) # 12 : monthly data
autoplot(a10) #one year after each other
ggseasonplot(a10)  #compares each month across different  years 
ggseasonplot(a10, polar=TRUE)  #compares each month across different  years, polar format 


#consider now the beer data set, which is also monthly
frequency(beer) #12
#for each month, plot the data of different years close to each other. The blu line indicates the mean 
ggsubseriesplot(beer)

#example of the window function
#select throught the window function only the data of the 1992
beer1992 <- window(beer, start = 1992)


##AUTO-CORRELATION ANALYIS
#consider the oil data set, which is a ts object (yearly data)
#plot the ACF graph, with in blue the significance threshold
#autocorrelation is significant in the first four lags
ggAcf(oil)

##AUTOCORELATION FOR A SEASONAL TIME SERIES
#hyndsight: number of visitors to R. Hyndman's blog, https://robjhyndman.com/
#plot the data and try to guess how the auto-correlation will look
autoplot(hyndsight)

#you might consider a subset of data for better visualization
#hyndsight contains daily data for 53 weeks. 
# Time Series:
# Start = c(40, 4) 
# End = c(53, 4) 
#means that the time series starts in the 4-th day of the week of week 40 and 
#end in the 4=-th day of the week of week 53
#Let us consider only  the last four weeks, starting from the 1-st day of the 49-th week
#up to the last day of week 52
hyndsightShort <- window(hyndsight, start = c(49,1), end=c(52,7))
autoplot(hyndsightShort)
#the weekly pattern is now very clear

#the autocorrelation function peaks at lags 7/14/21 etc
#this function both plots and return a struct of results
hyndsightAcf <- ggAcf(hyndsight)
#the actual autocorrelation data are in the Acf object, within the data frame "data"
which.max(hyndsightAcf$data$ACF)

#other examples: you can check that the monthly data of Australian beer sales ("ausbeer") have an ACF peaked in 4/8/12 etc.
frequency(ausbeer)#12
head(beer)#inspect the beginning of the time series
autoplot(beer)
ggAcf(ausbeer)

install.packages("goftest")
library(goftest)
#toolbox
data<-diff(goog)
cvm.test(data,"pnorm", mean=0, sd=sd(data))$p.value

data<-diff(goog)
compare_data = rnorm(n=1000,mean=0,sd=sd(data))
ks.test(data,compare_data)$p.value

beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
meanf(beer2, h=11)(beer2) +
  autolayer(meanf(beer2, h=11), PI=FALSE, series="Mean") +
  autolayer(naive(beer2, h=11), PI=FALSE, series="Naïve") +
  autolayer(snaive(beer2, h=11), PI=FALSE, series="Seasonal naïve") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))
  
autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

fits <- fitted(naive(goog200))
autoplot(goog200, series="Data") +
  autolayer(fits, series="Fitted") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

ggAcf(res) + ggtitle("ACF of residuals")

cvm.test(res[-1], "pnorm", mean = 0, sd=sd(res[-1]))$p.value
checkresiduals(naive(goog200))

# 24.10.

autoplot(gas)
windowTrain = window(gas, start = 1970, end = 1980)
windowTest = window(gas, start = 1980, end = 1990)
#autoplot(windowTrain)

gasFitAdd = hw(windowTrain, seasonal = "additive", h= 120)
gasFitMult = hw(windowTrain, seasonal = "multiplicative", h= 120)

summary(gasFitAdd)
summary(gasFitMult)

accuracy(gasFitAdd, windowTest)
accuracy(gasFitMult, windowTest)

autoplot(window(gas, start = 1980, end = 1990)) + autolayer(gasFitAdd, color="blue", PI=FALSE) + autolayer(gasFitsumMult, color = "red", PI = FALSE)

help(chicken)
autoplot(chicken)

train<- window(chicken, end=1980)
test<- window(chicken, start=1981)
sesFit<- ses(train, h =length(test))
holtFit <- holt (train, h =length(test))

summary(sesFit)
summary(holtFit)

sesFit$model$mse
holtFit$model$mse

sesFit$model$aic
holtFit$model$aic

accuracy(sesFit, test)
accuracy(holtFit, test)

#extra parameters caused overfitting!

autoplot(chicken) + autolayer(sesFit, color="blue")
autoplot(chicken) + autolayer(holtFit, color="blue")

autoplot(usdeaths)
etsFit = ets(usdeaths)
autoplot(etsFit)

fcUSdeaths = forecast(ets(usdeaths), h=10)

autoplot(usdeaths) + autolayer(fcUSdeaths)

autoplot(ausbeer)
etsFitAusBeer = ets(ausbeer)
fcAusBeer = forecast(ets(ausbeer), h = 20)
autoplot(ausbeer) + autolayer(fcAusBeer)
