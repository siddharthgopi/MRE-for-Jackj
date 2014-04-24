library(midasr)
library(zoo)


 yvellaregdata <- read.table("~/Desktop/attempt1/ymonthlyjackson.csv", quote="\"")
 yvellareg <- ts(yvellaregdata, start=c(2008,7), frequency=12)

 xvellareginit <- read.table("~/Desktop/attempt1/xdailyjackson.csv", quote="\"")
 xvellaregzoo <- zoo(xvellareg)
 xvellareg <- as.numeric(xvellaregzoo) #i had to convert to numeric for it to work

#yvellareg is the monthly y variable
#xvellareg is the daily x variable
 betareg <- midas_r(yvellareg ~ mls(yvellareg, 1, 1) + mls(xvellareg, 3:25, 30), start=NULL)
 summary(betareg)


#Defining data for forecasting
 xdailyfulldataread <- read.table("~/Desktop/attempt1/xdailyfulldatajackson.csv", quote="\"")
 xdailyfulldata <- zoo(xdailyfulldataread)

 ymonthlyfulldataread <- read.table("~/Desktop/attempt1/ymonthlyfulldatajackson.csv", quote="\"")
 ymonthlyfulldata <- ts(ymonthlyfulldataread,start=c(2008,7), frequency=12)


fulldata <- list(xx=xdailyfulldata,
                   yy=ymonthlyfulldata)
insample <- 1:length(yvellareg)
outsample <- (1:length(fulldata$yy))[-insample]

#error here
avgf<-average_forecast(list(betareg),
                       data=fulldata,
                       insample=insample,
                       outsample=outsample)
sqrt(avgf$accuracy$individual$MSE.out.of.sample)
