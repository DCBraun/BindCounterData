#' A function that plots events per hour of Logie counter data
#'
#' This function plots the number of events per hour channel and day for Logie counter data by channel
#' @param data This is the dataset used to create the plots.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Events
#' @export


######################################################
library(plyr)

abundance.plot<-function(data, first.day, site, year){
  #setwd("/Users/doug/Desktop/Example Data/Counter/")
  #data <- read.csv("Braun Test 2014.csv", stringsAsFactors=FALSE)
 
  
  data$date.alt<-strptime(data$date, '%Y-%m-%d')
  data$jday<-data$date.alt$yday
  d1<-subset(data, jday>=first.day)
  
  up1<-data.frame(subset(d1, description=="U"), no=1)
  up1$date.alt <- NULL
  up2<-ddply(up1, c("date"), function(x){
                     channel.daily=sum(x$no) 
                     data.frame(channel.daily)
  })
  up2[,"cummulative.count"] <- ave(up2$channel.daily, FUN=cumsum) 
  #up2[,"cummulative.count"] <- ave(up2$channel.daily, by=up2$channel, FUN=cumsum) 
 
  InSeason<-up2[-nrow(up2),]
    
  fig.name<-paste(getwd(), site, year, "UpCounts.pdf", sep="")
  quartz(10,10, type="pdf", bg="white", file=fig.name, dpi=150)
  par(mfrow=c(2,1), mar=c(0,4,0,0), oma=c(5,2,0.5,0.5))
  plot(channel.daily~as.Date(date), data=InSeason, 
       ylim=c(0,max(InSeason$channel.daily)*1.1), 
       typ="b", 
       xlab="", 
       ylab="", 
       axes=FALSE, 
       lwd=2, 
       pch=19)

  axis(2, las=1, col="grey60")
  box(col="grey60")
  mtext("Fish/day", side=2, outer=FALSE, line=3, cex=1.5, las=0)

  plot(cummulative.count~as.POSIXct(date), data=InSeason, ylim=c(0, max(InSeason$cummulative.count)*1.1), typ="b", xlab="", ylab="", axes=FALSE, lwd=2, pch=19)
  r<-as.POSIXct(range(InSeason$date))
  axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=0.85, col="grey60")
  axis(2, las=1, col="grey60")
  box(col="grey60")

  mtext("Total no. of fish", side=2, outer=FALSE, line=4, cex=1.5, las=0)
  mtext("Date", side=1, line=4,cex=1.5)

  dev.off()
  }

data<-data
first.day<-200
site<-"Simpson"
year<-2013

abundance.plot(data, first.day, site, year)