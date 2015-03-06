#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param data This is the dataset used to create the histograms.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Histogram
#' @export

library(plyr)

record.hist<-function(data, first.day, site, year){
  data$date.alt<-strptime(data$date, '%Y-%m-%d')
  data$jday<-data$date.alt$yday
  d1<-subset(data, jday>=first.day)
  no.channels<-length(unique(d1$channel))#number of counter channels in dataset
  h<-10; w<-10
  site.name<-site
  study.year<-year
  
  name.events<-"Events by Channel.pdf"
  fig.name<-paste(getwd(), site.name, study.year, name.events, sep="")
  quartz(h,w, type="pdf", bg="white", file=fig.name, dpi=150)
  par(mfrow=c(no.channels,1), mar=c(4,3,3,1), oma=c(2,2,1,0), las=1, xaxs="i", yaxs="i")
  
  no.events<-ddply(subset(d1, description=="E"), c("channel"), function(xx){
    hist(xx$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", xx$channel[1], sep=""), col="grey60")
    no.events<-length(xx$signal)
    data.frame(no.events)
  })
  
  mtext("Frequency of EVENT signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.events)
  
  name.up<-"Ups by Channel.pdf"
  fig.name<-paste(getwd(), site.name, study.year, name.up, sep="")
  quartz(h,w, type="pdf", bg="white", file=fig.name, dpi=150)
  par(mfrow=c(no.channels,1), mar=c(4,3,3,1), oma=c(2,2,1,0), las=1, xaxs="i", yaxs="i")
  
  no.up<-ddply(subset(d1, description=="U"), c("channel"), function(xx){
    hist(xx$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", xx$channel[1], sep=""), col="grey60")
    no.up<-length(xx$signal)
    data.frame(no.up)
  })
  
  mtext("Frequency of UP signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.up)
  
  name.down<-"Downs by Channel.pdf"
  fig.name<-paste(getwd(), site.name, study.year, name.down, sep="")
  quartz(h,w, type="pdf", bg="white", file=fig.name, dpi=150)
  par(mfrow=c(no.channels,1), mar=c(4,3,3,1), oma=c(2,2,1,0), las=1, xaxs="i", yaxs="i")
  

e<-subset(d1, description=="D" & channel==2)
no.down<-ddply(subset(d1, description=="U"), c("channel"), function(xx){
  hist(xx$signal, breaks=15, xlim=c(0, 130), main="", ylab="", xlab=paste("Channel ", xx$channel[1], sep=""), col="grey60")
  no.down<-length(xx$signal)
  data.frame(no.down)
  })
  
  mtext("Frequency of DOWN signal sizes", side=2, outer=TRUE, las=0)
  dev.off()
  print(no.down)
}