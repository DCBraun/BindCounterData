#' A function that plots events per hour of Logie counter data
#'
#' This function plots the number of events per hour channel and day for Logie counter data by channel
#' @param data This is the dataset used to create the plots.
#' @param first.day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Events
#' @export

events.plot<-function(data, first.day, site, year){
  
  data$date.alt<-strptime(data$date, '%Y-%m-%d')
  data$jday<-data$date.alt$yday
  d1<-subset(data, jday>=first.day)
  
  events.hour1<-data.frame(subset(d1, description=="E"), no=1)
  events.hour1$date.time.alt<-as.character(as.POSIXct(strptime(paste(events.hour1$date, events.hour1$time,sep=" "), '%Y-%m-%d %H')))
  events.hour1$date.alt <- NULL
  events.hour.channel<-ddply(events.hour1, c("date.time.alt", "channel"), summarize, no.events=sum(no))
  events.hour.channel$date.time.alt<-as.POSIXct(strptime(events.hour.channel$date.time.alt, format="%Y-%m-%d %H"))
  r<-range(events.hour.channel$date.time.alt)
  
  h<-10
  w<-10
  name<-"Events by Channel and Hour.pdf"
  fig.name<-paste(getwd(), site, year, name, sep="")
  quartz(h,w, type="pdf", bg="white", file=fig.name, dpi=150)
  
  par(mfrow=c(length(unique(events.hour1$channel)),1), mar=c(4,2,1,2), oma=c(4,4,1,2), las=1)
  events.hour.ch<-ddply(events.hour.channel, c("channel"), function(xx){
    
    plot(xx$no.events~as.POSIXct(xx$date.time.alt), main="", ylab="", ylim=c(0, max(events.hour.channel$no.events)*1.05), 		xlim=c(r[1], r[2]), xlab=paste("Channel ", xx$channel[1], sep=" "), type="l", lwd=2, axes=FALSE)
    lines(x=c(r[1],r[2]), y=c(mean(events.hour.channel$no.events), mean(events.hour.channel$no.events)), col="red", lty=2)
    axis(2)
    axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=1)
    box()
    data.frame(no.events=xx$no.events, date.time=xx$date.time.alt)
  })
  mtext("mean events/hour for all channels", side=2, outer=TRUE, col="red", line=-24, at=1, las=1)
  mtext("No. Events/hour", side=2, line=2, outer=TRUE, las=0, cex=1.25)
  print(events.hour.ch)
  dev.off()
}