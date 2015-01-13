#' Counter Clean Up Draft
#' A function to bind and process Logie counter data
#' This function allows you to bind together mulitple counter data files, remove errors and produce a master datafile.
#' @param path.to.folder This is the file path for the folder that contains all data files for processing.
#' @param no.channels This is the number of counter channels that were operated.
#' @param site.name Name of the study river.
#' @param year Year of counter operation.
#' @export

counter.data.cleanup<-function(path.to.folder, no.channels, site.name, year){
	library(plyr)
counter.paths <- dir(path.to.folder, pattern = "\\.L$", full.names = TRUE)
names(counter.paths) <- basename(counter.paths)

counter.data1<-ldply(counter.paths, read.table, header=FALSE, sep="", fill=TRUE, stringsAsFactors=FALSE)[,c(1:7)]#stringsAsFactors=FALSE is important because conversion of numeric factors to numeric is problematic
colnames(counter.data1)<-c("file", "date", "time", "X", "channel", "description", "signal")
counter.data2<-subset(counter.data1, description=="U" | description=="D" | description=="E")#This removes erronious data or unwanted counter status data

counter.data3<-data.frame("file"=counter.data2$file, 
"date"=as.POSIXct(strptime(counter.data2$date, format="%d/%m/%y")),
"time"=as.POSIXct(strptime(paste(counter.data2$date, counter.data2$time, sep="-"), format="%d/%m/%y%-H:%M:%S")),
"X"=as.numeric(counter.data2$X),
"channel"=as.numeric(counter.data2$channel),
"description"=counter.data2$description,
"signal"=as.numeric(counter.data2$signal))

counter.data4<-subset(counter.data3, channel<(no.channels+1))#removes any errors in channel number
counter.data5<-subset(counter.data4, time=unique(counter.data4$time))#removes any duplicate data
counter.data<-droplevels(counter.data5)#gets rid of levels that have been subseted out. 
counter.data<-counter.data[order(counter.data$time),]
#Now write a new text file with only the graphics data. The row names, column names and quotes must be removed.
write.csv(x=counter.data, file=paste(path.to.folder,
                                     site.name, 
                                     year,
                                     ".csv", 
                                     sep=""), row.names=FALSE)
}