#' Signal Clean Up Draft
#' A function to bind and process Logie signal data
#' This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
#' @param path.to.folder This is the file path for the folder that contains all data files for processing.
#' @param site.name Name of the study river.
#' @param year Year of counter operation.
#' @keywords Logie
#' @export

signal.data.cleanup<-function(path.to.folder, site.name, year){
  library(plyr)
  
  #"\\.txt$" tells r that the files are text files.  
  signal.paths <- dir(path.to.folder, pattern = "\\.txt$", full.names = TRUE)
  names(signal.paths) <- basename(signal.paths)
  s
  signal.data1<-ldply(signal.paths, 
                      read.table, 
                      header=FALSE, 
                      sep="", 
                      fill=TRUE, 
                      stringsAsFactors=FALSE)
  
  signal.data2<-subset(signal.data1[,c(1:8)], V1=="S")[,-2]
  signal.data3<-droplevels(signal.data2)
  colnames(signal.data3)<-c("file", 
                            "date", 
                            "time", 
                            "X", 
                            "channel", 
                            "description", 
                            "signal")
  
  signal.data4<-data.frame("file"=signal.data3$file,
                           "date.time"=as.POSIXlt(strptime(paste(signal.data3$date, signal.data3$time, sep="-"), format='%d/%m/%y-%H:%M:%S')),
                           "date"=as.character(as.POSIXlt(strptime(signal.data3$date, format="%d/%m/%y"))),
                           "time"=as.character(signal.data3$time),
                           "X"=as.numeric(signal.data3$X),
                           "channel"=as.numeric(signal.data3$channel),
                           "description"=signal.data3$description,
                           "signal"=as.numeric(signal.data3$signal))
  signal.data<-signal.data4[!duplicated(signal.data4[,c(2,6)]), ]
  signal.data<-signal.data[order(signal.data$time),]
  #try changing the encoding when exporting. Look at what the encoding is when using a PC. Load the graphics file onto Jan's computer.
  write.csv(x=signal.data[,-2], 
            file=paste(path.to.folder, 
                       site.name, 
                       year,
                       ".csv", 
                       sep=""),
            row.names=FALSE)
}

#path.to.folder <- "/Users/doug/Desktop/Example Data/Graphics/"
#site.name <- "Seton"
#year <-2014
#signal.data.cleanup(path.to.folder, site.name, year)