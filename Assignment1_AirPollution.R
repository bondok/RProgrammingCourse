## File         : Assignment1_AirPollution.R
## System       : Assignment 1 - RProgramming Course (Coursera)
## Date         : 05/04/2016
## Author       : Ala Halaseh

##read CSV data from files
readCSVData <- function(directory, id){
  df <- data.frame()
  for (i in id){
    filePath <-paste(directory,"/",sprintf("%03d.csv",i),sep="")
    f <- read.csv(filePath)
    df <- rbind(df,f)
  }
  df
}

pollutantmean <- function(directory, pollutant, id=1:332){
  data <- readCSVData(directory,id)
  mean(data[[pollutant]], na.rm = TRUE)
}


complete <- function(directory, id){
  df <- data.frame()
  for (i in id){
    data <- readCSVData(directory,i)
    rowNoNa <- sum(complete.cases(data))
    df <- rbind(df,c(i,rowNoNa))
  }
  colnames(df) <- c("id", "nobs")
  df
}

corr <- function(directory, threshold = 0){
  files<-list.files(directory)
  ret<-vector()
  for (f in files){
    csvF <- read.csv(paste(directory,"/",f,sep=""))
    csvFile2Complete <-csvF[complete.cases(csvF),]
    if (sum(complete.cases(csvF)) >= threshold){
      x<-cor(csvFile2Complete[["sulfate"]],csvFile2Complete[["nitrate"]])
      ret <- c(ret,x)
    }
  }
  ret
}