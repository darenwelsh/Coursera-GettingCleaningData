expensiveproperties <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/IdahoHousing.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        download.file(fileURL, destfile="./data/IdahoHousing.csv", method="curl")
    }
    
    df <- read.csv("./data/IdahoHousing.csv")
    
    df$FES
}

avgcalcspeed <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/GettingCleaning")
    library(data.table)
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/IdahoHousing2.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        download.file(fileURL, destfile="./data/IdahoHousing2.csv", method="curl")
    }
    
    DT <- fread("./data/IdahoHousing2.csv")
    
    race <- 1000
    func1 <- function(DT){
        #tapply(DT$pwgtp15,DT$SEX,mean)
        replicate(race,tapply(DT$pwgtp15,DT$SEX,mean))
    }
    
    func2 <- function(DT){
        mean(DT[DT$SEX==1,]$pwgtp15)
        mean(DT[DT$SEX==2,]$pwgtp15)
    }
    
    func3 <- function(DT){
        replicate(race,sapply(split(DT$pwgtp15,DT$SEX),mean))
    }
    
    func4 <- function(DT){
        mean(DT$pwgtp15,by=DT$SEX)
    }
    
    func5 <- function(DT){
        replicate(race,DT[,mean(pwgtp15),by=SEX])
    }
    
    runrace <- function(func){
        system.time(func)[1]
    }
    
    print("Function 1:")
    print(system.time(func1(DT)))
    
    print(" ")
    print("Function 3:")
    print(system.time(func3(DT)))
    
    print(" ")
    print("Function 5:")
    print(system.time(func5(DT)))
    
}

