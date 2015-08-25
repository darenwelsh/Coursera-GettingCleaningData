question1 <- function(){
    setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/IdahoHousing.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        download.file(fileURL, destfile="./data/IdahoHousing.csv", method="curl")
        # Data book:
        # https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
    }
    
    df <- read.csv("./data/IdahoHousing.csv")
    
    names <- names(df)
    splitnames <- strsplit(names, "wgtp")
    print(splitnames[123])
    
}

question2 <- function(){
    library(stringr)
    setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/GDP2.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        download.file(fileURL, destfile="./data/GDP2.csv", method="curl")
    }
    
    GDPcolClasses <- c("factor","integer","NULL","factor","factor",rep("NULL",5))
    GDP <- read.csv("./data/GDP.csv", skip=4, nrows=190, stringsAsFactors=TRUE, header=TRUE, colClasses=GDPcolClasses)
    # GCP[,1] = country shortcodes = GDP$X
    names(GDP) <- c("countrycode", "GDPrank", "country", "GDP")
    
    answer <- as.numeric(str_trim(gsub(",","",GDP$GDP)))
    answer <- mean(answer)
    print(answer)
    
}

question3 <- function(){
    library(stringr)
    setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/GDP2.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        download.file(fileURL, destfile="./data/GDP2.csv", method="curl")
    }
    
    GDPcolClasses <- c("factor","integer","NULL","factor","factor",rep("NULL",5))
    GDP <- read.csv("./data/GDP.csv", skip=4, nrows=190, stringsAsFactors=TRUE, header=TRUE, colClasses=GDPcolClasses)
    # GCP[,1] = country shortcodes = GDP$X
    names(GDP) <- c("countrycode", "GDPrank", "country", "GDP")
    
    countries <- as.character(GDP$country)
    # Remove two entries because they have "\" for some stupid reason
    a <- countries[1:98]
    b <- countries[100:185]
    c <- countries[187:190]
    countries <- c(a, b, c)
    length(grep("^United",countries))
    
}

question4 <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    library(plyr)
    library(Hmisc)
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/GDP.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        download.file(fileURL, destfile="./data/GDP.csv", method="curl")
    }
    if(!file.exists("./data/EDSTATS_Country.csv")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        download.file(fileURL, destfile="./data/EDSTATS_Country.csv", method="curl")
    }
    
    GDPcolClasses <- c("factor","integer","NULL","factor","factor",rep("NULL",5))
    GDP <- read.csv("./data/GDP.csv", skip=4, nrows=190, stringsAsFactors=TRUE, header=TRUE, colClasses=GDPcolClasses)
    # GCP[,1] = country shortcodes = GDP$X
    names(GDP) <- c("countrycode", "GDPrank", "country", "GDP")
    GDP$newGDP <- sub(",", "", GDP$GDP)
    GDP$newGDP <- sub(",", "", GDP$newGDP)
    GDP$newGDP <- as.numeric(GDP$newGDP)
    
    EDSTATS <- read.csv("./data/EDSTATS_Country.csv", na.strings="<NA>", stringsAsFactors=FALSE)
    # EDSTATS[,1] = country shortcodes = EDSTATS$CountryCode
    
    mergedData = merge(GDP,EDSTATS,by.x="countrycode",by.y="CountryCode")
    notes <- mergedData$Special.Notes
    length(grep("Fiscal year end: June", notes))
    
}

question5 <- function(){
    library(quantmod)
    amzn = getSymbols("AMZN",auto.assign=FALSE)
    sampleTimes = index(amzn)
    
    inyear <- grep("^2012-",sampleTimes,value=TRUE)
    dayofweek <- weekdays(as.Date(inyear))
    mondays <- grep("Monday",dayofweek)
    
    print(paste(length(inyear), ", ", length(mondays)))
    
}
