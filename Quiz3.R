question1 <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
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
    
    # >10 acres && $10,000+ ag products sold
    # ACR == 3 && AGS == 6
    agricultureLogical <- df[which(df$ACR == 3 & df$AGS == 6),]
    
    head(agricultureLogical, n=3)
    
}

question2 <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    library(jpeg)
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/jeff.jpg")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
        download.file(fileURL, destfile="./data/jeff.jpg", method="curl", mode="wb")
    }
    
    jeff <- readJPEG("./data/jeff.jpg", native = TRUE)
    
    quantile(jeff, probs=c(0.3,0.8))
    
}

#as.numeric.factor <- function(x) {seq_along(levels(x))[x]}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

question3 <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    library(plyr)
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
    
    sortedmd <- arrange(mergedData,desc(mergedData$GDPrank))

    sortedmd[13,1:6]
    
}

question4 <- function(){
    ## setwd("/Users/lwelsh/r/Coursera/Coursera-GettingCleaningData")
    library(plyr)
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
    
    mergedData$Income.Group <- as.factor(mergedData$Income.Group)
    
    splitmd <- split(mergedData, mergedData$Income.Group)
    
    firstanswer <- mean(splitmd[["High income: OECD"]]$GDPrank)
    secondanswer <- mean(splitmd[["High income: nonOECD"]]$GDPrank)
    
    print(c(firstanswer,secondanswer))
    
}



question5 <- function(){
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
    
    GDPranking <- cut2(mergedData$GDPrank, g=5)
    
    table(GDPranking, mergedData$Income.Group)
    
}
