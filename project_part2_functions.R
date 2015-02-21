library(plyr)

# data subsetting for one dimensional charts
selectedDataOneDim <- function(data, feature) {
    
    # column selection based on ui input
    d1 <- as.data.frame(data[, feature])
    
    # removal on NA values
    d1 <- d1[!is.na(d1[1]),]
    
    # data summarization using table function
    d1 <- as.data.frame(table(d1))
    
    # standard column name are applied
    names(d1) <- c("data","count")
    
    # sorting on the data
    d1 <- d1[order(-d1$count),]
    
    # number of rows selection based on the slider
    minnum <- min(200, dim(d1)[1])
    
    # subset of the data based on the slider input
    d1 <- d1[c(1:minnum),]

    # return the dataset to be used for the chart
    d1
}


# data subsetting for two dimensional charts
selectedDataTwoDim <- function(data, features) {

    # column selection based on ui input
    d1 <- as.data.frame(data[, c("hyperlinkCount", "correlationCount")])
    
    # removal on NA values
    d1 <- d1[!is.na(d1[1]),]
    d1 <- d1[!is.na(d1[2]),]
    
    # standard column names are applied (required for ddply)
    names(d1) <- c("x","y")
    
    # data summarization using ddply function
    d1 <- ddply(d1, .(x, y), summarize, freq=length(x))
    
    # sorting on the data
    d1 <- d1[order(-d1$freq),]
    
    # subset of the data, only the 30 tuples with the highest frequency are kept
    d1 <- d1[c(1:30),]
    
    # standard column names are applied (required for chart generation)
    d1$id <- paste("x:", d1$x, " y:", d1$y, sep=" ")
    
    # return the dataset to be used for the chart
    d1
}