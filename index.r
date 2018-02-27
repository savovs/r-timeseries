library(corrplot)
library(dplyr)

# # Correlation between Bitcoin and Ethereum prices
# eth <- data.frame(read.csv("./cryptoData/ethereum_price.csv"))
# btc <- data.frame(read.csv("./cryptoData/bitcoin_price.csv"))
# 
# ethNames <- paste("eth", names(eth), sep = "")
# btcNames <- paste("btc", names(btc), sep = "")
# 
# ethNames[1] <- "Date"
# btcNames[1] <- "Date"
# 
# colnames(eth) <- ethNames
# colnames(btc) <- btcNames
# 
# combined <- merge(eth, btc, by = "Date")
# numerical <- combined[sapply(combined, function(x) is.numeric(x))]
# 
# correlationMatrix <- cor(numerical)
# corrplot(correlationMatrix, method = "number")


# Correlation between all crypto prices
# fileNames <- list.files(path = "./cryptoData/", pattern = "price")
# filePaths <- paste('./crypdoData/')
# 
# prefixes <- strsplit(fileNames, split = "_")
# prefixes <- sapply(prefixes, function(x) x[1])
# 
# columnsNoDate <- c("Open", "Close", "High", "Low", "Volume",)
# 
# data <-data.frame(read.csv(fileNames))

dataFolder <- file.path(getwd(), "data", "/")
print(dataFolder)

fileList <- list.files(path = dataFolder, pattern="*_price.csv")

renameColumns <- function(df, oldNames, newNames) {
  dfNames <- colnames(df)
  dfNames[which(dfNames %in% oldNames)] <- newNames

  colnames(df) <- dfNames

  return(df)
}

# Load each .csv with unique column names except "Date"
load <- function(item) {
  data <- read.csv(paste(dataFolder, item, sep=''))
  data <- data.frame(data)

  prefix <- unlist(strsplit(item, split = "_price"))[1]

  newColumnNames <- paste(prefix, names(data), sep = "")
  newColumnNames[1] <- "Date"
  
  data = renameColumns(data, names(data), newColumnNames)
  
  return(data)
}
  
dataframes <- lapply(fileList, load)
combined <- Reduce(function(x, y) merge(x, y, by="Date"), dataframes)
onlyNumerical <- combined[sapply(combined, function(x) is.numeric(x))]

correlationMatrix <- cor(onlyNumerical)
corrplot(correlationMatrix, method = "color", title="Correlation of Currencies for last 220 days (roughly)", mar=c(0,0,1,0))

