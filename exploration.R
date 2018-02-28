library(corrplot)
library(sm)
library(anytime)

dataFolder <- file.path(getwd(), "data", "/")

fileList <- list.files(path = dataFolder, pattern = "*_price.csv")

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
  
  # Format Date
  data$Date <- anydate(data$Date)
  
  prefix <- unlist(strsplit(item, split = "_price"))[1]

  newColumnNames <- paste(prefix, names(data), sep = "")
  newColumnNames[1] <- "Date"

  data = renameColumns(data, names(data), newColumnNames)

  return(data)
}

dataframes <- lapply(fileList, load)
combined <- Reduce(function(x, y) merge(x, y, by = "Date"), dataframes)
combined <- combined[order(combined$Date),]

onlyNumerical <- combined[sapply(combined, function(x) is.numeric(x))]

correlationMatrix <- cor(onlyNumerical)
corrplot(correlationMatrix, method = "color", title = "Correlation of Currencies for last 200 days (roughly)", mar = c(0,0,1,0))

btc <- onlyNumerical$bitcoinClose
eth <- onlyNumerical$ethereumClose
rpl <- onlyNumerical$rippleClose

# layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
hist(btc, main = "Histogram of Bitcoin Closing Price")
hist(eth, main = "Histogram of Ethereum Closing Price")
hist(rpl, main = "Histogram of Ripple Closing Price")

# Compare price density of the 3 most popular cryptocurrencies:
groupIndex <- rep(1:3, c(length(btc), length(eth), length(rpl)))
densityComparison <- sm.density.compare(c(btc, eth, rpl), group = groupIndex, xlab = "Closing Price")

legend("topright", c("Bitcoin", "Ethereum", "Ripple"), lwd = 4, col = c("red", "green", "blue"))
