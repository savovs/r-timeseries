dataPath <- "data/bitcoin_dataset.csv"
data <- read.csv(dataPath)
data$Date <- as.Date(data$Date)

data <- subset(data, data$Date > "2017-01-01")
data <- data[sample(nrow(data)),]

numFolds <- 10
folds <- cut(seq(1, nrow(data)), breaks = numFolds, labels = FALSE)

errors <- NULL
for(i in 1:numFolds) {
  testIndices <- which(folds == i, arr.ind = TRUE)
  
  trainData <- data[-testIndices,]
  testData <- data[testIndices,]
  
  model <- lm(btc_market_price ~ Date + btc_trade_volume, data = trainData)
  pricePredictions <- predict(model, testData)
  
  error <- rmse(testData$btc_market_price, pricePredictions)
  errors <- c(errors, error)
  print("RMSE:")
  print(error)
}

plot(pricePredictions, type = "l", col = "red", main = sprintf("BTC Linear Model, 10-Fold Mean RMSE: %#.1f", mean(errors)), xlab = "Date Index")
lines(testData$btc_market_price)
legend("topright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))