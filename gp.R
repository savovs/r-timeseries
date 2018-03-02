library(rgp)

dataPath <- "data/bitcoin_dataset.csv"
data <- read.csv(dataPath)
data$Date <- as.Date(data$Date)

data <- subset(data, data$Date > "2017-01-01")
plot(data$Date, data$btc_market_price, type = "l", main = "Bitcoin", xlab = "Time", ylab = "Price")

timeseries <- ts(data[,c("btc_market_price", "btc_trade_volume", "btc_hash_rate", "btc_transaction_fees")])
plot.ts(timeseries)

# price <- ts(data$btc_market_price)
# holt <- HoltWinters(price)
# plot(holt)
# "Error: Time series has no or less than 2 periods." Seems like bitcoin is too unpredictable for Holt Winters.



# Correlogram, shows autocorrelation and cross-correlation,
# used to see how much impact past values have on future values
# gets harder to predict with bigger lag (i.e. further into the future)
# if correlation is high, linear regression can be used to make short-term predictions
timeseries <- ts(data[,c("btc_market_price", "btc_trade_volume")])
acf(timeseries, lag.max = 40)

# 
# Genetic Programming bit
#

# K Fold Validation
data <- data[sample(nrow(data)),]

numFolds <- 10
folds <- cut(seq(1, nrow(data)), breaks = numFolds, labels = FALSE)

windowSize <- 4
pricesVariableSet <- do.call(inputVariableSet, as.list(sprintf("price%d", 1:windowSize)))

errors <- NULL

# For plotting, 2 rows of 5 columns
# par(mfrow=c(2, 5)) 

for(i in 1:numFolds) {
  testIndices <- which(folds == i, arr.ind = TRUE)

  trainData <- data[-testIndices,]
  testData <- data[testIndices,]
  
  gpResult <- geneticProgramming(
    functionSet = functionSet("+", "-", "*", "/"),
    inputVariables = pricesVariableSet,
    constantSet = constantFactorySet(function() rnorm(1)),
    stopCondition = makeTimeStopCondition(60),

    fitnessFunction = function(f) {
      result = NULL

      for(i in 1:(nrow(trainData) - windowSize)) {
        result[i] = do.call(f, as.list(trainData$btc_market_price[i:(i + windowSize - 1)]))
      }

      rangeToPredict = (windowSize + 1):nrow(trainData)
      fitness = rmse(result, trainData$btc_market_price[rangeToPredict])

      if (is.na(fitness)) {
        return(Inf)
      }
      
      return(fitness)
    }
  )

  bestFunction <- gpResult$population[[which.min(gpResult$fitnessValues)]]

  trainResults <- NULL

  # Different indices because scope screws with plot names
  for(j in 1:(nrow(trainData) - windowSize)) {
    trainResults[j] <- do.call(bestFunction, as.list(trainData$btc_market_price[j:(j+ windowSize - 1)]))
  }

  predictions <- trainData$btc_market_price[(nrow(trainData) - windowSize + 1):nrow(trainData)]
  
  for(k in 1:(windowSize + 1)) {
    predictions[k + windowSize] <- do.call(bestFunction, as.list(predictions[k:(k + windowSize - 1)]))
  }

  predictionError <- rmse(predictions[(windowSize + 1):(windowSize * 2 + 1)], testData$btc_market_price[1:(windowSize + 1)])
  errors <- c(errors, predictionError)
  
  print(sprintf("RMSE: %d", i))
  print(predictionError)
  
  
  # png(filename = sprintf("plots/gp/kFold%d.png", i))

  plot(testData$btc_market_price[1:(windowSize + 1)], type = "l", ylab = "Price", main = sprintf("RMSE: %#.1f", predictionError))
  lines(predictions[(windowSize + 1):(windowSize * 2 + 1)], col = "red")
  
  # legend("topright", c("Actual", "Prediction"), lwd = 4, col = c("black", "red"))
  
  # dev.off()
}

print("Mean of the k-fold RMSEs: ")
print(mean(errors))