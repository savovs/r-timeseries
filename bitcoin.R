dataPath <- file.path(getwd(), "data/bitcoin_dataset.csv")
data <- read.csv(dataPath)
data$Date <- as.Date(data$Date)

data <- subset(data, data$Date > "2017-01-01")

# plot(data$Date, data$btc_market_price, type = "l", main = "Bitcoin", xlab = "Time", ylab = "Price")

timeseries <- ts(data[,c("btc_market_price", "btc_trade_volume", "btc_hash_rate", "btc_transaction_fees")])
plot.ts(timeseries)

# holt <- HoltWinters(log(ts(data[, "btc_market_price"])))
# "Error: Time series has no or less than 2 periods." Seems like bitcoin is too unpredictable for Holt Winters.

timeseries <- ts(data[,c("btc_market_price", "btc_trade_volume")])

# Correlogram, shows autocorrelation and cross-correlation,
# used to see how much impact past values have on future values
# gets harder to predict with bigger lag (i.e. further into the future)
# if correlation is high, linear regression can be used to make short-term predictions 
acf(timeseries, lag.max = 40)