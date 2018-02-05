library(forecast)

# input data
Stock = read.table(paste(getwd(), "/input/CUB.NS.csv", sep = ""), sep=",", header=TRUE)

# Convert data into time series data
tsStock = ts(Stock$Close, start = c(2004, 1), frequency = 12)
plot(tsStock)

# Actual functions of stock prices can be chaotic and misleading for predictions
# Create polynomial model & convert it into time series data
tl = seq(2004, 2017,length = length(tsStock))
tl2 = tl^7
polyStock = lm(tsStock ~ tl + tl2)
tsStocktrend1 = ts(polyStock$fit, start = c(2004, 1), frequency = 12)
plot(tsStock, lw = 2, col = "blue", xlim = c(2004, 2017))
lines(tsStocktrend1, lw = 2, col = "red")
abline(v = 2017.25, lty = 2)

# Actual functions of stock prices can be chaotic and misleading for predictions
# Create stl model & convert it into time series data
stlStock = stl(tsStock, s.window = "periodic")
plot(stlStock, col = "blue", lw = 2)
tsStocktrend2 = stlStock$time.series[ , 2]
plot(forecast(stlStock))
abline(v = 2017.25, lty = 2)

# Actual function, STL trend, Polynomial trend
plot(tsStock, lw = 2)
lines(tsStocktrend1, col = 'purple', lw = 2)
lines(tsStocktrend2, col = 'red', lw = 2)
abline(v = 2017.25, lty = 2)
legend("topleft", legend = c("Actual Function", "STL Trend", "Polynomial Trend"),
	   col = c("black", "red", "purple"), lw = 2)

## Prediction on polynomial trend

# Holt Winter filtering on polynomial trend with minimized square error & using exponential smoothing
HWStock1_ng = HoltWinters(tsStocktrend1, gamma = FALSE)
# Holt winter filtering on polynomial trend for fit, lwr, upr predictions
HWStock1 = HoltWinters(tsStocktrend1)
# Neural network on polynomial trend (feed-forward type)
NETfit1 = nnetar(tsStocktrend1)
# Arima model on polynomial trend
autofit1 = auto.arima(tsStocktrend1)
# Linear model on polynomial trend (adds trend & season componets to computation)
fitl1 = tslm(tsStocktrend1 ~ trend + season, lambda = 0)
# Arima model on polynomial trend with non-seasonal & seasonal parameters
fit12 = arima(tsStocktrend1, order = c(1, 0, 0), list(order = c(2, 1, 0), period = 5))
# STL model on polynomial trend
stlStock1 = stl(tsStocktrend1, s.window = "periodic")
plot(forecast(autofit1, h = 24), xlim = c(2004, 2019), ylim = c(-50, 250),
	 lw = 2, col = "red", xlab = "Time", ylab = "Stock Price",
	 main = "Predictions of the Polynomial Trend")
lines(tsStock, lw = 2)
lines(forecast(stlStock1, h = 24)$mean, col = "red", lw = 2)
lines(forecast(fitl1, h = 24)$mean, col = "orange")
lines(forecast(NETfit1, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")
lines(predict(HWStock1_ng, n.ahead = 24), lw = 2, col = "green")
lines(forecast(fit12, h = 24)$mean, lw = 2, col = "purple")
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 1],
	  lw = 2, col = "green")
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 2],
	  col = "green")
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 3],
	  col = "green")
legend("topleft", legend = c("Actual Function", "Polynomial Trend",
	"Prediction - Holt Winters", "Prediction - Arima (auto)", "Prediction - Arima (fixed)",
	"Prediction - Neural Network", "Prediction - Linear Model"),
	col = c("black", "red", "green", "blue", "purple", "brown", "orange"), lw = 2)
abline(v = 2017.25, lty = 2)

## Prediction on STL trend

# Holt Winter filtering on stl trend with minimized square error & using exponential smoothing
HWStock2_ng = HoltWinters(tsStocktrend2, gamma = FALSE)
# Holt Winter filtering on stl trend with minimized square error & without using exponential smoothing
HWStock2 = HoltWinters(tsStocktrend2)
# Neural network on stl trend (feed-forward type)
NETfit2 = nnetar(tsStocktrend2)
# Arima model on STL trend
autofit2 = auto.arima(tsStocktrend2)
# Linear model on stl trend (adds trend & season componets to computation)
fitl2 = tslm(tsStocktrend2 ~ trend + season, lambda = 0)
# Arima model on STL trend with non-seasonal parameters
fit2 = Arima(tsStocktrend2, order = c(15, 3, 3))
# STL model on stl trend
stlStock2 = stl(tsStocktrend2, s.window = "periodic")
plot(forecast(autofit2, h = 24), xlim = c(2004, 2019), ylim = c(-50, 250),
	 lw = 2, col = "blue", xlab = "Time", ylab = "Stock Price",
	 main = "Predictions of STL Trend")
lines(tsStock, lw = 2)
lines(forecast(stlStock2, h = 24)$mean, col = "red", lw = 2)
lines(forecast(fitl2, h = 24)$mean, col = "orange")
lines(forecast(fit2, h = 24)$mean, lw = 2, col = "purple")
lines(tsStocktrend2, lw = 2, col = "red")
lines(forecast(NETfit2, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")
lines(predict(HWStock2, n.ahead = 24), lw = 2, col = "green")
lines(predict(HWStock2_ng, n.ahead = 24), lw = 2, col = "green")
lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 1],
	  lw = 2, col = "orange")
lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 2],
	  col = "orange")
lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 3],
	  col = "orange")
legend("topleft", legend = c("Actual Function", "STL Trend",
	 "Prediction - Holt Winters", "Prediction - Arima (auto)", "Prediction - Arima (fixed)",
	 "Prediction - Neural Network", "Prediction - Linear Model"),
	 col = c("black", "red", "green", "blue", "purple", "brown", "orange"), lw = 2)
abline(v = 2017.25, lty = 2)


## Prediction on actual trend

# Holt Winter filtering on actual trend with minimized square error & using exponential smoothing
HWStockr_ng = HoltWinters(tsStock, gamma = F)
# Holt Winter filtering on actual trend with minimized square error & without using exponential smoothing
HWStockr = HoltWinters(tsStock)
# Neural network on actual trend (feed-forward type)
NETfitr = nnetar(tsStock)
# Arima model on actual trend
autofitr = auto.arima(tsStock)
# Arima model on actual trend with non-seasonal parameters
fitr = Arima(tsStock, order = c(15, 3, 3))
# Arima model on actual trend with non-seasonal & seasonal parameters
fitr2 = arima(tsStock, order = c(1, 0, 0), list(order = c(2, 1, 0), period = 9))
# Linear model on actual trend (adds trend & season componets to computation)
fitlr = tslm(tsStock ~ trend + season, lambda = 0)
# STL model on actual trend
stlStockr = stl(tsStock, s.window = "periodic")

plot(forecast(autofitr, h = 24), xlim = c(2004, 2019), ylim = c(-50, 250),
	 lw = 2, col = "blue", xlab = "Time", ylab = "Stock Price",
	 main = "Predictions of the Actual Model")
lines(tsStock, lw = 2)
lines(forecast(stlStockr, h = 24)$mean, col = "red", lw = 2)
lines(forecast(fitr, h = 24)$mean, col = "purple")
lines(forecast(fitr2, h = 24)$mean, lw = 2, col = "purple")
lines(forecast(fitlr, h = 24)$mean, lw = 2, col = "orange")
lines(forecast(NETfitr, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")
lines(predict(HWStockr, n.ahead = 24), lw = 2, col = "green")
lines(predict(HWStockr_ng, n.ahead = 24), lw = 2, col = "green")
legend("topleft", legend = c("Actual Function", "Prediction - Holt Winters",
		"Prediction - Arima (auto)", "Prediction - Arima (fixed)",
		"Prediction - Neural Network", "Prediction - Linear Model"),
	   col = c("black", "red", "green", "blue", "purple", "brown", "orange"), lw = 2)
abline(v = 2017.25, lty = 2)


plot(forecast(autofitr, h = 24), xlim = c(2004, 2019.2), ylim = c(-50, 250),
	 lw = 2, col = "blue", xlab = "Time", ylab = "Stock Price",
	 main = "All 24 predictions computing")

lines(forecast(fitr, h = 24)$mean, col = "purple")
lines(forecast(fitr2, h = 24)$mean, lw = 2, col = "purple")
lines(tsStock, lw = 2)
lines(forecast(NETfitr, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")
lines(predict(HWStockr, n.ahead = 24), lw = 2, col = "green")
lines(predict(HWStockr_ng, n.ahead = 24), lw = 2, col = "green")
lines(forecast(autofit2, h = 24)$mean, lw = 2, col = "blue")
lines(forecast(fitl2, h = 24)$mean, lw = 2, col = "purple")
lines(forecast(stlStock1, h = 24)$mean, col = "yellow", lw = 2)
lines(forecast(stlStock2, h = 24)$mean, col = "yellow", lw = 2)
lines(forecast(stlStockr, h = 24)$mean, col = "yellow", lw = 2)
lines(forecast(fit2, h = 24)$mean, lw = 2, col = "purple")
lines(tsStocktrend1, col = 'red', lw = 2)
lines(tsStocktrend2, col = 'red', lw = 2)
lines(forecast(NETfit2, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")
lines(predict(HWStock2, n.ahead = 24), lw = 2, col = "green")
lines(predict(HWStock2_ng, n.ahead = 24), lw = 2, col = "green")
lines(forecast(autofit1, h = 24)$mean, lw = 2, col = "green")

lines(forecast(fitl1, h = 24)$mean, col = "orange")
lines(forecast(fitl2, h = 24)$mean, lw = 2, col = "orange")
lines(forecast(fitlr, h = 24)$mean, lw = 2, col = "orange")

lines(tsStock, lw = 2)

lines(forecast(NETfit1, h = 24)$mean, lw = 2, lty = "longdash", col = "brown")

lines(predict(HWStock1_ng, n.ahead = 24), lw = 2, col = "green")

lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 1],
	  lw = 2, col = "green")
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 2],
	  col = "green")
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[ , 3],
	  col = "green")
legend("topleft", legend = c("Actual Function", "Prediction - Holt Winters",
							 "Prediction - Arima (auto)", "Prediction - Arima (fixed)",
							 "Prediction - Neural Network", "Prediction - Linear Model"),
	   col = c("black", "red", "green", "blue", "purple", "brown", "orange"), lw = 2)
abline(v = 2017.25, lty = 2)
