library(forecast)

tryCatch({

	findBestModel <- function(Stockadd){

		Stock = Stockadd

		# Convert data into time series data
		tryCatch({
			tsStock = ts(Stock$Close, start = c(2004, 1), frequency = 12)
		},
		error = function(e){
			tsStock = ts(Stock$Close, start = c(2010, 1), frequency = 12)
		})

		# train data
		tryCatch({
			train <- window(tsStock, end = 2017)
		},
		error = function(e){
			train = 0
		})

		# test data
		tryCatch({
			test <- window(tsStock, start = 2017)
		},
		error = function(e){
			test = 0
		})

		tl = seq(2004, 2017, length = length(train))

		tl2 = tl^7

		# Actual functions of stock prices can be chaotic and misleading for predictions
		# Create polynomial model
		tryCatch({
			polyStock = lm(train ~ tl + tl2)
		},
		error = function(e){
			polyStock = 0
		})

		# Convert polynomial data into time series data
		tryCatch({
			tsStocktrend1 = ts(polyStock$fit, start = c(2004, 1), frequency = 12)
		},
		error = function(e){
			tsStocktrend1 = 0
		})

		# Actual functions of stock prices can be chaotic and misleading for predictions
		# Create stl model
		tryCatch({
			stlStock = stl(train, s.window = "periodic")
		},
		error = function(e){
			stlStock = 0
		})

		# Convert stl data into time series data
		tryCatch({
			tsStocktrend2 = stlStock$time.series[ ,2]
		},
		error = function(e){
			tsStocktrend2 = 0
		})

		# 1
		# Arima model on actual trend
		tryCatch({
			autofitr = auto.arima(train)
		},
		error = function(e){
			autofitr = 0
		})

		# 2
		# Arima model on actual trend with non-seasonal parameters
		tryCatch({
			fitr <- arima(train, order = c(15, 3, 3))
		},
		error = function(e){
			fitr = 0
		})

		# 3
		# Arima model on actual trend with non-seasonal & seasonal parameters
		tryCatch({
			fitr2 <- arima(train, order = c(1, 0, 0), list(order = c(2, 1, 0), period = 12))
		},
		error = function(e){
			fitr2 = 0
		})

		# 4
		# Neural network on actual trend (feed-forward type)
		tryCatch({
			NETfitr <- nnetar(train)
		},
		error = function(e){
			NETfitr = 0
		})

		# 5
		# Holt Winter filtering on actual trend with minimized square error & without using exponential smoothing
		tryCatch({
			HWStockr = HoltWinters(train)
		},
		error = function(e){
			HWStockr = 0
		})

		# 6
		# Holt Winter filtering on actual trend with minimized square error & using exponential smoothing
		tryCatch({
			HWStockr_ng = HoltWinters(train, gamma = FALSE)
		},
		error = function(e){
			HWStockr_ng = 0
		})

		# 7
		# Arima model on STL trend
		tryCatch({
			autofit2 = auto.arima(tsStocktrend2)
		},
		error = function(e){
			autofit2 = 0
		})

		# 8
		# Arima model on polynomial trend with non-seasonal & seasonal parameters
		tryCatch({
			fit12 <- arima(tsStocktrend1, order = c(1, 0, 0), list(order = c(2, 1, 0), period = 12))
		},
		error = function(e){
			fit12 = 0
		})

		# 9
		# Arima model on STL trend with non-seasonal parameters
		tryCatch({
			fit2 <- arima(tsStocktrend2, order = c(15, 3, 3))
		},
		error = function(e){
			fit2 = 0
		})

		# 10
		# Arima model on STL trend with non-seasonal & seasonal parameters
		tryCatch({
			fit22 <- arima(tsStocktrend2, order = c(1, 0, 0), list(order = c(2, 1, 0), period = 12))
		},
		error = function(e){
			fit22 = 0
		})

		# 11
		# STL model on polynomial trend
		tryCatch({
			stlStock1 = stl(tsStocktrend1, s.window = "periodic")
		},
		error = function(e){
			stlStock1 = 0
		})

		# 12
		# STL model on stl trend
		tryCatch({
			stlStock2 = stl(tsStocktrend2, s.window = "periodic")
		},
		error = function(e){
			stlStock2 = 0
		})

		# 13
		# STL model on actual trend
		tryCatch({
			stlStockr = stl(train, s.window = "periodic")
		},
		error = function(e){
			stlStockr = 0
		})

		# 14
		# Neural network on stl trend (feed-forward type)
		tryCatch({
			NETfit2 <- nnetar(tsStocktrend2)
		},
		error = function(e){
			NETfit2 = 0
		})

		# 15
		# Holt Winter filtering on stl trend with minimized square error & without using exponential smoothing
		tryCatch({
			HWStock2 = HoltWinters(tsStocktrend2)
		},
		error = function(e){
			HWStock2 = 0
		})

		# 16
		# Holt Winter filtering on stl trend with minimized square error & using exponential smoothing
		tryCatch({
			HWStock2_ng = HoltWinters(tsStocktrend2, gamma = FALSE)
			
		},
		error = function(e){
			HWStock2_ng = 0
		})

		# 17
		# Arima model on polynomial trend
		tryCatch({
			autofit1 = auto.arima(tsStocktrend1)
		},
		error = function(e){
			autofit1 = 0
		})

		# 18
		# Linear model on actual trend (adds trend & season componets to computation)
		tryCatch({
			fitlr <- tslm(train ~ trend + season, lambda = 0)
		},
		error = function(e){
			fitlr = 0
		})

		# 19
		# Linear model on polynomial trend (adds trend & season componets to computation)
		tryCatch({
			fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda = 0)
		},
		error = function(e){
			fitl1 = 0
		})

		# 20
		# Linear model on stl trend (adds trend & season componets to computation)
		tryCatch({
			fitl2 <- tslm(tsStocktrend2 ~ trend + season, lambda = 0)
		},
		error = function(e){
			fitl2 = 0
		})

		# 21
		# Neural network on polynomial trend (feed-forward type)
		tryCatch({
			NETfit1 <- nnetar(tsStocktrend1)
		},
		error = function(e){
			NETfit1 = 0
		})

		# 22
		# Holt Winter filtering on polynomial trend with minimized square error & using exponential smoothing
		tryCatch({
			HWStock1_ng = HoltWinters(tsStocktrend1, gamma = FALSE)
		},
		error = function(e){
			HWStock1_ng = 0
		})

		# 23 & 24 & 25
		# Holt winter filtering on polynomial trend for fit, lwr, upr predictions
		tryCatch({
			HWStock1 = HoltWinters(tsStocktrend1)
		},
		error = function(e){
			HWStock1 = 0
		})

		# 1
		tryCatch({
			predautofitr = window(forecast(autofitr, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predautofitr = 0
		})

		# 2
		tryCatch({
			predfitr = window(forecast(fitr, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfitr = 0
		})

		# 3
		tryCatch({
			predfitr2 = window(forecast(fitr2, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfitr2 = 0
		})

		# 4
		tryCatch({
			predNETfitr = window(forecast(NETfitr, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predNETfitr = 0
		})

		# 5
		tryCatch({
			predHWStockr = window(predict(HWStockr, n.ahead = 39), start = 2017)
		},
		error = function(e){
			predHWStockr = 0
		})

		# 6
		tryCatch({
			predHWStockr_ng = window(predict(HWStockr_ng, n.ahead = 39), start = 2017)
		},
		error = function(e){
			predHWStockr_ng  = 0
		})

		# 7
		tryCatch({
			predautofit2 = window(forecast(autofit2, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predautofit2 = 0
		})

		# 8
		tryCatch({
			predfit12 = window(forecast(fit12, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfit12 = 0
		})

		# 9
		tryCatch({
			predfit2 = window(forecast(fit2, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfit2 = 0
		})

		# 10
		tryCatch({
			predfit22 = window(forecast(fit22, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfit22 = 0
		})

		# 11
		tryCatch({
			predstlStock1 = window( forecast(stlStock1, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predstlStock1 = 0
		})

		# 12
		tryCatch({
			predstlStock2 = window(forecast(stlStock2, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predstlStock2 = 0
		})

		# 13
		tryCatch({
			predstlStockr = window(forecast(stlStockr, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predstlStockr = 0
		})

		# 14
		tryCatch({
			predNETfit2 = window(forecast(NETfit2, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predNETfit2 = 0
		})

		# 15
		tryCatch({
			predHWStock2 = window(predict(HWStock2, n.ahead = 39), start = 2017)
		},
		error = function(e){
			predHWStock2 = 0
		})

		# 16
		tryCatch({
			predHWStock2_ng = window(predict(HWStock2_ng, n.ahead = 39), start = 2017)
			print(predict(predHWStock2_ng, n.ahead = 12))
		},
		error = function(e){
			predHWStock2_ng = 0
		})

		# 17
		tryCatch({
			predautofit1 = window(forecast(autofit1, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predautofit1 = 0
		})

		# 18
		tryCatch({
			predfitlr = window(forecast(fitlr, h = 39)$mean , start = 2017)
		},
		error = function(e){
			predfitlr = 0
		})

		# 19
		tryCatch({
			predfitl1 = window(forecast(fitl1, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predfitl1 = 0
		})

		# 20
		tryCatch({
			predfitl2 = window(forecast(fitl2, h = 39)$mean , start = 2017)
		},
		error = function(e){
			predfitl2 = 0
		})

		# 21
		tryCatch({
			predNETfit1 = window(forecast(NETfit1, h = 39)$mean, start = 2017)
		},
		error = function(e){
			predNETfit1 = 0
		})

		# 22
		tryCatch({
			predHWStock1_ng = window(predict(HWStock1_ng, n.ahead = 39), start = 2017)
		},
		error = function(e){
			predHWStock1_ng = 0
		})

		# 23
		tryCatch({
			predHWStock11 = window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,1], start = 2017)
		},
		error = function(e){
			predHWStock11 = 0
		})

		# 24
		tryCatch({
			predHWStock12 = window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,2], start = 2017)
		},
		error = function(e){
			predHWStock12 = 0
		})

		# 25
		tryCatch({
			predHWStock13 = window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,3], start = 2017)
		},
		error = function(e){
			predHWStock13 = 0
		})

		tryCatch({
			mae = matrix(NA, 25, length(test)+1)
		},
		error = function(e){
			mae = matrix(NA, 25, 50)
		})

		for(i in 1:length(test)){

			tryCatch({
				mae[1, i] <- abs(predautofitr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[2, i] <- abs(predfitr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[3, i] <- abs(predfitr2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[4, i] <- abs(predNETfitr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[5, i] <- abs(predHWStockr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[6, i] <- abs(predHWStockr_ng[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[7, i] <- abs(predautofit2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[8, i] <- abs(predfit12[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[9, i] <- abs(predfit2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[10, i] <- abs(predfit22[i]-test[i])
			},
			error=function(e) { })

			tryCatch({
				mae[11, i] <- abs(predstlStock1[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[12, i] <- abs(predstlStock2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[13, i] <- abs(predstlStockr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[14, i] <- abs(predNETfit2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[15, i] <- abs(predHWStock2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[16, i] <- abs(predHWStock2_ng[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[17, i] <- abs(predautofit1[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[18, i] <- abs(predfitlr[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[19, i] <- abs(predfitl1[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[20, i] <- abs(predfitl2[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[21, i] <- abs(predHWStock1_ng[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[22, i] <- abs(predNETfit1[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[23, i] <- abs(predHWStock11[i]-test[i])
			},
			error=function(e) { })

			tryCatch({
				mae[24, i] <- abs(predHWStock12[i]-test[i])
			},
			error = function(e) { })

			tryCatch({
				mae[25, i] <- abs(predHWStock13[i]-test[i])
			},
			error=function(e) { })
		}

		for(i in 1:25)
		{
			mae[i,5] = sum(mae[i,1:4])
		}

		best = which.min(mae[1:25,5])

		cat(" ==> Winning Model ID: ", best )

		return (best)
	}
},
error = function(e){
	cat("findBestPrediction failed for:", Stockadd);
})