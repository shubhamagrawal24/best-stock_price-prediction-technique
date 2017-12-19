tryCatch({
	
	input_data <- function(stockVector){
		
		stockVector[[1]] = read.table(paste(getwd(), "/input/ACN.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[2]] = read.table(paste(getwd(), "/input/IBM.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[3]] = read.table(paste(getwd(), "/input/LPL.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[4]] = read.table(paste(getwd(), "/input/INFY.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[5]] = read.table(paste(getwd(), "/input/LHL.F.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[6]] = read.table(paste(getwd(), "/input/TCS.BO.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[7]] = read.table(paste(getwd(), "/input/MARUTI.NS.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[8]] = read.table(paste(getwd(), "/input/HDFCBANK.NS.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[9]] = read.table(paste(getwd(), "/input/ICICIBANK.NS.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[10]] = read.table(paste(getwd(), "/input/SBIN.NS.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[11]] = read.table(paste(getwd(), "/input/CUB.NS.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector[[12]] = read.table(paste(getwd(), "/input/MSFT.csv", sep = ""), sep=",", header=TRUE)
		
		stockVector

	}
},
error = function(e){
	cat("findBestPrediction failed for:",Stockadd);
})