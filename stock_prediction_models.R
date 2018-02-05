source("best_model.R")
source("input_data.R")

library(forecast)

stockVector = list()

stockVector = input_data(stockVector)

BestPrediction = vector()

for(i in 1:length(stockVector))
{
	cat("\n*** Predicting Stock Data No. ",i);
	BestPrediction[i] = findBestModel(stockVector[[i]])
}

cat("\n\n!!! winning models:", BestPrediction)

table = table(BestPrediction)

as.data.frame(table)
