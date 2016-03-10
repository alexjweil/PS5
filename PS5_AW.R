## 4625 - R Programming
## Problem Set 5
## March 10
## Alex Weil

#' Splits a dataframe into a list of two smaller dataframes
#' randomly, based on the percentage provided.
#'
#' @param data The dataframe to split.
#' @param percentage The percentage of the dataframe that should
#' be placed in the first smaller dataframe.
#'
#' @return A list containing two dataframes, $a and $b, that represents
#' a split version of \code{data}.
partition.data = function(data, percentage) {
	indices = sample(1:nrow(data), size=percentage * nrow(data))
	part.a  = data[indices,]
	part.b  = data[-indices,]
	return(list(a=part.a, b=part.b))
}

#' Returns the first digit of every item
#' in the provided data, as a numeric.
#'
#' @param data The target data.
#'
#' @return The first digit of every item, as a numeric.
first.digit = function(data) {
	return(as.numeric(substr(data, 1, 2)))
}

#' Computes the absolute error of the dataset
#'
#' @param predicted The predicted values
#' @param observed  The actual observed values
#'
#' @return A vector containing the absolute error of the two arguments
absolute.error = function(predicted, observed) {
	return(abs(predicted - observed))
}

#' Computes the absolute percentage error of the dataset
#'
#' @param predicted The predicted values
#' @param observed  The actual observed values
#'
#' @return A vector containing the absolute error of the two
absolute.percentage.error = function(predicted, observed) {
	return(absolute.error(predicted, observed) / (abs(observed) + 0.0001) * 100)
}

#' Computes the logarithmic absolute error of the dataset.
#' Replaced invalid values with zero
#'
#' @param predicted The predicted values
#' @param observed  The actual observed values
#'
#' @return A vector containing the absolute error of the two
logarithmic.error = function(predicted, observed) {
	predicted = ifelse(predicted > 0, predicted, 0)
	observed = ifelse(observed  > 0, observed, 0)
	
	return(absolute.error(log(predicted + 1), log(observed + 1)))
}

#' Computes the root mean square of the error
#'
#' @param error The error calculated
#' @seealso \link{absolute.error}
#'
#' @return The root mean square of the error
root.mean.square = function(error) {
	return(sqrt(mean(error^2)))
}



#' Computes descriptive statistics for predicted and observed values.
#'
#' @param observed A vector of observed values. 
#' @param predicted A matrix of predicted values,
#'  with each column representing a model.
#' @param statistics A vector of statistics to include. Valid values are:
#' 	'rmse', 'mad', 'rmsle', 'mape', 'meape'
#' Defaults to all of the above.
#'
#' @return A matrix with each model as a row, and each statistic as a column.
descriptive.statistics = function(observed, predicted, statistics = c('rmse', 'mad', 'rmsle', 'mape', 'meape')) {
	error            = apply(predicted, 2, absolute.error, observed=observed)
	log.error        = apply(predicted, 2, logarithmic.error, observed=observed)
	percentage.error = apply(predicted, 2, absolute.percentage.error, observed=observed)
	
	rmse  = apply(error,            2, root.mean.square)
	mad   = apply(error,            2, median)
	rmsle = apply(log.error,        2, root.mean.square)
	mape  = apply(percentage.error, 2, mean)
	meape = apply(percentage.error, 2, median)
	return(cbind(rmse, mad, rmsle, mape, meape)[,statistics])
}

rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("./PS5/anes_timeseries_2012_stata12.dta")

### Question 1
## Randomly subset the data into two partitions
split.data   = partition.data(anes, 0.5)
training.set = split.data$a
test.set     = split.data$b

training.thermometer = as.numeric(training.set$ft_dpc)
test.thermometer     = as.numeric(test.set$ft_dpc)

## Use the "training set" to build at least three models
# Model 1: Feeling thermometer as a linear function of prevote ballot color
ballot.colors = first.digit(training.set$prevote_ballot_color)

color.model   = lm(training.thermometer ~ ballot.colors, training.set)

# Model 2: Feeling thermometer as a log-lin function of political party
# Exclude voters without a Republican or Democractic preference
party   = first.digit(training.set$prevote_regpty)
party.model = lm(training.thermometer ~ log(ifelse(party > 0, party, 0.0001)), training.set)

# Model 3: Feeling thermometer as a linear function of happiness
happiness       = first.digit(training.set$happ_lifesatisf)
happiness.model = lm(training.thermometer ~ happiness, training.set)


### Question 2
# The predictions using various models
color.prediction     = predict(color.model)
party.prediction	 = predict(party.model)
happiness.prediction = predict(happiness.model)

predictions = as.matrix(cbind(color.prediction, party.prediction, happiness.prediction))


# As expected, none of these variables are particularly useful
# in predicting Obama's thermometer score. They have nearly identical
# RMSE and RMSLE values, suggesting similar inadequacy.
#
# 						   rmse      mad    rmsle    mape    meape
# color.prediction     34.77530 31.82167 1.555526 7052406 37.43722
# party.prediction     34.33195 28.16526 1.555805 7091620 41.29037
# happiness.prediction 34.37994 27.88597 1.556038 7095023 40.18706
print(descriptive.statistics(test.thermometer, predictions))

