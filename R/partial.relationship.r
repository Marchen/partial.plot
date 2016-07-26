#-------------------------------------------------------------------------------
#	lsmeansを使って予測値と信頼区間を計算する。
#-------------------------------------------------------------------------------
#'	(Internal) Calculate partial regression lines and intervals.
#'
#'	This function calculates data for regression lines and intervals using
#'	\code{\link[lsmeans]{lsmeans}} function.
#'
#'	@param model
#'		a model object for which partial regression lines and its confidence 
#'		intervals are calculated.
#'
#'	@param x.names 
#'		a character vector containing names of explanatory variables
#'		to focus on them.
#'
#'	@param data
#'		a data.frame containing data used for prediction.
#'		If data is missing, this function try to extract data.frame from
#'		\code{model} object.
#'
#'	@param resolution
#'		an integer specifing length of prediction to calculate predictions.
#'
#'	@return
#'		a data.frame containing predictions.
#-------------------------------------------------------------------------------
partial.relationship.lsmeans <- function(
	model, x.names, data = NULL, resolution = 100
) {
	# prepare combinations of x variables.
	# 説明変数の組み合わせを用意。
	adapter <- model.adapter(model, data = data)
	numerics <- numeric.sequences(adapter$data, x.names, resolution)
	factors <- get.unique.factors(adapter$data, x.names)
	# calculate prediction.
	# 予測値を計算。
	at <- c(numerics, factors)
	rg <- ref.grid(model, at, data = adapter$data, type = "terms")
	lsm <- summary(lsmeans(rg, c(x.names)))
	colnames(lsm)[colnames(lsm) == "lsmean"] <- "fit"
	colnames(lsm)[colnames(lsm) == "lower.CL"] <- "lower"
	colnames(lsm)[colnames(lsm) == "upper.CL"] <- "upper"
	colnames(lsm)[colnames(lsm) == "asymp.LCL"] <- "lower"
	colnames(lsm)[colnames(lsm) == "asymp.UCL"] <- "upper"
	# Remove predictions with out-ranged explanatory variable for each group.
	# 各グループの説明変数の範囲を外れた予測値を削除。
	if (length(factors) != 0) {
		lsm <- filter.result(lsm, adapter$data, x.names)
	}
	return(lsm)
}

#-------------------------------------------------------------------------------
#	lsmeansの結果から、各グループごとにX軸の値が元のデータの範囲外に
#	ある予測値を削除する。
#-------------------------------------------------------------------------------
#'	(Internal) Remove out-ranged values from result of lsmeans.
#'
#'	This internal function removes predicted values those explanatory 
#'	variable is out of range of original data used for modeling for each group.
#'
#'	@param prediction a result of summary.lsmeans.
#'	@param data a data.frame of original data used for modeling.
#'	@param x.names a character vector of names of focal explanatory variables.
#'
#'	@return a data.frame containing predicted values.
#-------------------------------------------------------------------------------
filter.result <- function(prediction, data, x.names) {
	# Get list of unique factors.
	# 因子の一覧を作成。
	factors <- expand.grid(get.unique.factors(data, x.names))
	# Get names of numeric variables.
	# 数値型変数の名前を取得。
	numeric.names <- get.numeric.names(data, x.names)
	# Split data and prediction for each factor group.
	# データを因子のグループごとに分割。
	pred.split <- split(prediction, prediction[names(factors)])
	data.split <- split(data, data[names(factors)])
	# Filter out out-ranged numeric values.
	# 範囲外の数値を削除。
	result <- list()
	for (i in 1:nrow(factors)) {
		split.name <- combine.columns(as.data.frame(factors[i,]))
		current.pred <- pred.split[[split.name]]
		current.data <- data.split[[split.name]]
		for (numeric.name in numeric.names) {
			var.range <- range(current.data[[numeric.name]])
			filter <- current.pred[[numeric.name]] >= var.range[1]
			filter <- filter & current.pred[[numeric.name]] <= var.range[2]
			current.pred <- current.pred[filter,]
		}
		result[[split.name]] <- current.pred
	}
	result <- do.call(rbind, result)
	return(result)
}


