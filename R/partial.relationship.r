#-------------------------------------------------------------------------------
#	説明変数と応答変数の関係式を描画する。
#-------------------------------------------------------------------------------
#'	(Internal) Draw partial relationship graph.
#'
#'	These functions draw partial relationship graph.
#'	\code{draw.partial.relationship} dispatch data to
#'	\code{draw.partial.relationship.2d} and \code{draw.partial.relationship.3d}
#'	functions based on the number of focal numeric explanatory variables.
#'	\code{draw.partial.relationship.2d} draws two dimentional partial
#'	relationship graph (points and lines) and
#'	\code{draw.partial.relationship.3d} graws three dimentional graph
#'	(image or contour).
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'	@param partial.relationship.data
#'		a data.frame containing data of partial relationship
#-------------------------------------------------------------------------------
draw.partial.relationship <- function(settings) {
	# Make partial relationship data.
	pr.data <- partial.relationship.lsmeans(settings)
	# Dispatch based on number of numeric variables.
	# 数値型の説明変数の数に応じて使う関数を変える。
	numeric.names <- get.numeric.names(settings)
	if (length(numeric.names) == 2) {
		draw.partial.relationship.3d(settings, pr.data)
	} else {
		draw.partial.relationship.2d(settings, pr.data)
	}
}


#-------------------------------------------------------------------------------
#'	@describeIn draw.partial.relationship
#'	Draw 2D partial relationship graph.
#-------------------------------------------------------------------------------
draw.partial.relationship.2d <- function(settings, partial.relationship.data) {
	# Open new plot.
	# 新しいプロットを開く。
	open.new.plot(settings, partial.relationship.data)
	# Prepare color palette.
	# カラーパレットを用意。
	color.palette <- set.group.color(settings, TRUE)
	# Split data.
	# データを分割。
	if (length(names(color.palette)) == 1) {
		partial.relationship.data <- list(all = partial.relationship.data)
	} else {
		factors <- get.factor.names(settings)
		partial.relationship.data <- split(
			partial.relationship.data, partial.relationship.data[factors],
			sep = settings$sep
		)
	}
	# Draw polygons.
	# ポリゴンを描画
	numeric.name <- get.numeric.names(settings)
	for (i in names(color.palette)) {
		d <- partial.relationship.data[[i]]
		x <- c(d[[numeric.name]], rev(d[[numeric.name]]))
		y <- c(d$lower, rev(d$upper))
		polygon(x, y, border = NA, col = trans.color(color.palette[i]))
	}
	# Draw partial relationships.
	# To handle valid graphic paramters in ... for lines, use do.call.
	# 関係式を描画。...の中からlinesで使えるグラフィックパラメーターだけを
	# 使うため、do.callを呼ぶ。
	for (i in names(color.palette)) {
		d <- partial.relationship.data[[i]]
		args <- list(x = d[[numeric.name]], y = d$fit, col = color.palette[i])
		lines.par <- c("lty", "lwd", "lend", "ljoin", "lmitre")
		args <- c(args, settings$other.pars[settings$other.pars %in% lines.par])
		do.call(lines, args)
	}
}


#-------------------------------------------------------------------------------
#'	@describeIn draw.partial.relationship
#'	Draw 3D partial relationship graph.
#-------------------------------------------------------------------------------
draw.partial.relationship.3d <- function(settings, partial.relationship.data) {
	cat("3D plot is not implimented yet...\n")
}


#-------------------------------------------------------------------------------
#	lsmeansを使って予測値と信頼区間を計算する。
#-------------------------------------------------------------------------------
#'	(Internal) Calculate partial regression lines and intervals.
#'
#'	This function calculates data for regression lines and intervals using
#'	\code{\link[lsmeans]{lsmeans}} function.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return
#'		a data.frame containing predictions.
#-------------------------------------------------------------------------------
partial.relationship.lsmeans <- function(settings) {
	# prepare combinations of x variables.
	# 説明変数の組み合わせを用意。
	numerics <- numeric.sequences(settings)
	factors <- get.unique.factors(settings)
	# calculate prediction.
	# 予測値を計算。
	at <- c(numerics, factors)
	rg <- ref.grid(settings$model, at, data = settings$data, type = "terms")
	lsm <- summary(lsmeans(rg, settings$x.names))
	colnames(lsm)[colnames(lsm) == "lsmean"] <- "fit"
	colnames(lsm)[colnames(lsm) == "lower.CL"] <- "lower"
	colnames(lsm)[colnames(lsm) == "upper.CL"] <- "upper"
	colnames(lsm)[colnames(lsm) == "asymp.LCL"] <- "lower"
	colnames(lsm)[colnames(lsm) == "asymp.UCL"] <- "upper"
	# Remove predictions with out-ranged explanatory variable for each group.
	# 各グループの説明変数の範囲を外れた予測値を削除。
	if (length(factors) != 0) {
		lsm <- filter.result(settings, lsm)
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
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'	@param prediction a result of summary.lsmeans.
#'
#'	@return a data.frame containing predicted values.
#-------------------------------------------------------------------------------
filter.result <- function(settings, prediction) {
	# Get list of unique factors.
	# 因子の一覧を作成。
	factors <- expand.grid(get.unique.factors(settings))
	# Get names of numeric variables.
	# 数値型変数の名前を取得。
	numeric.names <- get.numeric.names(settings)
	# Split data and prediction for each factor group.
	# データを因子のグループごとに分割。
	sep = settings$sep
	pred.split <- split(prediction, prediction[names(factors)], sep = sep)
	data.split <- split(settings$data, settings$data[names(factors)], sep = sep)
	# Filter out out-ranged numeric values.
	# 範囲外の数値を削除。
	result <- list()
	for (i in 1:nrow(factors)) {
		split.name <- combine.columns(as.data.frame(factors[i,]), sep = sep)
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


