#------------------------------------------------------------------------------
#'	(Internal) Calculate partial residual.
#'
#'	This function calculates partial residual for specfied model.
#'	Resultant values are adjusted to be congruent with result of
#'	\code{\link[lsmeans]{lsmeans}}.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return a numeric vector of partial residuals.
#------------------------------------------------------------------------------
#	PL      = i + PW + SL + SP + PW:SP + SL:SP + r		# FULL MODEL
#	PL      = i + PW      + SP + PW:SP         + r		# Want to see this
#	PL      =        + SL              + SL:SP		    # Remove these effects
#	predict = i + PW + SL + SP + PW:SP + SL:SP
#
#	Case of LSMEANS
#	LSMEANS = i + PW + MM + SP + PW:SP + MM:SP
#	LSMEANS = i + PW + MM + SP + PW:SP + MM:SP + r		# Want to see this
#------------------------------------------------------------------------------
partial.residual <- function(settings) {
	# Prepare names of numeric variables.
	# 数値型変数の変数名を用意。
	all.numerics <- settings$adapter$x.names(type = "base")
	all.numerics <- all.numerics[
		sapply(settings$data[all.numerics], is.numeric)
	]
	other.numerics <- all.numerics[!all.numerics %in% settings$x.names.numeric]
	# Calculate prediction 1.
	# 予測値１を計算。
	data1 <- settings$data
	for (i in other.numerics) {
		data1[[i]] <- data1[[i]] - mean(data1[[i]])
	}
	data1[settings$x.names.numeric] <- 0
	pred1 <- settings$adapter$predict(newdata = data1, type = "link")
	pred1 <- pred1$fit[, "fit"]
	# Calculate prediction 2.
	# 予測値２を計算。
	data2 <- settings$data
	data2[all.numerics] <- 0
	pred2 <- settings$adapter$predict(newdata = data2, type = "link")
	pred2 <- pred2$fit[, "fit"]
	# Calculate partial residual.
	# 偏残差を計算。
	resid <- (
		settings$adapter$link(settings$data[[settings$adapter$y.names()]])
		- (pred1 - pred2)
	)
	return(resid)
}


#------------------------------------------------------------------------------
#	偏残差の点を描画。
#------------------------------------------------------------------------------
#'	(Internal) Draw partial residual graph.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#------------------------------------------------------------------------------
draw.partial.residual <- function(settings) {
	# Calculate and draw partial residual
	# 偏残差を計算して描画。
	part.resid <- partial.residual(settings)
	if (!settings$draw.relationships) {
		args <- list(
			settings$data[[settings$x.names.numeric]], part.resid,
			col = settings$obs.colors,
			xlab = settings$xlab, ylab = settings$ylab
		)
		args <- c(args, settings$other.pars)
		do.call(plot, args)
	} else {
		# To handle graphic parameters in ..., use do.call,
		# not directly call points function.
		# ...に入ったグラフィックパラメーターを制御するため、
		# points関数を直接呼ばずにdo.callを使う。
		args <- list(
			x = settings$data[[settings$x.names.numeric]],
			y = part.resid, col = settings$obs.colors
		)
		args <- settings$set.function.args(args, points)
		do.call(points, args)
	}
}

