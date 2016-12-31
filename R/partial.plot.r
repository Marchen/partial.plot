#-------------------------------------------------------------------------------
#	指定されたデータフレームを確認して、指定した変数名から指定された型の
#	変数名だけを取得する。
#-------------------------------------------------------------------------------
#'	(Internal) Get variable names of specified type.
#'
#'	\code{get.names} select variable names meeting a condition.
#'	\code{get.numeric.names} returns names of numeric variables.
#'	\code{get.facto.names} returns names of vactors.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@param fun
#'		a function like \code{\link[base]{is.numeric}} and
#'		\code{\link[base]{is.factor}}.
#'
#'	@return
#'		character vector containing variable names.
#'		\code{get.numeric.names} returns names of numeric variables.
#'		\code{get.facto.names} returns names of vactors.
#-------------------------------------------------------------------------------
get.names <- function(settings, fun) {
	result <- settings$x.names[sapply(settings$data[settings$x.names], fun)]
	return(result)
}


#-------------------------------------------------------------------------------
#	数値型の変数名を取得する。
#-------------------------------------------------------------------------------
#'	@describeIn get.names
#'
#'	Extract names of numeric variables from \code{var.name}
#-------------------------------------------------------------------------------
get.numeric.names <- function(settings) {
	return(get.names(settings, is.numeric))
}


#-------------------------------------------------------------------------------
#	因子型の変数名を取得する。
#-------------------------------------------------------------------------------
#'	@describeIn get.names
#'
#'	Extract names of factors from \code{var.name}
#-------------------------------------------------------------------------------
get.factor.names <- function(settings) {
	return(get.names(settings, is.factor))
}


#-------------------------------------------------------------------------------
#	データフレームの全ての列を結合して、文字列ベクトルを作る。
#-------------------------------------------------------------------------------
#'	(Internal) Combine columns into a character vector
#'
#'	This function combines all columns in a data.frame to make a character
#'	vector. This function is not intended to be users.
#'
#'	@param data a data.frame.
#'	@param sep a character representing separator of grouping factor levels.
#'
#'	@return a character vector.
#-------------------------------------------------------------------------------
combine.columns <- function(data, sep) {
	data <- as.data.frame(sapply(data, as.character, simplify = FALSE))
	result <- apply(data, 1, paste, collapse = sep)
	return(result)
}


#-------------------------------------------------------------------------------
#	因子型の変数の一意な値を取得する。
#-------------------------------------------------------------------------------
#'	(Internal) Extract unique factors in a data.frame
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return a list having unique values of factors.
#-------------------------------------------------------------------------------
get.unique.factors <- function(settings) {
	factor.names <- get.factor.names(settings)
	result <- list()
	for (name in factor.names) {
		result[[name]] <- unique(settings$data[[name]])
	}
	return(result)
}


#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
#	PL      = i + PW + SL + SP + PW:SP + SL:SP + r		# FULL MODEL
#	PL      = i + PW      + SP + PW:SP         + r		# Want to see this
#	PL      =        + SL              + SL:SP		    # Remove these effects
#	predict = i + PW + SL + SP + PW:SP + SL:SP
#
#	Case of LSMEANS
#	LSMEANS = i + PW + MM + SP + PW:SP + MM:SP
#	LSMEANS = i + PW + MM + SP + PW:SP + MM:SP + r		# Want to see this
#-------------------------------------------------------------------------------
partial.residual <- function(settings) {
	# Prepare names of numeric variables.
	# 数値型変数の変数名を用意。
	focal.numerics <- get.numeric.names(settings)
	all.numerics <- settings$adapter$x.names(type = "base")
	all.numerics <- all.numerics[
		sapply(settings$data[all.numerics], is.numeric)
	]
	other.numerics <- all.numerics[!all.numerics %in% focal.numerics]
	# Calculate prediction 1.
	# 予測値１を計算。
	data1 <- settings$data
	for (i in other.numerics) {
		data1[[i]] <- data1[[i]] - mean(data1[[i]])
	}
	data1[focal.numerics] <- 0
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


#-------------------------------------------------------------------------------
#	数値型の説明変数に対して範囲を均等に区切ったベクトルを作る。
#-------------------------------------------------------------------------------
#'	(Internal) Make sequence of numeric variables.
#'
#'	This function creates sequence of numeric variables for each specified
#'	variable name. Ranges of the variables is obtained data.frame specified
#'	in \code{data}.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return
#'		a named list containing sequence of variables.
#-------------------------------------------------------------------------------
numeric.sequences <- function(settings) {
	numeric.names <- get.numeric.names(settings)
	result <- list()
	for (name in numeric.names) {
		result[[name]] <- seq(
			min(settings$data[[name]], na.rm = TRUE),
			max(settings$data[[name]], na.rm = TRUE),
			length.out = settings$resolution
		)
	}
	return(result)
}


#-------------------------------------------------------------------------------
#	新しいプロットを開く。
#-------------------------------------------------------------------------------
#'	(Internal) Open new plot.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'	@param partial.residual.data
#'		a named list of data.frame containing result of
#'		\code{\link{partial.relationship.lsmeans}} function.
#-------------------------------------------------------------------------------
open.new.plot <- function(settings, partial.residual.data) {
	x.name <- get.numeric.names(settings)
	xlab <- set.xlab.2d(settings)
	ylab <- set.ylab.2d(settings)
	if (is.null(partial.residual.data$upper)) {
		x <- partial.residual.data[[x.name]]
		y <- partial.residual.data$fit
	} else {
		x <- rep(partial.residual.data[[x.name]], 2)
		y <- c(partial.residual.data$upper, partial.residual.data$lower)
	}
	args <- list(x, y, type = "n", xlab = xlab, ylab = ylab)
	args <- c(args, settings$other.pars)
	do.call(plot, args)
}


#-------------------------------------------------------------------------------
#	X軸ラベルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Set X label if original is NULL.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return x label.
#-------------------------------------------------------------------------------
set.xlab.2d <- function(settings) {
	if (length(settings$xlab) == 0) {
		return(get.numeric.names(settings))
	}
	return(settings$xlab)
}


#-------------------------------------------------------------------------------
#	Y軸ラベルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Set Y label if original is NULL.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'
#'	@return y label.
#-------------------------------------------------------------------------------
set.ylab.2d <- function(settings) {
	if (length(settings$ylab) == 0) {
		return(settings$adapter$y.names())
	}
	return(settings$ylab)
}


#-------------------------------------------------------------------------------
#	グループごとの色ベクトルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Make color vector for groups.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'	@param unique.pal
#'		if TRUE, this returns color palette. If FALSE, this returns vector of
#'		colors.
#'
#'	@return
#'		A character vector same as \code{\link{color.ramp}} function returns.
#'		If x.names doesn't have factors, this returns color palette with length
#'		1 which named as "all".
#-------------------------------------------------------------------------------
set.group.color <- function(settings, unique.pal) {
	factors <- get.factor.names(settings)
	if (!length(factors) == 0) {
		result <- color.palette <- color.ramp(
			settings$data, factors, pal = settings$col,
			sep = settings$sep, unique.pal = unique.pal
		)
	} else {
		result <- color.ramp(
			rep("all", nrow(settings$data)), pal = settings$col,
			unique.pal = unique.pal
		)
	}
	return(result)
}


#-------------------------------------------------------------------------------
#	偏残差の点を描画。
#-------------------------------------------------------------------------------
#'	(Internal) Draw partial residual graph.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#-------------------------------------------------------------------------------
draw.partial.residual <- function(settings) {
	# Prepare graphic parameters.
	# グラフィックパラメーターを用意。
	col <- set.group.color(settings, FALSE)
	xlab <- set.xlab.2d(settings)
	ylab <- set.ylab.2d(settings)
	# Calculate and draw partial residual
	# 偏残差を計算して描画。
	part.resid <- partial.residual(settings)
	numerics <- get.numeric.names(settings)
	if (!settings$draw.relationships) {
		args <- list(
			settings$data[[numerics]], part.resid, col = col,
			xlab = xlab, ylab = ylab
		)
		args <- c(args, settings$other.pars)
		do.call(plot, args)
	} else {
		# To handle graphic parameters in ..., use do.call,
		# not directly call points function.
		# ...に入ったグラフィックパラメーターを制御するため、
		# points関数を直接呼ばずにdo.callを使う。
		args <- list(x = settings$data[[numerics]], y = part.resid, col = col)
		points.pars <- c("pch", "bg", "cex")
		do.call(points, c(args, settings$other.pars[points.pars]))
	}
}


#-------------------------------------------------------------------------------
#	偏残差・偏回帰プロットを描画。
#-------------------------------------------------------------------------------
#'	Partial relationship graph.
#'
#'	Draw partial relationship graph between focal explanatory variables and
#'	response variable with controlling effects of other explanatory variables.
#'
#'	@param model
#'		a model object by which relationship between focal explanatory variables
#'		and response variables is determined. Basically, statistical models and
#'		machine learning models supported by
#'		\code{\link[model.adapter]{model.adapter}} class are supported.
#'
#'	@param x.names
#'		a character vector specifying name of focal explanatory variables.
#'		Combination of one or two numeric factors and any number of
#'		factors is supported. Currently, at least one numeric variable should
#'		be specified. This problem may be fixed in future.
#'
#'	@param data
#'		a data.frame used for prediction. If not specified, this function
#'		try to obtain original data used for the modeling from the object
#'		specified by the \code{model} argument.
#'
#'	@param draw.residuals
#'		a logical. If TRUE, points representing partial residual are drawn.
#'
#'	@param draw.relationships
#'		a logical. If TRUE, partial relationships between focal explanatory
#'		variables and response variable are drawn.
#'
#'	@param resolution
#'		an integer specifying resolution of lines, polygons, wireframes,
#'		and images of numeric variables.
#'		Larger number indicate higher resolution.
#'
#'	@param col
#'		a function or named vector representing color of the graph object.
#'		For the detail, see pal option of \code{\link{color.ramp}} function.
#'
#'	@param xlab
#'		label of x axis.
#'
#'	@param ylab
#'		label of y axis.
#'
#'	@param sep
#'		a character used for separator of factor levels.
#'
#'	@param ...
#'		other graphic parameters passed to poltting functions.
#'		Currently, \code{pch}, \code{bg} and \code{cex} are passed to
#'		\code{points} function, \code{lty}, \code{lwd}, \code{lend},
#'		\code{ljoin} and \code{lmitre} are passed to \code{lines} function,
#'		and others are passed to \code{plot} function.
#'
#'	@return
#'		An object of \code{\link{pp.legend}} containing informations which
#'		will be used for drawing legend.
#'
#'	@export
#'
#'	@details
#'		For models supported by \code{\link[lsmeans]{lsmeans}}, this function
#'		calculate partial dependence using \code{lsmeans} and adjusted partial
#'		residual to match result of \code{lsmeans}.
#'		For models having complicated interactions such as machine learning
#'		models, partial dependence is calculated by similar way as
#'		\code{\link[randomForest]{partialPlot}} function.
#'
#'	@examples
#'	#---------------------------------------------------------------------------
#'	# Example 1: normal partial.plot with partial relationship graph with
#'	# points representing partial residuals.
#'	#---------------------------------------------------------------------------
#'	data(iris)
#'	model <- lm(
#'		Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
#'	)
#'	partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
#'	partial.plot(model, c("Petal.Width", "Species"), pch = 16)
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 2: no residuals.
#'	#---------------------------------------------------------------------------
#'	partial.plot(
#'		model, c("Sepal.Length", "Species"), pch = 16, draw.residuals = FALSE
#'	)
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 3: no partial relationship.
#'	#---------------------------------------------------------------------------
#'	partial.plot(
#'		model, c("Sepal.Length", "Species"), pch = 16,
#'		draw.relationships = FALSE
#'	)
#-------------------------------------------------------------------------------
partial.plot <- function(
	model, x.names, data = NULL,
	draw.residuals = TRUE, draw.relationships = TRUE, resolution = 100L,
	col = gg.colors, xlab = character(), ylab = character(), sep = " - ", ...
) {
	# Initialize setting object.
	# 設定オブジェクトの初期化。
	settings <- pp.settings(
		model, x.names, data, draw.residuals, draw.relationships, resolution,
		col, xlab, ylab, sep, ...
	)
	# Draw partial relationship graph.
	# 関係式グラフの描画。
	if (draw.relationships) {
		draw.partial.relationship(settings)
	}
	# Plot partial residuals.
	# 偏残差の描画。
	if (draw.residuals) {
		draw.partial.residual(settings)
	}
	invisible(settings)
}

