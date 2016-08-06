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
#'	@param data
#		a data.frame in which types of variables in var.names is evaluated.
#'
#'	@param var.names
#'		a character vector containing variable names to check their type.
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
get.names <- function(data, var.names, fun) {
	if (is(data, "model.adapter")) {
		data <- data$data
	}
	result <- var.names[sapply(data[var.names], fun)]
	return(result)
}


#-------------------------------------------------------------------------------
#	数値型の変数名を取得する。
#-------------------------------------------------------------------------------
#'	@describeIn get.names
#'
#'	Extract names of numeric variables from \code{var.name}
#-------------------------------------------------------------------------------
get.numeric.names <- function(data, var.names) {
	return(get.names(data, var.names, is.numeric))
}


#-------------------------------------------------------------------------------
#	因子型の変数名を取得する。
#-------------------------------------------------------------------------------
#'	@describeIn get.names
#'
#'	Extract names of factors from \code{var.name}
#-------------------------------------------------------------------------------
get.factor.names <- function(data, var.names) {
	return(get.names(data, var.names, is.factor))
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
#'	@param sep a character literal used for separateor of factor group names.
#'
#'	@return a character vector.
#-------------------------------------------------------------------------------
combine.columns <- function(data, sep = ".") {
	data <- as.data.frame(sapply(data, as.character, simplify = FALSE))
	result <- apply(data, 1, paste, collapse = sep)
	return(result)
}


#-------------------------------------------------------------------------------
#	因子の一意な組み合わせを作る。
#-------------------------------------------------------------------------------
#'	(Internal) Extract unique factors in a data.frame
#'
#'	@param data a data.frame containing factors.
#'	@param factor.names 
#'		a character vector of factor names to extract unique values of them.
#'		Only factor names are used and names of non-factor variables 
#'		are ignored.
#'
#'	@return a list having unique values of factors.
#'	@export
#-------------------------------------------------------------------------------
get.unique.factors <- function(data, factor.names) {
	factor.names <- get.factor.names(data, factor.names)
	result <- list()
	for (name in factor.names) {
		result[[name]] <- unique(data[[name]])
	}
	return(result)
}


#-------------------------------------------------------------------------------
#	パラメーターの整合性を確認する。
#-------------------------------------------------------------------------------
#'	(Internal) Check consistensy of parameters.
#'
#'	@param adapter 
#'		a \code{\link[model.adapter]{model.adapter}} object with 
#'		model and data.
#'
#'	@param x.names
#'		a character vector of names of explanatory variables.
#'
#'	@return NULL
#-------------------------------------------------------------------------------
check.params <- function(adapter, x.names) {
	# check availability of data.
	# dataが使えるかを確認。
	if (is.null(adapter$data)) {
		stop("'model' object does not have original data. Please specify 'data' argument.")
	}
	# check x.names.
	# x.namesのチェック。
	if (!all(x.names %in% colnames(adapter$data))) {
		error <- x.names[!x.names %in% colnames(adapter$data)]
		stop(sprintf("\n Column '%s' is not found in data.", error))
	}
	if (!all(x.names %in% adapter$x.names(type = "base"))) {
		error <- x.names[!x.names %in% adapter$x.names(type = "base")]
		stop(sprintf("\n '%s' is not found in explanatory variables.", error))
	}
	# Check number of continuous explanatory variables
	# 連続値の説明変数の数チェック
	var.types <- sapply(adapter$data[, x.names, drop = FALSE], class)
	n.continuous <- sum(var.types == "numeric")
	if (n.continuous > 2) {
		stop("Plotting more than two continuous explanatory variables is not supported.")
	}
}


#-------------------------------------------------------------------------------
#'	(Internal) Calculate partial residual.
#'
#'	This function calculates partial residual for specfied model.
#'	Resultant values are adjusted to be congruent with result of 
#'	\code{\link[lsmeans]{lsmeans}}.
#'
#'	@param adapter
#'		a \code{\link[model.adapter]{model.adapter}} object with information of
#'		the model.
#'
#'	@param x.names
#'		name of focal explanatory variable.
#'
#'	@param link
#'		link function to convert values of response variable.
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
#	
#-------------------------------------------------------------------------------
partial.residual <- function(adapter, x.names, link) {
	# Prepare names of numeric variables.
	# 数値型変数の変数名を用意。
	focal.numerics <- get.numeric.names(adapter$data, x.names)
	all.numerics <- get.numeric.names(
		adapter$data, adapter$x.names(type = "base")
	)
	other.numerics <- all.numerics[!all.numerics %in% focal.numerics]
	# Calculate prediction 1.
	# 予測値１を計算。
	data1 <- adapter$data
	for (i in other.numerics) {
		data1[[i]] <- data1[[i]] - mean(data1[[i]])
	}
	data1[focal.numerics] <- 0
	pred1 <- adapter$predict(newdata = data1, type = "link")$fit[, "fit"]
	# Calculate prediction 2.
	# 予測値２を計算。
	data2 <- adapter$data
	data2[all.numerics] <- 0
	pred2 <- adapter$predict(newdata = data2, type = "link")$fit[, "fit"]
	# Calculate partial residual.
	# 偏残差を計算。
	resid <- link(adapter$data[[adapter$y.names()]]) - (pred1 - pred2)
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
#'	@param data a data.frame with variables.
#'
#'	@param x.names
#'		a character vector of names of variable for which sequences are made.
#'		Note only names of numeric variables are used and names of other type
#'		of variables are ignored.
#'
#'	@param resolution
#'		a integer specifying length of sequence.
#'
#'	@return
#'		a named list containing sequence of variables.
#-------------------------------------------------------------------------------
numeric.sequences <- function(data, x.names, resolution = 100) {
	numeric.names <- get.numeric.names(data, x.names)
	result <- list()
	for (name in numeric.names) {
		result[[name]] <- seq(
			min(data[[name]], na.rm = TRUE), max(data[[name]], na.rm = TRUE),
			length.out = resolution
		)
	}
	return(result)
}


#-------------------------------------------------------------------------------
#	新しいプロットを開く。
#-------------------------------------------------------------------------------
#'	(Internal) Open new plot.
#'
#'	@param partial.residual.data 
#'		a named list of data.frame containing result of 
#'		\code{\link{partial.relationship.lsmeans}} function.
#'
#'	@param x.name
#'		name of focal continuous explanatory variable.
#'
#'	@param xlab
#'		label of x axis.
#'
#'	@param ylab
#'		label of y axis.
#'
#'	@param ... other parameters passed to plot function.
#-------------------------------------------------------------------------------
open.new.plot <- function(partial.residual.data, x.name, xlab, ylab, ...) {
	if (is.null(partial.residual.data$upper)) {
		x <- partial.residual.data[[x.name]]
		y <- partial.residual.data$fit
	} else {
		x <- rep(partial.residual.data[[x.name]], 2)
		y <- c(partial.residual.data$upper, partial.residual.data$lower)
	}
	plot(x, y, type = "n", xlab = xlab, ylab = ylab, ...)
}


#-------------------------------------------------------------------------------
#	X軸ラベルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Set X label if original is NULL.
#'
#'	@param xlab original x label.
#'	@param adapter a model.adapter object.
#'	@param x.names a character vector of focal explanatory variables.
#'
#'	@return x label.
#-------------------------------------------------------------------------------
set.xlab.2d <- function(xlab, adapter, x.names) {
	if (is.null(xlab)) {
		xlab <- get.numeric.names(adapter, x.names)
	}
	return(xlab)
}


#-------------------------------------------------------------------------------
#	Y軸ラベルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Set Y label if original is NULL.
#'
#'	@param xlab original y label.
#'	@param adapter a model.adapter object.
#'
#'	@return y label.
#-------------------------------------------------------------------------------
set.ylab.2d <- function(ylab, adapter) {
	if (is.null(ylab)) {
		ylab <- adapter$y.names()
	}
	return(ylab)
}


#-------------------------------------------------------------------------------
#	グループごとの色ベクトルを設定する。
#-------------------------------------------------------------------------------
#'	(Internal) Make color vector for groups.
#'
#'	@param adapter a model.adapter object.
#'	@param x.names a character vector of focal explanatory variables.
#'	@param col a function or character vector.
#'	@param unique.pal
#'		if TRUE, this returns color palette. If FALSE, this returns vector of
#'		colors.
#'
#'	@return
#'		A character vector same as \code{\link{color.ramp}} function returns.
#'		If x.names doesn't have factors, this returns color palette with length
#'		1 which named as "all".
#-------------------------------------------------------------------------------
set.group.color <- function(adapter, x.names, col, unique.pal) {
	factors <- get.factor.names(adapter$data, x.names)
	if (!length(factors) == 0) {
		result <- color.palette <- color.ramp(
			adapter$data, factors, pal = col, unique.pal = unique.pal
		)
	} else {
		col <- col[1]
		names(col) <- NULL
		result <- color.ramp(
			rep("all", nrow(adapter$data)), pal = col, unique.pal = unique.pal
		)
	}
	return(result)
}


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
#'	@param model a model object.
#'
#'	@param x.names names of focal explanatory variables.
#'
#'	@param adapter 
#'		a model.adapter object containing information of model object.
#'
#'	@param resolution 
#'		an integer specifying number of steps for prediction.
#'
#'	@param col 
#'		a function or named character vector passed to \code{pal} option of
#'		\code{\link{color.ramp}} function.
#'
#'	@param xlab
#'		label of x axis.
#'
#'	@param ylab
#'		label of y axis.
#'
#'	@param partial.relationship.data
#'		a data.frames made by \code{\link{partial.relationship.lsmeans}}.
#'
#'	@param ... other parameters passed to poltting functions.
#-------------------------------------------------------------------------------
draw.partial.relationship <- function(
	model, x.names, adapter, resolution, col, xlab, ylab, ...
) {
	# データ作成
	pr.data <- partial.relationship.lsmeans(
		model, x.names, adapter$data, resolution
	)
	# Dispatch based on number of numeric variables.
	# 数値型の説明変数の数に応じて使う関数を変える。
	numeric.names <- get.numeric.names(adapter, x.names)
	if (length(numeric.names) == 2) {
		draw.partial.relationship.3d(
			adapter, x.names, pr.data, col, xlab, ylab, ...
		)
	} else {
		draw.partial.relationship.2d(
			adapter, x.names, pr.data, col, xlab, ylab, ...
		)
	}
}


#-------------------------------------------------------------------------------
#'	@describeIn draw.partial.relationship
#'	Draw 2D partial relationship graph.
#-------------------------------------------------------------------------------
draw.partial.relationship.2d <- function(
	adapter, x.names, partial.relationship.data, col, xlab, ylab, ...
) {
	# Prepare graphic parameters.
	# グラフィックパラメーターを用意。	
	color.palette <- set.group.color(adapter, x.names, col, TRUE)
	xlab <- set.xlab.2d(xlab, adapter, x.names)
	ylab <- set.ylab.2d(ylab, adapter)
	# Open new plot.
	# 新しいプロットを開く。
	numeric.names <- get.numeric.names(adapter, x.names)
	open.new.plot(partial.relationship.data, numeric.names, xlab, ylab, ...)
	# Split data.
	# データを分割。
	if (length(names(color.palette)) == 1) {
		partial.relationship.data <- list(all = partial.relationship.data)
	} else {
		factors <- get.factor.names(adapter, x.names)
		partial.relationship.data <- split(
			partial.relationship.data, partial.relationship.data[factors]
		)
	}
	# Draw polygons.
	# ポリゴンを描画
	for (i in names(color.palette)) {
		d <- partial.relationship.data[[i]]
		x <- c(d[[numeric.names]], rev(d[[numeric.names]]))
		y <- c(d$lower, rev(d$upper))
		polygon(x, y, border = NA, col = trans.color(color.palette[i]))
	}
	# Draw partial relationships.
	# To handle valid graphic paramters in ... for lines, use do.call.
	# 関係式を描画。...の中からlinesで使えるグラフィックパラメーターだけを
	# 使うため、do.callを呼ぶ。
	for (i in names(color.palette)) {
		d <- partial.relationship.data[[i]]
		args <- list(x = d[[numeric.names]], y = d$fit, col = color.palette[i])
		lines.par <- c("lty", "lwd", "lend", "ljoin", "lmitre")
		dots <- list(...)
		do.call(lines, c(args, dots[names(dots) %in% lines.par]))
	}
}


#-------------------------------------------------------------------------------
#'	@describeIn draw.partial.relationship
#'	Draw 3D partial relationship graph.
#-------------------------------------------------------------------------------
draw.partial.relationship.3d <- function(
	adapter, x.names, partial.relationship.data, col, xlab, ylab, ...
) {
	cat("3D plot is not implimented yet...\n")
}


#-------------------------------------------------------------------------------
#	偏残差の点を描画。
#-------------------------------------------------------------------------------
#'	(Internal) Draw partial residual graph.
#'
#'	@param adapter a model.adapter object containing required information.
#'
#'	@param x.names 
#'		a character vector having names of focal explanatory variables
#'
#'	@param draw.relationships
#'		a logical indicating partial relationship graph was already drawn.
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
#-------------------------------------------------------------------------------
draw.partial.residual <- function(
	adapter, x.names, draw.relationships, col, xlab, ylab, ...
) {
	# Prepare graphic parameters.
	# グラフィックパラメーターを用意。
	col <- set.group.color(adapter, x.names, col, FALSE)
	xlab <- set.xlab.2d(xlab, adapter, x.names)
	ylab <- set.ylab.2d(ylab, adapter)
	# Calculate and draw partial residual
	# 偏残差を計算して描画。
	part.resid <- partial.residual(adapter, x.names, adapter$link)
	numerics <- get.numeric.names(adapter$data, x.names)
	if (!draw.relationships) {
		plot(
			adapter$data[[numerics]], part.resid, col = col,
			xlab = xlab, ylab = ylab, ...
		)
	} else {
		# To handle graphic parameters in ..., use do.call, 
		# not directly call points function.
		# ...に入ったグラフィックパラメーターを制御するため、
		# points関数を直接呼ばずにdo.callを使う。
		args <- list(x = adapter$data[[numerics]], y = part.resid, col = col)
		points.pars <- c("pch", "bg", "cex")
		dots <- list(...)[points.pars]
		do.call(points, c(args, dots))
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
	draw.residuals = TRUE, draw.relationships = TRUE, resolution = 100,
	col = gg.colors, xlab = NULL, ylab = NULL, ...
) {
	# Check errors.
	# エラーチェック。
	adapter <- model.adapter(model, data = data)
	check.params(adapter, x.names)
	# Draw partial relationship graph.
	# 関係式グラフの描画。
	if (draw.relationships) {
		draw.partial.relationship(
			model, x.names, adapter, resolution, col, xlab, ylab, ...
		)
	}
	# Plot partial residuals.
	# 偏残差の描画。
	if (draw.residuals) {
		draw.partial.residual(
			adapter, x.names, draw.relationships, col, xlab, ylab, ...
		)
	}
	# Prepare information for legend.
	# レジェンド用の情報を準備。
	legend.info <- pp.info(
		col = set.group.color(adapter, x.names, col, TRUE),
		title = paste0(get.factor.names(adapter, x.names), collapse ="."),
		draw.residuals = draw.residuals, 
		draw.relationships = draw.relationships, ...
	)
	invisible(legend.info)
}

