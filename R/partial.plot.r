#------------------------------------------------------------------------------
#	データフレームの全ての列を結合して、文字列ベクトルを作る。
#------------------------------------------------------------------------------
#'	(Internal) Combine columns into a character vector
#'
#'	This function combines all columns in a data.frame to make a character
#'	vector. This function is not intended to be users.
#'
#'	@param data a data.frame.
#'	@param sep a character representing separator of grouping factor levels.
#'
#'	@return a character vector.
#------------------------------------------------------------------------------
combine.columns <- function(data, sep) {
	data <- as.data.frame(sapply(data, as.character, simplify = FALSE))
	result <- apply(data, 1, paste, collapse = sep)
	return(result)
}


#------------------------------------------------------------------------------
#	偏残差・偏回帰プロットを描画。
#------------------------------------------------------------------------------
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
#'	@param type
#'		a character literal indicating type of scale of plotting.
#'		This is similar to type argument of many predict methods.
#'		Possible values are "response", "link".
#'		If "link" is specified, partial relationship and residuals are
#'		drawn in the scale of the linear predictor.
#'		On the other hand, partial relationship and residuals are drawn
#'		in the scale of the response variable if "response" is specified.
#'		"prob" may be implimented in the future.
#'
#'	@param fun.3d
#'		a function to produce 3d graph. Currently
#'		\code{\link[graphics]{persp}}, \code{\link[grpahics]{image}},
#'		\code{\link[graphics]{contour}} and \code{\link[rgl]{persp3d}}
#'		are supported.
#'
#'	@param draw.residual
#'		a logical. If TRUE, points representing partial residual are drawn.
#'
#'	@param draw.relationship
#'		a logical. If TRUE, partial relationships between focal explanatory
#'		variables and response variable are drawn.
#'
#'	@param draw.interval
#'		a logical. If TRUE, interval of partial relationships are drawn.
#'		Note that currently this intervals are confidential intervals if
#'		\code{model} is supported by lsmeans and  are quantile of perdicted
#'		values for the \code{model} not supported by lsmeans.
#'
#'	@param interval.levels
#'		values between 0 <= levels <= 1 indicating level(s) of interval.
#'
#'		For the models supported by \code{\link[lsmeans]{lsmeans}},
#'		this can be a value indicating confidence interval. Only the first
#'		element of the numeric vector is used for confidence intervals.
#'
#'		For the models not supported by lsmeans, this can be a value of
#'		lower and upper quantile, e.g., \code{values = 0.95} indicates
#'		lower quantile is 0.05 and uppter quantile is 0.95.
#'		A numeric vector of length two specifying
#'		both lower and upper quantiles also can be specified.
#'
#'	@param resolution
#'		an integer specifying resolution of lines, polygons, wireframes,
#'		and images of numeric variables.
#'		Default value is 100 for 2D plots and 10 for 3D plots.
#'		Larger number indicate higher resolution. Note that this parameter
#'		can affect computation time so that higher resolution require
#'		more times to draw the graph.
#'
#'	@param col
#'		a function or named vector representing color of the graph object.
#'		For the detail, see pal option of \code{\link{color.ramp}} function.
#'
#'	@param xlab
#'		label of X axis.
#'
#'	@param ylab
#'		label of Y axis.
#'
#'	@param zlab
#'		label of Z axis.
#'
#'	@param sep
#'		a character used for separator of factor levels.
#'
#'	@param n.cores
#'		an integer representing number of processes used for calculation.
#'		If NULL is specified, maximum number of logical processors are used.
#'		This value is ignored when the models compatible with lsmeans are
#'		specified.
#'
#'	@param ...
#'		other graphic parameters passed to poltting functions.
#'		Possible parameters for each element of the graph is:
#'
#'		\describe{
#'			\item{Partial relatioship lines}{
#'				\code{lty}, \code{lwd}, \code{lend},\code{ljoin},
#'				\code{lmitre}, \code{col}, parameters supported by
#'				\code{\link[graphics]{lines.default}}.
#'			}
#'			\item{Interval of partial relationship}{
#'				none.
#'			}
#'			\item{Partial residual points}{
#'				\code{pch}, \code{bg}, \code{cex}, \code{col}, parameters
#'				supported by \code{\link[graphics]{points.default}}.
#'				In the case not drawing interval of partial relationship,
#'				all parameters are used.
#'			}
#'			\item{3D partial relationship by \code{\link[graphics]{persp}}}{
#'				\code{cex.lab}, \code{font.lab}, \code{cex.axis},
#'				\code{font.axis}, parameters supported by
#'				\code{\link[graphics]{persp.default}}.
#'			}
#'			\item{3D partial relationship by \code{\link[graphics]{image}}}{
#'				\code{asp}, \code{axes}, \code{bg}, paramters supported by
#'				\code{link[graphics]{image.default}},
#'			}
#'			\item{3D partial relationship by \code{\link[grpahics]{contour}}}{
#'				\code{xaxs}, \code{yaxs}, \code{lab}, \code{col.main},
#'				\code{cex.sub}, \code{xpd}, \code{mgp}, \code{cex.axis},
#'				\code{col.axis}, \code{font.axis}, \code{xaxp}, \code{yaxp},
#'				\code{tck}, \code{tcl}, \code{las}, \code{fg}, \code{xaxt},
#'				\code{yaxt}, \code{bty}, parameters supported by
#'				\code{\link[graphics]{contour.default}},
#'				\code{\link[graphics]{plot.window}},
#'				\code{\link[graphics]{title}}, \code{\link[graphics]{Axis}},
#'				\code{\link[graphics]{axis}} and \code{\link[graphics]{box}}
#'				functions.
#'			}
#'			\item{3D partial relationship by \code{\link[rgl]{persp3d}}}{
#'				\code{col}, parameters supported by
#'				\code{\link[rgl]{persp3d.default}},
#'				\code{\link[rgl]{surface3d}} and
#'				\code{\link[rgl]{rgl.material}} functions.
#'			}
#'		}
#'
#'	@return
#'		An object of \code{\link{pp.legend}} containing informations which
#'		will be used for drawing legend.
#'
#'	@details
#'		For models supported by \code{\link[lsmeans]{lsmeans}}, this function
#'		calculate partial dependence using \code{lsmeans} and adjusted partial
#'		residual to match result of \code{lsmeans}.
#'		For models having complicated interactions such as machine learning
#'		models, partial dependence is calculated by similar way as
#'		\code{\link[randomForest]{partialPlot}} function in randomForest
#'		package.
#'
#'		For the detailed explanation, see \code{vignette("partial.plot")}
#'		(English) or \code{vignette("partial.plot.j")} (Japanese)
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
#'		model, c("Sepal.Length", "Species"), pch = 16, draw.residual = FALSE
#'	)
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 3: no partial relationship.
#'	#---------------------------------------------------------------------------
#'	partial.plot(
#'		model, c("Sepal.Length", "Species"), pch = 16,
#'		draw.relationship = FALSE
#'	)
#'
#'	@export
#------------------------------------------------------------------------------
partial.plot <- function(
	model, x.names, data = NULL, type = "response", fun.3d = persp,
	draw.residual = TRUE, draw.relationship = TRUE, draw.interval = TRUE,
	interval.levels = 0.95, resolution = NULL, col = gg.colors,
	xlab = NULL, ylab = NULL, zlab = NULL, sep = " - ", n.cores = NULL, ...
) {
	# Initialize setting object.
	# 設定オブジェクトの初期化。
	if (is(model, "pp.settings")) {
		settings <- model$copy()
		settings$update.pars(
			fun.3d, draw.residual, draw.relationship, draw.interval,
			col, xlab, ylab, zlab, ...
		)
	} else {
		settings <- pp.settings(
			model, x.names, data, type, fun.3d, draw.residual,
			draw.relationship, draw.interval, interval.levels, resolution, col,
			xlab, ylab, zlab, sep, n.cores, ...
		)
		# Calculate required data.
		partial.relationship(settings)
		partial.residual(settings)
	}
	# Draw
	drawer <- pp.drawer(settings)
	drawer$draw()
	invisible(settings)
}


