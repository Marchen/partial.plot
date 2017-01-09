#------------------------------------------------------------------------------
#	偏残差を計算、描画するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class calculate and draw partial residuals.
#'
#'	@field settings
#'		pp.settings object having settings of the class.
#'
#'	@field data
#'		a numeric vector having calculated partial residual data.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
partial.residual <- setRefClass(
	"partial.residual",
	fields = list(settings = "pp.settings", data = "numeric")
)


#------------------------------------------------------------------------------
#	偏残差計算クラスを初期化する。
#------------------------------------------------------------------------------
partial.residual$methods(
	initialize = function(settings) {
		"
		Initialize class and calculate residuals.

		\\describe{
			\\item{\\code{settings}}{\\code{\\link{pp.settings}} object.}
		}
		"
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
		.self$calculate.residuals()
		settings$set.residual(.self)
	}
)


#------------------------------------------------------------------------------
#	偏残差を計算する。
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
partial.residual$methods(
	calculate.residuals = function() {
		"
		Calculate partial residuals.
		"
		# Prepare names of numeric variables.
		# 数値型変数の変数名を用意。
		all.numerics <- settings$adapter$x.names(type = "base")
		all.numerics <- all.numerics[
			sapply(settings$data[all.numerics], is.numeric)
		]
		other.numerics <- all.numerics[
			!all.numerics %in% settings$x.names.numeric
		]
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
		.self$data <- (
			settings$adapter$link(settings$data[[settings$adapter$y.names()]])
			- (pred1 - pred2)
		)
	}
)


#------------------------------------------------------------------------------
#	偏残差の点を描画。
#------------------------------------------------------------------------------
partial.residual$methods(
	draw = function() {
		"
		Draw partial residuals.
		"
		if (settings$plot.type == "2D") {
			.self$draw.2d()
		} else {
			.self$draw.3d()
		}
	}
)


#------------------------------------------------------------------------------
#	二次元プロットで偏残差の点を描画。
#------------------------------------------------------------------------------
partial.residual$methods(
	draw.2d = function() {
		"
		Draw 2D residuals.
		"
		args <- list(
			x = settings$data[[settings$x.names.numeric]],
			y = .self$data, col = settings$obs.colors
		)
		args <- settings$set.function.args(args, points)
		do.call(points, args)
	}
)


#------------------------------------------------------------------------------
#	三次元プロットで偏残差の点を描画。
#------------------------------------------------------------------------------
partial.residual$methods(
	draw.3d = function() {
		"
		Draw 3D residuals.
		"
		if (identical(settings$fun.3d, image)) {
			points(
				settings$data[[settings$x.names.numeric[1]]],
				settings$data[[settings$x.names.numeric[2]]],
				pch = 16, col = "white"
			)
		}
	}
)


