#------------------------------------------------------------------------------
#	偏依存関係描画クラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to draw partial relationship graph.
#'
#'	@field settings
#'		a \code{\link{pp.settings}} object to keep settings of the function.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
pp.drawer <- setRefClass(
	"pp.drawer",
	fields = list(settings = "pp.settings")
)


#------------------------------------------------------------------------------
#	偏依存関係描画クラスを初期化する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	initialize = function(settings) {
		"
		Initialize object.
		"
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
	}
)


#------------------------------------------------------------------------------
#	新しいプロットを開く。
#------------------------------------------------------------------------------
pp.drawer$methods(
	open.plot.window = function() {
		"
		Open new plot window.
		"
		# Find possible x and y values.
		if (settings$has.relationship) {
			relationship <- settings$relationship
			if (is.null(relationship$upper)) {
				x <- relationship[[settings$x.names.numeric]]
				y <- relationship$fit
			} else {
				x <- rep(relationship[[settings$x.names.numeric]], 2)
				y <- c(relationship$upper, relationship$lower)
			}
		} else {
			x <- y <- numeric()
		}
		if (settings$has.residual) {
			x <- c(x, settings$data[[settings$x.names.numeric]])
			y <- c(y, settings$residual)
		}
		# Open plot window.
		args <- list(
			x, y, type = "n", xlab = settings$xlab, ylab = settings$ylab
		)
		args <- c(args, settings$other.pars)
		do.call(plot, args)
	}
)


#------------------------------------------------------------------------------
#	グラフィックを描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw = function() {
		"
		Draw graph.
		"
		if (settings$plot.type == "2D") {
			open.plot.window()
		}
		if (settings$draw.interval & settings$has.relationship) {
			.self$draw.interval()
		}
		if (settings$draw.relationship & settings$has.relationship) {
			.self$draw.relationship()
		}
		if (settings$draw.residual & settings$has.residual) {
			.self$draw.residual()
		}
	}
)

#------------------------------------------------------------------------------
#	偏残差の点を描画。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual = function() {
		"
		Draw partial residuals.
		"
		if (settings$plot.type == "2D") {
			.self$draw.residual.2d()
		} else {
			.self$draw.residual.3d()
		}
	}
)


#------------------------------------------------------------------------------
#	二次元プロットで偏残差の点を描画。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual.2d = function() {
		"
		Draw 2D residuals.
		"
		args <- list(
			x = settings$data[[settings$x.names.numeric]],
			y = settings$residual, col = settings$obs.colors
		)
		args <- settings$set.function.args(args, points)
		do.call(points, args)
	}
)


#------------------------------------------------------------------------------
#	三次元プロットで偏残差の点を描画。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.residual.3d = function() {
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


#------------------------------------------------------------------------------
#	説明変数と応答変数の関係式を描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship = function() {
		"
		Draw partial relationships.
		"
		if (settings$plot.type == "2D") {
			draw.relationship.2d()
		} else {
			draw.relationship.3d()
		}
	}
)


#------------------------------------------------------------------------------
#	説明変数と応答変数の関係式の信頼区間を描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval = function() {
		"
		Draw intervals of partial relationships.
		"
		if (settings$plot.type == "2D") {
			draw.interval.2d()
		} else {
			draw.interval.3d()
		}
	}
)

#------------------------------------------------------------------------------
#	二次元偏依存性の線を描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship.2d = function() {
		for (i in names(settings$group.colors)) {
			"
			Draw partial relationship lines in 2D graph.
			"
			current.data <- settings$relationship.split[[i]]
			args <- list(
				x = current.data[[settings$x.names.numeric]],
				y = current.data$fit, col = settings$group.colors[i]
			)
			args <- settings$set.function.args(args, lines)
			do.call(lines, args)
		}
	}
)


#------------------------------------------------------------------------------
#	二次元偏依存性の信頼（などの）区間を描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval.2d = function() {
		"
		Draw intervals of partial.relationship in 2D graph.
		"
		for (i in names(settings$group.colors)) {
			current.data <- settings$relationship.split[[i]]
			x <- current.data[[settings$x.names.numeric]]
			x <- c(x, rev(x))
			y <- c(current.data$lower, rev(current.data$upper))
			polygon(
				x, y, border = NA,
				col = trans.color(settings$group.colors[i])
			)
		}
	}
)


#------------------------------------------------------------------------------
#	三次元偏依存性グラフを描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.relationship.3d = function() {
		"
		Draw 3D partial relationship.
		"
		z.matrix <- matrix(
			settings$relationship$fit, nrow = settings$resolution
		)
		if (identical(settings$fun.3d, image)) {
			col <- color.ramp(z.matrix, settings$col, unique.pal = TRUE)
		} else if (identical(settings$fun.3d, persp)) {
			col <- pp.colors(settings)$colors.for.persp(z.matrix)
		} else {
			col <- color.ramp(z.matrix, settings$col)
		}
		args <- list(
			z = z.matrix,
			x = unique(settings$relationship[[settings$x.names[1]]]),
			y = unique(settings$relationship[[settings$x.names[2]]]),
			xlab = settings$xlab, ylab = settings$ylab, zlab = settings$zlab,
			col = col
		)
		args <- settings$set.function.args(args)
		do.call(settings$fun.3d, args)
	}
)


#------------------------------------------------------------------------------
#	三次元偏依存性グラフの信頼区間を描画する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	draw.interval.3d = function() {
		"
		Draw 3D interval of partial relationship (not implimented).
		"
		#####   DO NOTHING   #####
	}
)


