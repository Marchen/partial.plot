#------------------------------------------------------------------------------
#	偏依存関係描画クラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to draw partial relationship graph.
#'
#'	@field settings
#'		a \code{\link{pp.settings}} object to keep settings of the function.
#'
#'	@field relationship
#'		an object of \code{\link{partial.relationship}} class.
#'
#'	@field residual
#'		an object of \code{\link{partial.residual}} class.
#'
#'	@include pp.settings.r
#'	@include partial.relationship.r
#'	@include partial.residual.r
#------------------------------------------------------------------------------
pp.drawer <- setRefClass(
	"pp.drawer",
	fields = list(
		settings = "pp.settings",
		relationship = "ANY",
		residual = "ANY"
	)
)


#------------------------------------------------------------------------------
#	偏依存関係描画クラスを初期化する。
#------------------------------------------------------------------------------
pp.drawer$methods(
	initialize = function(settings, relationship = NULL, residual = NULL) {
		"
		Initialize object.
		"
		if (missing(settings)) {
			return()
		}
		# Check correctness of relationship and residual.
		if (!is.null(relationship)) {
			if (!is(relationship, partial.relationship)) {
				stop("'relationship' should be object of partial.relationship.")
			}
		}
		if (!is.null(residual)) {
			if (!is(residual, partial.residual)) {
				stop("'residual' should be object of partial.residual.")
			}
		}
		initFields(
			settings = settings, relationship = relationship,
			residual = residual
		)
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
		if (!is.null(relationship)) {
			if (is.null(relationship$data$upper)) {
				x <- relationship$data[[settings$x.names.numeric]]
				y <- relationship$fit
			} else {
				x <- rep(relationship$data[[settings$x.names.numeric]], 2)
				y <- c(relationship$data$upper, relationship$data$lower)
			}
		} else {
			x <- y <- numeric()
		}
		if (!is.null(residual)) {
			x <- c(x, settings$data[[settings$x.names.numeric]])
			y <- c(y, residual$data)
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
		if (settings$draw.interval) {
			relationship$draw.interval()
		}
		if (settings$draw.relationship) {
			relationship$draw.relationship()
		}
		if (settings$draw.residual) {
			residual$draw()
		}
	}
)

