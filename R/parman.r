#------------------------------------------------------------------------------
#	グラフィックパラメーター制御するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to handle graphic pars used for plotting.
#'
#'	@field settings
#'		a \code{\link{pp.settings}} object to keep settings of the function.
#'
#'	@field group
#'		a character vector of group names made from factors specified by
#'		x.names.
#'
#'	@field col
#'		a function or named character vector representing color of the graph
#'		object.
#'		For the detail, see pal option of \code{\link{switch.par}} function.
#'
#'	@field lty
#'		a vector of the line types or a function generate line types.
#'		For the detail, see \code{\link{switch.par}}.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
par.manager <- setRefClass(
	"par.manager",
	fields = list(
		group = "character",
		col = "ANY",
		lty = "ANY",
		pch = "ANY"
	)
)


#------------------------------------------------------------------------------
#	グラフィックパラメーター管理クラスの初期化。
#------------------------------------------------------------------------------
par.manager$methods(
	initialize = function(settings, col, lty, pch, ...) {
		"
		Initialize class and set group field.
		"
		# Initialize fields.
		if (missing(settings)) {
			return()
		}
		initFields(col = col, lty = lty, pch = pch)
		# Make group depending on number of factor variables.
		if (!length(settings$x.names.factor) == 0) {
			.self$group <- combine.columns(
				settings$data[settings$x.names.factor], settings$sep
			)
		} else {
			.self$group <- rep("all", nrow(settings$data))
		}
	}
)


#------------------------------------------------------------------------------
#	各グループ用の色パレットを作成する。
#------------------------------------------------------------------------------
par.manager$methods(
	colors.for.groups = function() {
		"
		Make color vector for each group.
		"
		result <- color.ramp(group, pal = .self$col, unique.pal = TRUE)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	各観測値ごとの色を作成する。
#------------------------------------------------------------------------------
par.manager$methods(
	colors.for.observations = function() {
		"
		Make color vector for each observation.
		"
		result <- color.ramp(group, pal = .self$col, unique.pal = FALSE)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	perspに使われる色を作成する。
#------------------------------------------------------------------------------
par.manager$methods(
	colors.for.persp = function(z.matrix) {
		"
		Make color vector used for \\code{\\link[graphics]{persp}} funciton.
		\\describe{
			\\item{\\code{z.matrix}}{
				a matrix used for \\code{z} option of \\code{persp}.
			}
		}
		"
		m <- matrix(nrow = nrow(z.matrix) - 1, ncol = ncol(z.matrix) - 1)
		for (row in 1:(nrow(z.matrix) - 1)) {
			for (column in 1:(nrow(z.matrix) - 1)) {
				m[row, column] <- mean(
					z.matrix[row:(row + 1), column:(column + 1)]
				)
			}
		}
		color <- color.ramp(m, .self$col)
		return(color)
	}
)

