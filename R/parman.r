#------------------------------------------------------------------------------
#	グラフィックパラメーター制御するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to handle graphic pars used for plotting.
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
	initialize = function(group, col, lty, pch, ...) {
		"
		Initialize class and set group field.
		"
		# Initialize fields.
		if (missing(group)) {
			return()
		}
		initFields(group = group, col = col, lty = lty, pch = pch)
	}
)


#------------------------------------------------------------------------------
#	各グループ用の色パレットを作成する。
#------------------------------------------------------------------------------
par.manager$methods(
	par.group = function(group.name = NULL) {
		"
		Make a list of graphic parameters for each group.
		"
		pars <- list()
		for (i in c("col", "lty", "pch")) {
			pars[[i]] <- switch.par(
				.self$group, pal = .self[[i]], unique.pal = TRUE
			)
		}
		if (!is.null(group.name)) {
			pars <- lapply(pars, function(x) x[group.name])
		}
		return(pars)
	}
)


#------------------------------------------------------------------------------
#	各観測値ごとのグラフィックパラメーターを作成する。
#------------------------------------------------------------------------------
par.manager$methods(
	par.obs = function() {
		"
		Make a list of graphic parameters for each observation.
		"
		pars <- list()
		for (i in c("col", "lty", "pch")) {
			pars[[i]] <- switch.par(.self$group, pal = .self[[i]])
		}
		return(pars)
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


#------------------------------------------------------------------------------
#	pp.legendに使われる引数を用意する。
#------------------------------------------------------------------------------
par.manager$methods(
	legend.pars = function() {
		"
		Prepare pars for \\code{\\link{pp.legend}} function.
		"
		pars <- .self$par.group()
		pars$legend = names(pars$col)
		return(pars)
	}
)
