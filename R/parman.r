#------------------------------------------------------------------------------
#	プロットに使う色を制御するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to handle colors used for plotting.
#'
#'	@field settings
#'		a \code{\link{pp.settings}} object to keep settings of the function.
#'
#'	@field group
#'		a character vector of group names made from factors specified by
#'		x.names.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
pp.colors <- setRefClass(
	"pp.colors",
	fields = list(settings = "pp.settings", group = "character")
)


#------------------------------------------------------------------------------
#	色管理クラスの初期化。
#------------------------------------------------------------------------------
pp.colors$methods(
	initialize = function(settings) {
		"
		Initialize class and set group field.
		"
		# Initialize fields.
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
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
pp.colors$methods(
	colors.for.groups = function() {
		"
		Make color vector for each group.
		"
		result <- color.ramp(group, pal = settings$col, unique.pal = TRUE)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	各観測値ごとの色を作成する。
#------------------------------------------------------------------------------
pp.colors$methods(
	colors.for.observations = function() {
		"
		Make color vector for each observation.
		"
		result <- color.ramp(group, pal = settings$col, unique.pal = FALSE)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	perspに使われる色を作成する。
#------------------------------------------------------------------------------
pp.colors$methods(
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
			for (col in 1:(nrow(z.matrix) - 1)) {
				m[row, col] <- mean(z.matrix[row:(row + 1), col:(col + 1)])
			}
		}
		color <- color.ramp(m, settings$col)
		return(color)
	}
)

