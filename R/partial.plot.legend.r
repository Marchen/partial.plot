#-------------------------------------------------------------------------------
#	partial.plotのレジェンドを描画するクラス。
#-------------------------------------------------------------------------------
#'	(Internal) A reference class to make legend of partial.plot
#'
#'	This reference class keeps information used for partial.plot and 
#'	makes appropriate legend. This class is not intended to be used by 
#'	users directly.
#'
#'	@field col
#'		a named character vector representing names and colors of legend.
#'	@field title
#'		a character representing title of the legend.
#'	@field draw.residuals
#'		a logical representing that partial.plot drew residual points or not.
#'	@field draw.relationships
#'		a logical representing that partial.plot drew predicted relationships
#'		or not.
#-------------------------------------------------------------------------------
pp.legend <- setRefClass(
	"pp.legend",
	fields = list(
		col = "character",
		title = "character",
		draw.residuals = "logical",
		draw.relationships = "logical",
		other.pars = "list"
	)
)

pp.legend$methods(
	initialize = function(col, title, draw.residuals, draw.relationships, ...) {
		"
		Initialize pp.legend object.
		\\describe{
			\\item{col}{color vector.}
			\\item{title}{legend title.}
			\\item{draw.residuals}{
				whether the partial.plot drewn residual points or not.
			}
			\\item{draw.relationships}{
				whether partial.plot drew predicted relationships or not.
			}
		}
		"
		col <<- col
		title <<- title
		draw.residuals <<- draw.residuals
		draw.relationships <<- draw.relationships
		other.pars <<- list(...)
	}
)


pp.legend$methods(
	build.legend.args = function(x, ...) {
		"
		Prepare arguments for \\code{\\link[graphic]{legend}} function.
		\\describe{
			\\item{x}{x argument of \\code{\\link[graphic]{legend}} function.}
			\\item{...}{
				other arguments passed to \\code{\\link[graphic]{legend}} 
				function.
			}
		}
		"
		# Prepare arguments for legend function.
		# legend関数の引数を用意。
		legend.args <- list(x = x, ...)
		replace.args <- list(col = col, title = title, legend = names(col))
		replace.args <- c(replace.args, other.pars)
		for (i in names(replace.args)) {
			if (is.null(legend.args[[i]])) {
				legend.args[[i]] <- replace.args[[i]]
			}
		}
		# Set line type based on the setting of partial.plot.
		# 線の種類をpartial.plotの設定に基づいて決定。
		if (draw.relationships) {
			if (is.null(legend.args$lty)) {
				legend.args$lty <- "solid"
			}
		} else {
			legend.args <- NULL
		}
		# Set plot character based on the setting of partial.plot.
		# 点のシンボルをpartial.plotの設定に基づいて決定。
		if (draw.residuals) {
			if (is.null(legend.args$pch)) {
				legend.args$pch = 1
			}
		} else {
			legend.args$pch <- NULL
		}
		return(legend.args)
	}
)


#-------------------------------------------------------------------------------
#	partial.plotの結果を用いてレジェンドを描画する。
#-------------------------------------------------------------------------------
#'	Draw legend of partial plot.
#'
#'	@param object
#'		\code{\link{pp.legend}} object resulted by\code{\link{partial.plot}}.
#'	@param x 
#'		position of the legend. For the detail, see 
#'		\code{\link[grahpic]{legend}} function.
#'		
#'	@param ...
#'		other graphic parameters passed to \code{\link[grahpic]{legend}} 
#'		function.
#'
#'	@export
#'
#'	@examples
#'
#'	data(iris)
#'	model <- lm(
#'		Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
#'	)
#'	info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
#'	partial.plot.legend(info, "topleft")
#-------------------------------------------------------------------------------
partial.plot.legend <- function(object, x, ...) {
	if (!is(object, "pp.legend")) {
		stop("'object' should be an instance of 'pp.legend' class")
	}
	# Prepare arguments for build.legend.args().
	# build.legend.args()の引数を用意。
	call <- match.call()
	call$object <- NULL
	call <- match.call(legend, call)
	call <- as.list(call)
	call[[1]] <- NULL
	# Make arguments for legend().
	# legend()の引数を作成。
	legend.args <- do.call(object$build.legend.args, as.list(call))
	legend.args <- legend.args[
		names(legend.args) %in% names(as.list(args(legend)))
	]
	do.call(legend, legend.args)
}


