#-------------------------------------------------------------------------------
#	partial.plotの結果を用いてレジェンドを描画する。
#-------------------------------------------------------------------------------
#'	Draw legend of partial plot.
#'
#'	@param object
#'		\code{\link{pp.info}} object resulted by\code{\link{partial.plot}}.
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
pp.legend <- function(object, x, ...) {
	if (!is(object, "pp.settings")) {
		stop("'object' should be an instance of 'pp.settings' class")
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


