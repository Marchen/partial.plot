#------------------------------------------------------------------------------
#	partial.plotの結果を用いてレジェンドを描画する。
#------------------------------------------------------------------------------
#'	Draw legend of partial plot.
#'
#'	@param settings
#'		\code{\link{pp.settings}} object resulted by\code{\link{partial.plot}}.
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
#------------------------------------------------------------------------------
pp.legend <- function(settings, x, ...) {
	if (!is(settings, "pp.settings")) {
		stop("'object' should be an instance of 'pp.settings' class")
	}
	# Prepare a list containing arguments specified in '...' named as if
	# they are arguments of legend().
	# '...' に指定された引数をlegend()の引数であるかのように扱い、
	# 名前付きリストに格納する。
	call <- match.call()
	call$settings <- NULL
	call <- match.call(legend, call)
	call <- as.list(call)
	call[[1]] <- NULL
	# Make arguments for legend().
	# legend()の引数を作成。
	legend.args <- prepare.args.for.legend(settings, as.list(call))
	legend.args <- settings$set.function.args(legend.args, legend)
	do.call(legend, legend.args)
}


#------------------------------------------------------------------------------
#	レジェンドの描画に使う引数を用意する。
#------------------------------------------------------------------------------
#'	(Internal) Prepare arguments used for legend.
#'
#'	This function overwrite arguments of legend() that users did not specify
#'	manually with default value.
#'
#'	@param settings
#'		an object of \code{\link{pp.settings}} object having settings of
#'		partial.plot.
#'	@param legend.args
#'		a list containing arguments used for \code{\link[grahpic]{legend}}
#'		function manually specified by users.
#'
#'	@return
#'		A list containing arguments used for \code{\link[grahpic]{legend}}
#'		function
#------------------------------------------------------------------------------
prepare.args.for.legend = function(settings, legend.args) {
	# Override arguments of legend() that users did not specify manually.
	# ユーザーが指定しなかったlegend()関数の引数を上書き。
	group.colors <- settings$parman$colors.for.groups()
	args.to.overwrite <- list(
		col = group.colors, legend = names(group.colors),
		title = paste0(settings$x.names.factor, collapse = settings$sep)
	)
	args.to.overwrite <- c(args.to.overwrite, settings$other.pars)
	for (i in names(args.to.overwrite)) {
		if (is.null(legend.args[[i]])) {
			legend.args[[i]] <- args.to.overwrite[[i]]
		}
	}
	# Set line type based on the setting of partial.plot.
	# 線の種類をpartial.plotの設定に基づいて決定。
	if (settings$draw.relationship) {
		if (is.null(legend.args$lty)) {
			legend.args$lty <- "solid"
		}
	} else {
		legend.args$lty <- NULL
	}
	# Set plot character based on the setting of partial.plot.
	# 点のシンボルをpartial.plotの設定に基づいて決定。
	if (settings$draw.residual) {
		if (is.null(legend.args$pch)) {
			legend.args$pch = 1
		}
	} else {
		legend.args$pch <- NULL
	}
	return(legend.args)
}
