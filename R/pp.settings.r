#------------------------------------------------------------------------------
#	partial.plotの情報を保持するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class to keep settings/information of partial.plot.
#'
#'	This reference class keeps information used for partial.plot.
#'	This class is not intended to be used by users directly.
#'
#'	@field adapter
#'		an instance of model.adapter.
#'
#'	@field model
#'		a supported model object used for plotting.
#'
#'	@field x.names
#'		a character vector representing names of explanatory variables used
#'		for plotting.
#'
#'	@field x.names.factor
#'		a character vector representing names of factor explanatory variables
#'		used for plotting.
#'
#'	@field x.names.numeric
#'		a character vector representing names of numeric explanatory variables
#'		used for plotting.
#'
#'	@field data
#'		a data.frame containing data used for plotting.
#'
#'	@field function.3d
#'		the function draws 3D graphs.
#'		Possibly \code{\link[graphics]{persp}} and
#'		\code{\link[graphics]{images}} can work.
#'
#'	@field draw.residuals
#'		a logical representing that partial.plot drew residual points or not.
#'
#'	@field draw.relationships
#'		a logical representing that partial.plot drew predicted relationships
#'		or not.
#'
#'	@field resolution
#'		an integer specifying resolution of lines, polygons, wireframes,
#'		and images of numeric variables.
#'
#'	@param col
#'		a function or named character vector representing color of the graph
#'		object.
#'		For the detail, see pal option of \code{\link{color.ramp}} function.
#'
#'	@field xlab
#'		a character or expression specifying used for label of X axis.
#'
#'	@field ylab
#'		a character or expression specifying used for label of Y axis.
#'
#'	@field zlab
#'		a character or expression specifying used for label of Z axis.
#'
#'	@field sep
#'		a character representing separator of grouping factor levels.
#'
#'	@field other.pars
#		a list containing other graphic parameters passed to partial.plot().
#'
#'	@field n.cores
#'		an integer specifing number of processes used for multiprocessing.
#'
#------------------------------------------------------------------------------
pp.settings <- setRefClass(
	"pp.settings",
	fields = list(
		adapter = "ANY",
		model = "ANY",
		x.names = "character",
		x.names.factor = "character",
		x.names.numeric = "character",
		data = "data.frame",
		function.3d = "function",
		draw.residuals = "logical",
		draw.relationships = "logical",
		resolution = "ANY",
		col = "ANY",
		xlab = "ANY",
		ylab = "ANY",
		zlab = "ANY",
		sep = "character",
		other.pars = "list",
		n.cores = "ANY"
	)
)


#------------------------------------------------------------------------------
#	設定を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	initialize = function(
		model, x.names, data = NULL, function.3d = persp,
		draw.residuals = TRUE, draw.relationships = TRUE, resolution = 10L,
		col = gg.colors, xlab = NULL, ylab = NULL, zlab = NULL, sep = " - ",
		n.cores = NULL, ...
	) {
		"
		Initialize pp.settings object.
		\\describe{
			\\item{model}{a supported model object used for plotting.}
			\\item{x.names}{
				a character vector representing names of explanatory variables
				used for plotting.
			}
			\\item{data}{a data.frame containing data used for plotting.}
			\\item{function.3d}{
				the function used for drawing 3D relationship graphs.
				Possibly, persp and image can work.
			}
			\\item{draw.residuals}{
				whether the partial.plot drewn residual points or not.
			}
			\\item{draw.relationships}{
				whether partial.plot drew predicted relationships or not.
			}
			\\item{resolution}{
				an integer specifying resolution of lines, polygons,
				wireframes, and images of numeric variables.
			}
			\\item{col}{color vector.}
			\\item{xlab, ylab, zlab}{
				a character specifying used for label of X/Y/Z axis.
			}
			\\item{sep}{
				a character representing separator of grouping factor levels.
			}
			\\item{n.cores}{
				an integer representing number of processes used for
				calculation. If NULL is specified, maximum number of logical
				processors are used. This value is ignored when the models
				compatible with lsmeans are specified.
			}
			\\item{...}{other graphic parameters.}
		}
		"
		# If required arguments are not specified, exit function with doing
		# nothing by assuming called from mechanisms of the referance class.
		# 必須のパラメーターなしで呼び出されたらReference Classの機能で
		# 呼び出されたことを仮定し、何もしないで終了する。
		if (missing(model) | missing(x.names)) {
			return()
		}
		initFields(
			adapter = model.adapter(model, data = data),
			function.3d = function.3d, model = model, x.names = x.names,
			draw.residuals = draw.residuals,
			draw.relationships = draw.relationships, resolution = resolution,
			col = col, xlab = xlab, ylab = ylab, zlab = zlab, sep = sep,
			n.cores = n.cores, other.pars = list(...)
		)
		initFields(data = adapter$data)
		.self$check.params()
		.self$init.multiprocessing()
		.self$init.x.names()
		.self$init.labels()
	}
)


#------------------------------------------------------------------------------
#	dataが使えるかを確認。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.data.access = function() {
		"
		Check availability of data.
		"
		if (!adapter$has.data()) {
			stop("'model' object does not have original data. Please specify 'data' argument.")
		}
	}
)


#------------------------------------------------------------------------------
#	x.namesの確認。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.x.names = function() {
		"
		Check correctness of x.names.
		"
		# Check variables in x.names are used in model and exist in data.
		# x.namesで指定されたデータがモデルに使われていて、
		# データにも存在するかをチェック
		if (!all(x.names %in% colnames(adapter$data))) {
			error <- x.names[!x.names %in% colnames(adapter$data)]
			stop(sprintf("\n Column '%s' is not found in data.", error))
		}
		if (!all(x.names %in% adapter$x.names(type = "base"))) {
			error <- x.names[!x.names %in% adapter$x.names(type = "base")]
			template <- "\n '%s' is not found in explanatory variables."
			stop(sprintf(template, error))
		}
		# Check number of continuous explanatory variables
		# 連続値の説明変数の数チェック
		var.types <- sapply(adapter$data[, x.names, drop = FALSE], class)
		n.continuous <- sum(var.types == "numeric")
		if (n.continuous > 2) {
			message <- paste(
				"Plotting more than two continuous explanatory variables",
				"is not supported."
			)
			stop(message)
		}
	}
)


#------------------------------------------------------------------------------
#	解像度が整数かどうかをチェックする。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.resolution = function() {
		"
		Check resolution is integer.
		"
		if (resolution %% 1 != 0) {
			stop("'resolution' should be integer.")
		}
	}
)


#------------------------------------------------------------------------------
#	xlabとylabの設定が正しいかをチェックする。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.labels = function() {
		"
		Test correctness of xlab and ylab.
		"
		if (!is.null(xlab) & !is.character(xlab) & !is.expression(xlab)) {
			stop("'xlab' should be NULL/character/expression.")
		}
		if (!is.null(ylab) & !is.character(ylab) & !is.expression(ylab)) {
			stop("'ylab' should be NULL/character/expression.")
		}
		if (!is.null(zlab) & !is.character(zlab) & !is.expression(zlab)) {
			stop("'zlab' should be NULL/character/expression.")
		}
	}
)


#------------------------------------------------------------------------------
#	パラメーターの整合性を確認する。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.params = function() {
		"
		Check errors in specified parameters and stop if any errors are found.
		"
		.self$check.data.access()
		.self$check.x.names()
		.self$check.resolution()
		.self$check.labels()
	}
)


#------------------------------------------------------------------------------
#	並列計算の設定を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.multiprocessing = function() {
		"
		Initialize multiprocessing.
		"
		if (is.null(.self$n.cores)) {
			# If n.cores is NULL, use all logical processors.
			.self$n.cores <- detectCores()
		} else {
			# If n.cores is not integer, raise error.
			if (.self$n.cores %% 1 != 0) {
				stop("'n.cores' should be integer or NULL")
			}
		}
	}
)


#------------------------------------------------------------------------------
#	xlab、ylab、zlabを設定する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.x.names = function() {
		.self$x.names.factor <- x.names[sapply(data[x.names], is.factor)]
		.self$x.names.numeric <- x.names[sapply(data[x.names], is.numeric)]
	}
)


#------------------------------------------------------------------------------
#	xlab、ylab、zlabを設定する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.labels = function() {
		"
		Set xlab, ylab and zlab for 2D & 3D plot.
		"
		if (is.null(xlab)) {
			.self$xlab <- x.names.numeric[1]
		}
		if (is.null(ylab)) {
			if (length(x.names.numeric) == 1) {
				.self$ylab <- .self$adapter$y.names()
			} else {
				.self$ylab <- x.names.numeric[2]
			}
		}
		if (is.null(zlab) & length(x.names.numeric) == 2) {
			.self$zlab <- .self$adapter$y.names()
		}
	}
)


#------------------------------------------------------------------------------
#	コア数の設定に合わせてlapply()かclusterApply()を実行する。
#------------------------------------------------------------------------------
pp.settings$methods(
	cluster.apply = function(X, FUN, ...) {
		"
		Interface for lapply() or clusterApply()
		"
		if (.self$n.cores == 1) {
			return(lapply(X, FUN, ...))
		} else {
			cl <- makeCluster(.self$n.cores)
			on.exit(stopCluster(cl))
			clusterEvalQ(cl, library(model.adapter))
			return(clusterApply(cl, x = X, fun = FUN, ...))
		}
	}
)



