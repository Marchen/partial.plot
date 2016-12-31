#------------------------------------------------------------------------------
#	partial.plotの情報を保持するクラス。
#-------------------------------------------------------------------------------
#'	(Internal) A reference class to keep settings/information of partial.plot.
#'
#'	This reference class keeps information used for partial.plot.
#'	This class is not intended to be used by users directly.
#'
#'	@field adapter
#'		an instance of model.adapter.
#'	@field model
#'		a supported model object used for plotting.
#'	@field x.names
#'		a character vector representing names of explanatory variables used
#'		for plotting.
#'	@field data
#'		a data.frame containing data used for plotting.
#'	@field draw.residuals
#'		a logical representing that partial.plot drew residual points or not.
#'	@field draw.relationships
#'		a logical representing that partial.plot drew predicted relationships
#'		or not.
#'	@field resolution
#'		an integer specifying resolution of lines, polygons, wireframes,
#'		and images of numeric variables.
#'	@param col
#'		a function or named character vector representing color of the graph
#'		object.
#'		For the detail, see pal option of \code{\link{color.ramp}} function.
#'	@field xlab
#'		a character specifying used for label of X axis.
#'	@field ylab
#'		a character specifying used for label of Y axis.
#'	@field sep
#'		a character representing separator of grouping factor levels.
#'	@field n.cores
#'		an integer specifing number of processes used for multiprocessing.
#'	@field other.pars
#		a list containing other graphic parameters passed to partial.plot().
#-------------------------------------------------------------------------------
pp.settings <- setRefClass(
	"pp.settings",
	fields = list(
		adapter = "ANY",
		model = "ANY",
		x.names = "character",
		data = "data.frame",
		draw.residuals = "logical",
		draw.relationships = "logical",
		resolution = "integer",
		col = "ANY",
		xlab = "character",
		ylab = "character",
		sep = "character",
		other.pars = "list",
		n.cores = "ANY"
	)
)


pp.settings$methods(
	initialize = function(
		model, x.names, data = NULL, draw.residuals = TRUE,
		draw.relationships = TRUE, resolution = 100L, col = gg.colors,
		xlab = character(), ylab = character(), sep = " - ", n.cores = NULL,
		...
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
			\\item{xlab, ylab}{
				a character specifying used for label of X/Y axis.
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
			model = model, x.names = x.names,
			draw.residuals = draw.residuals,
			draw.relationships = draw.relationships, resolution = resolution,
			col = col, xlab = xlab, ylab = ylab, sep = sep, n.cores = n.cores,
			other.pars = list(...)
		)
		initFields(data = adapter$data)
		.self$check.params()
		.self$init.multiprocessing()
	}
)



#-------------------------------------------------------------------------------
#	パラメーターの整合性を確認する。
#-------------------------------------------------------------------------------
pp.settings$methods(
	check.params = function() {
		"
		Check cerrors in specified parameters and stop if any errors are found.
		"
		# check availability of data.
		# dataが使えるかを確認。
		if (!adapter$has.data()) {
			stop("'model' object does not have original data. Please specify 'data' argument.")
		}
		# check x.names.
		# x.namesのチェック。
		if (!all(x.names %in% colnames(adapter$data))) {
			error <- x.names[!x.names %in% colnames(adapter$data)]
			stop(sprintf("\n Column '%s' is not found in data.", error))
		}
		if (!all(x.names %in% adapter$x.names(type = "base"))) {
			error <- x.names[!x.names %in% adapter$x.names(type = "base")]
			stop(
				sprintf("\n '%s' is not found in explanatory variables.", error)
			)
		}
		# Check number of continuous explanatory variables
		# 連続値の説明変数の数チェック
		var.types <- sapply(adapter$data[, x.names, drop = FALSE], class)
		n.continuous <- sum(var.types == "numeric")
		if (n.continuous > 2) {
			stop("Plotting more than two continuous explanatory variables is not supported.")
		}
	}
)


#-------------------------------------------------------------------------------
#	並列計算の設定を初期化する。
#-------------------------------------------------------------------------------
pp.settings$methods(
	init.multiprocessing = function() {
		"
		Initialize multiprocessing.
		"
		if (is.null(.self$n.cores)) {
			# If n.cores is NULL, use all logical processors.
			require(parallel)
			.self$n.cores <- detectCores()
		} else {
			# If n.cores is not integer, raise error.
			if (.self$n.cores %% 1 != 0) {
				stop("'n.cores' should be integer or NULL")
			}
		}
	}
)

