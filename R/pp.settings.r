#------------------------------------------------------------------------------
#	関数の引数名を作るための補助関数。
#------------------------------------------------------------------------------
#'	(Internal) Supporting function which retrieves names of arguments.
#'
#'	@param x list of functions from which names of arguments are retrieved.
#'	@param ... other parameter names to keep.
#'	@return a character vector of argument names.
#------------------------------------------------------------------------------
arg.names <- function(x, ...) {
	args <- character()
	for (i in x) {
		args <- c(args, names(as.list(args(i))))
	}
	args <- c(args, ...)
	args <- unique(args)
	return(args)
}


#------------------------------------------------------------------------------
#	関数の引き数名を用意してキャッシュする。
#------------------------------------------------------------------------------
#	Cache parameter names of graphic functions.
#------------------------------------------------------------------------------
ARG_NAMES <- list(
	persp = arg.names(
		list(graphics:::persp.default),
		"cex.lab", "font.lab", "cex.axis", "font.axis"
	),
	image = arg.names(list(graphics::image.default), "asp", "axes", "bg"),
	lines = arg.names(
		list(graphics::lines.default),
		"lty", "lwd", "lend", "ljoin", "lmitre", "col"
	),
	points = arg.names(
		list(graphics::points.default), "pch", "bg", "cex", "col"
	),
	contour = arg.names(
		list(
			graphics::contour.default, graphics::plot.window,
			graphics::title, graphics::Axis, graphics::axis,
			graphics::box
		),
		"xaxs", "yaxs", "lab", "col.main", "cex.sub", "xpd", "mgp",
		"cex.axis", "col.axis", "font.axis", "xaxp", "yaxp", "tck", "tcl",
		"las", "fg", "xaxt", "yaxt", "bty"
	)
)

if (require(rgl)) {
	ARG_NAMES$persp3d <- arg.names(
		list(rgl:::persp3d.default, rgl::surface3d, rgl::rgl.material), "col"
	)
}


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
#'	@field type
#'		a character literal indicating type of scale for plotting.
#'		This is similar to type argument of many predict methods.
#'		Possible values are "response", "link".
#'		"prob" will be implimented in future.
#'
#'	@field plot.type
#'		a character literal to indicate plot type. Possible values are
#'		"2D" and "3D".
#'
#'	@field factor.levels
#'		a list having levels of factors specified by x.names.
#'
#'	@field numeric.sequences
#'		a list having sequences of numeric variables specified by x.names.
#'
#'	@field data
#'		a data.frame containing data used for plotting.
#'
#'	@field fun.3d
#'		the function draws 3D graphs.
#'		Possibly \code{\link[graphics]{persp}} and
#'		\code{\link[graphics]{images}} can work.
#'
#'	@field draw.residual
#'		a logical representing that partial.plot draws residual points.
#'
#'	@field draw.relationship
#'		a logical representing that partial.plot draws predicted relationships.
#'
#'	@field draw.interval
#'		a logical representing that partial.plot draws intervals of predicted
#'		relationships.
#'
#'	@field interval.levels
#'		numeric vector indicating confidence level or quantiles for
#'		predicted relationships.
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
#'	@param group.colors
#'		colors for each group.
#'
#'	@param obs.colors
#'		colors for each observation.
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
#'	@field add
#'		logical indicating whether graphic elements are added to existing plot.
#'
#'	@field sep
#'		a character representing separator of grouping factor levels.
#'
#'	@field extraporate
#'		a logical indicating whether extraporation is allowed for predicted
#'		relationships.
#'
#'	@field other.pars
#		a list containing other graphic parameters passed to partial.plot().
#'
#'	@field n.cores
#'		an integer specifing number of processes used for multiprocessing.
#'
#'	@field relationship
#'		a data.frame having partial relationship data.
#'
#'	@field relationship.split
#'		a list having partial relationship data splitted for each group.
#'
#'	@field residual
#'		a numeric vector having partial residual data.
#'
#'	@field has.relationship
#'		a logical indicating the object has partial relationship data.
#'
#'	@field has.residual
#'		a logical indicating the object has partial residual data.
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
		type = "character",
		plot.type = "character",
		factor.levels = "list",
		numeric.sequences = "list",
		data = "data.frame",
		fun.3d = "function",
		draw.residual = "logical",
		draw.relationship = "logical",
		draw.interval = "logical",
		interval.levels = "numeric",
		resolution = "ANY",
		col = "ANY",
		group.colors = "ANY",
		obs.colors = "ANY",
		xlab = "ANY",
		ylab = "ANY",
		zlab = "ANY",
		add = "logical",
		sep = "character",
		extraporate = "logical",
		other.pars = "list",
		n.cores = "ANY",
		relationship = "data.frame",
		relationship.split = "list",
		residual = "numeric",
		has.relationship = "logical",
		has.residual = "logical"
	)
)


#------------------------------------------------------------------------------
#	設定を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	initialize = function(
		model, x.names, data = NULL, type = "response", fun.3d = persp,
		draw.residual = TRUE, draw.relationship = TRUE,
		draw.interval = TRUE, interval.levels = 0.95, resolution = NULL,
		col = gg.colors, xlab = NULL, ylab = NULL, zlab = NULL, add = FALSE,
		sep = " - ", extraporate = FALSE, n.cores = NULL, ...
	) {
		"
		Initialize pp.settings object.
		\\describe{
			\\item{\\code{model}}{
				a supported model object used for plotting.
				}
			\\item{\\code{x.names}}{
				a character vector representing names of explanatory variables
				used for plotting.
			}
			\\item{\\code{data}}{
				a data.frame containing data used for plotting.
			}
			\\item{\\code{type}}{
				type of relationship to draw.
				Possible values are 'response' and 'link'.
			}
			\\item{\\code{fun.3d}}{
				the function used for drawing 3D relationship graphs.
				Possibly, persp and image can work.
			}
			\\item{\\code{draw.residual}}{
				whether the partial.plot draws residual points.
			}
			\\item{\\code{draw.relationship}}{
				whether partial.plot draws predicted relationships.
			}
			\\item{\\code{draw.interval}}{
				whetehr partial.plot draws interval of predicted relationships.
			}
			\\item{\\code{interval.levels}}{
				numeric vector specifying level of confidence
				intervals/quantile.
			}
			\\item{\\code{resolution}}{
				an integer specifying resolution of lines, polygons,
				wireframes, and images of numeric variables.
			}
			\\item{\\code{col}}{
				color vector or color producing function.
			}
			\\item{\\code{xlab}, \\code{ylab}, \\code{zlab}}{
				a character specifying used for label of X/Y/Z axis.
			}
			\\item{\\code{add}}{
				logical specifying whether graphic elements are added to the
				existing plot.
			}
			\\item{\\code{sep}}{
				a character representing separator of grouping factor levels.
			}
			\\item{\\code{expraporate}}{
				a logical indicating whether extraporation is allowed for
				predicted relationships.
			}
			\\item{\\code{n.cores}}{
				an integer representing number of processes used for
				calculation. If NULL is specified, maximum number of logical
				processors are used. This value is ignored when the models
				compatible with lsmeans are specified.
			}
			\\item{\\code{...}}{
				other graphic parameters.
			}
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
			adapter = model.adapter(
				model, data = data, envir = parent.frame(5L)
			),
			type = type, fun.3d = fun.3d, model = model, x.names = x.names,
			draw.residual = draw.residual,
			draw.relationship = draw.relationship,
			draw.interval = draw.interval, interval.levels = interval.levels,
			resolution = resolution,
			col = col, xlab = xlab, ylab = ylab, zlab = zlab, add = add,
			sep = sep, extraporate = extraporate, n.cores = n.cores,
			other.pars = list(...), has.relationship = FALSE,
			has.residual = FALSE
		)
		initFields(data = adapter$data)
		.self$check.params()
		.self$init.multiprocessing()
		.self$init.x.names()
		.self$init.plot.type()
		.self$init.labels()
		.self$init.colors()
		.self$init.intervals()
		.self$init.resolution()
		.self$init.factor.levels()
		.self$init.numeric.sequences()
	}
)


#------------------------------------------------------------------------------
#	主に描画関連の設定を更新する。
#------------------------------------------------------------------------------
pp.settings$methods(
	update.pars = function(
		fun.3d, draw.residual, draw.relationship, draw.interval,
		col, xlab, ylab, zlab, ...
	) {
		"
		Update settings of (mainly) graphic parameters for data reusing.
		\\code{fun.3d}, \\code{draw.residual}, \\code{draw.relationship},
		\\code{draw.interval}, \\code{col}, \\code{xlab}, \\code{ylab},
		\\code{zlab} and \\code{...} can be set.
		"
		initFields(
			fun.3d = fun.3d, draw.residual = draw.residual,
			draw.relationship = draw.relationship,
			draw.interval = draw.interval, col = col, xlab = xlab,
			ylab = ylab, zlab = zlab, other.pars = list(...)
		)
		.self$check.params()
		.self$init.labels()
		.self$init.colors()
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
#	信頼区間、分位点が0と1の間かを確認する。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.intervals = function() {
		"
		Check interval of predicted relationships.
		"
		if (all(interval.levels < 0 | interval.levels > 1)) {
			stop("'interval.levels' should be 0 <= interval.levels <= 1")
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
		if (!is.null(.self$resolution)) {
			if (.self$resolution %% 1 != 0) {
				stop("'resolution' should be integer.")
			}
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
#	予測値の種類の設定が正しいかをチェックする。
#------------------------------------------------------------------------------
pp.settings$methods(
	check.type = function() {
		"
		Check specified type.
		"
		if (length(type) != 1) {
			stop("'type' should be a character of length 1.")
		}
		if (!type %in% c("response", "link")) {
			stop("'type' should be one of 'response' and 'link'.")
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
		.self$check.intervals()
		.self$check.resolution()
		.self$check.labels()
		.self$check.type()
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
#	因子型・数値型の説明変数名を準備する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.x.names = function() {
		"
		Initialize names of factor and numeric explanatory variables.
		"
		.self$x.names.factor <- x.names[
			sapply(data[x.names], is.factor)
			| sapply(data[x.names], is.character)
		]
		.self$x.names.numeric <- x.names[sapply(data[x.names], is.numeric)]
	}
)


#------------------------------------------------------------------------------
#	プロットの種類（2D/3D）を設定する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.plot.type = function() {
		"
		Determine plot type (2D/3D).
		"
		if (length(.self$x.names.numeric) == 2) {
			.self$plot.type <- "3D"
		} else {
			.self$plot.type <- "2D"
		}
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
			if (.self$plot.type == "2D") {
				.self$ylab <- .self$adapter$y.names()
			} else {
				.self$ylab <- x.names.numeric[2]
			}
		}
		if (is.null(zlab) & .self$plot.type == "3D") {
			.self$zlab <- .self$adapter$y.names()
		}
	}
)


#------------------------------------------------------------------------------
#	色ベクトルを初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.colors = function() {
		"
		Initialize colors.
		"
		brewer = pp.colors(.self)
		.self$group.colors <- brewer$colors.for.groups()
		.self$obs.colors <- brewer$colors.for.observations()
	}
)


#------------------------------------------------------------------------------
#	信頼区間、分位点のレベル数を調整する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.intervals = function() {
		"
		Initialize settings of intervals.
		"
		if (any(class(.self$model) %in% LSMEANS_COMPATIBLE_MODELS)) {
			.self$interval.levels <- interval.levels[1]
		} else {
			if (length(.self$interval.levels) == 1) {
				.self$interval.levels <- range(
					.self$interval.levels, 1 - .self$interval.levels
				)
			} else {
				.self$interval.levels <- .self.interval.levels[1:2]
			}
		}
	}
)


#------------------------------------------------------------------------------
#	解像度を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.resolution = function() {
		"
		Initialize resolution
		"
		if (is.null(.self$resolution)) {
			if (.self$plot.type == "3D") {
				.self$resolution <- 10
			} else {
				.self$resolution <- 100
			}
		}
	}
)


#------------------------------------------------------------------------------
#	因子のレベル一覧を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.factor.levels = function() {
		"
		Initialize factor levels.
		"
		result <- list()
		for (name in .self$x.names.factor) {
			result[[name]] <- unique(.self$data[[name]])
		}
		.self$factor.levels <- result
	}
)


#------------------------------------------------------------------------------
#	予測値計算対象の数値型の数列を初期化する。
#------------------------------------------------------------------------------
pp.settings$methods(
	init.numeric.sequences = function() {
		"
		Initialize numeric sequences.
		"
		result <- list()
		for (name in .self$x.names.numeric) {
			result[[name]] <- seq(
				min(.self$data[[name]], na.rm = TRUE),
				max(.self$data[[name]], na.rm = TRUE),
				length.out = .self$resolution
			)
		}
		.self$numeric.sequences <- result
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
			res <- clusterCall(
				cl, library, package = .self$adapter$package.name,
				character.only = TRUE
			)
			return(parLapply(cl, x = X, fun = FUN, ...))
		}
	}
)


#------------------------------------------------------------------------------
#	関数の引き数名の候補を取得する。
#------------------------------------------------------------------------------
pp.settings$methods(
	potential.arg.names = function(fun) {
		"
		Return a character vector having names of potential
		arguments of the function.

		\\describe{
			\\item{fun}{function to get potential argument names.}
		}
		"
		if (identical(fun, persp)) {
			return(ARG_NAMES$persp)
		}
		if (identical(fun, image)) {
			return(ARG_NAMES$image)
		}
		if (identical(fun, graphics::lines)) {
			return(ARG_NAMES$lines)
		}
		if (identical(fun, graphics::points)) {
			return(ARG_NAMES$points)
		}
		if (identical(fun, graphics::contour)) {
			return(ARG_NAMES$contour)
		}
		if (require(rgl)) {
			if (identical(fun, rgl::persp3d)) {
				return(ARG_NAMES$persp3d)
			}
		}
		args <- names(as.list(args(fun)))
		return(args)
	}
)


#------------------------------------------------------------------------------
#	関数の呼び出しに使う引数に保持している情報を適用する。
#------------------------------------------------------------------------------
pp.settings$methods(
	set.function.args = function(
		function.args = list(), fun = .self$fun.3d
	) {
		"
		Assign function arguments kept in this class and returns it in a list.

		\\describe{
			\\item{function.args}{
				arguments for the function call to be modified.
			}
			\\item{fun}{target function to be call.}
		}
		"
		pars <- c(list(xlab = xlab, ylab = ylab, zlab = zlab), other.pars)
		# Copy pars if it's not in function.args.
		for (i in names(pars)) {
			if (!i %in% names(function.args)) {
				function.args[[i]] <- pars[[i]]
			}
		}
		function.args <- function.args[
			names(function.args) %in% .self$potential.arg.names(fun)
		]
		return(function.args)
	}
)


#------------------------------------------------------------------------------
#	偏依存性データを設定する。
#------------------------------------------------------------------------------
pp.settings$methods(
	set.relationship = function(object) {
		"
		Set partial relationship data.
		"
		.self$relationship <- object$data
		.self$relationship.split <- object$data.split
		.self$has.relationship <- TRUE
	}
)


#------------------------------------------------------------------------------
#	偏残差データを設定する。
#------------------------------------------------------------------------------
pp.settings$methods(
	set.residual = function(object) {
		"
		Import partial residual data.
		"
		.self$residual <- object$data
		.self$has.residual <- TRUE
	}
)
