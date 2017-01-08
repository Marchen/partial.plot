#------------------------------------------------------------------------------
#	Names of models supported by lsmeans.
#------------------------------------------------------------------------------
LSMEANS_COMPATIBLE_MODELS <- c(
	"lm", "glm", "lme", "glmmML", "MCMCglmm", "glmmadmb"
)


#------------------------------------------------------------------------------
#	変依存性を計算するクラス。
#------------------------------------------------------------------------------
#'	Calculate and draw partial relationship.
#'
#'	To use this class initialize it and call draw() method.
#'
#'	@field data
#'		data.frame to keep calculated partial relationship data.
#'	@field settings
#'		class settings.
#'	@include pp.settings.r
#------------------------------------------------------------------------------
partial.relationship <- setRefClass(
	"partial.relationship",
	field = list(data = "data.frame", settings = "pp.settings")
)


#------------------------------------------------------------------------------
#	クラスを初期化して偏依存性を計算する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	initialize = function(settings) {
		"
		Initialize class and calculate partial.relationship data.

		\\describe{
			\\item{settings}{pp.settings object having function settings.}
		}
		"
		# Safe-guard for internal processing of reference class.
		if (missing(settings)) {
			return()
		}
		.self$settings <- settings
		# Calculate partial relationship data.
		if (any(class(settings$model) %in% LSMEANS_COMPATIBLE_MODELS)) {
			.self$data <- partial.relationship.lsmeans()
		} else {
			.self$data <- partial.relationship.internal()
		}
	}
)


#------------------------------------------------------------------------------
#	lsmeansを使って予測値と信頼区間を計算する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	partial.relationship.lsmeans = function() {
		"
		Calculate partial regression lines and intervals.

		This function calculates data for regression lines and intervals using
		\\code{\\link[lsmeans]{lsmeans}} function and returns a data.frame.
		"
		# prepare combinations of x variables.
		# 説明変数の組み合わせを用意。
		numerics <- numeric.sequences(settings)
		# calculate prediction.
		# 予測値を計算。
		at <- c(numerics, settings$factor.levels)
		rg <- ref.grid(
			settings$model, at, data = settings$data, type = "terms"
		)
		lsm <- summary(
			lsmeans(rg, settings$x.names), level = settings$interval.levels
		)
		colnames(lsm)[colnames(lsm) == "lsmean"] <- "fit"
		colnames(lsm)[colnames(lsm) == "lower.CL"] <- "lower"
		colnames(lsm)[colnames(lsm) == "upper.CL"] <- "upper"
		colnames(lsm)[colnames(lsm) == "asymp.LCL"] <- "lower"
		colnames(lsm)[colnames(lsm) == "asymp.UCL"] <- "upper"
		# Remove predictions with out-ranged explanatory variable for each group.
		# 各グループの説明変数の範囲を外れた予測値を削除。
		if (length(settings$factor.levels) != 0) {
			lsm <- filter.result(lsm)
		}
		return(as.data.frame(lsm))
	}
)


#------------------------------------------------------------------------------
#	クラスターで予測値を計算するラッパー関数。
#------------------------------------------------------------------------------
partial.relationship$methods(
	predict.stats = function(
		newdata, predict.fun, new.value.grid, index, levels
	) {
		"
		Calculate prediction in cluster.

		\\describe{
			\\item{\\code{newdata}}{newdata for prediction.}
			\\item{\\code{predict.fun}}{predict function.}
			\\item{\\code{new.value.grid}}{
				a data.frame having all combination of focal values of
				partial relationship grid.
			}
			\\item{\\code{index}}{
				row index of \\code{new.value.grid} where predicted values
				are calculated.
			}
			\\item{\\code{levels}}{levels of quantiles.}
		}
		"
		# Make data for prediction.
		# 予測用データを作成。
		param.names <- names(new.value.grid)
		replace.values <- new.value.grid[index,]
		names(replace.values) <- param.names
		for (i in param.names) {
			newdata[[i]] <- replace.values[[i]]
		}
		prediction <- predict.fun(
			#### TODO #####
			#### typeをなんとかする。
			newdata = newdata
		)
		quantiles <- quantile(prediction$fit, probs = levels, na.rm = TRUE)
		result <- c(fit = median(prediction$fit), quantiles, replace.values)
		names(result) <- c("fit", "lower", "upper", param.names)
		result <- as.data.frame(as.list(result))
		return(result)
	}
)


#------------------------------------------------------------------------------
#	偏依存関係を自前で計算する関数。主に機械学習モデル用。
#------------------------------------------------------------------------------
partial.relationship$methods(
	partial.relationship.internal = function() {
		"
		Calculate partial relationship data internally.
		Mainly used for machine learning methods.
		"
		# prepare combinations of x variables.
		# 説明変数の組み合わせを用意。
		numerics <- numeric.sequences(settings)
		grid <- do.call(expand.grid, c(numerics, settings$factor.levels))
		# Initialize cluster.
		# クラスター初期化
		cl <- makeCluster(settings$n.cores)
		on.exit(stopCluster(cl))
		clusterEvalQ(cl, library(model.adapter))
		# Run calculation.
		# 計算実行
		result <- settings$cluster.apply(
			X = 1:nrow(grid), FUN = .self$predict.stats,
			newdata = settings$data, predict.fun = settings$adapter$predict,
			new.value.grid = grid, levels = settings$interval.levels
		)
		result <- do.call(rbind, result)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	lsmeansの結果から、各グループごとにX軸の値が元のデータの範囲外に
#	ある予測値を削除する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	filter.result = function(prediction) {
		"
		Remove out-ranged values from result of lsmeans.

		This internal function removes predicted values those explanatory
		variable is out of range of original data used for modeling for each
		group and returns a resultant data.frame.

		\\describe{
			\\item{prediction}{result of lsmeans.}
		}
		"
		# Get list of unique factors.
		# 因子の一覧を作成。
		factors <- expand.grid(settings$factor.levels)
		# Split data and prediction for each factor group.
		# データを因子のグループごとに分割。
		sep = settings$sep
		pred.split <- split(prediction, prediction[names(factors)], sep = sep)
		data.split <- split(
			settings$data, settings$data[names(factors)], sep = sep
		)
		# Filter out out-ranged numeric values.
		# 範囲外の数値を削除。
		result <- list()
		for (i in 1:nrow(factors)) {
			split.name <- combine.columns(
				as.data.frame(factors[i,]), sep = sep
			)
			current.pred <- pred.split[[split.name]]
			current.data <- data.split[[split.name]]
			for (numeric.name in settings$x.names.numeric) {
				var.range <- range(current.data[[numeric.name]])
				filter <- current.pred[[numeric.name]] >= var.range[1]
				filter <- filter & current.pred[[numeric.name]] <= var.range[2]
				current.pred <- current.pred[filter,]
			}
			result[[split.name]] <- current.pred
		}
		result <- do.call(rbind, result)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	説明変数と応答変数の関係式を描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw = function() {
		"
		Draw partial relationship graph.
		"
		# Dispatch based on number of numeric variables.
		# 数値型の説明変数の数に応じて使う関数を変える。
		if (length(settings$x.names.numeric) == 2) {
			draw.3d()
		} else {
			draw.2d()
		}
	}
)


#------------------------------------------------------------------------------
#	二次元偏依存性の線を描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw.relationship.2d = function(data.split) {
		for (i in names(settings$group.colors)) {
			"
			(Internal) Draw partial relationship lines in 2D graph.
			\\describe{
				\\item{data.split}{
					splitted partial relationship data for each group.
				}
			}
			"
			current.data <- data.split[[i]]
			args <- list(
				x = current.data[[settings$x.names.numeric]],
				y = current.data$fit, col = settings$group.colors[i]
			)
			args <- settings$set.function.args(args, lines)
			do.call(lines, args)
		}
	}
)


#------------------------------------------------------------------------------
#	二次元偏依存性の信頼（などの）区間を描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw.interval.2d = function(data.split) {
		"
		(Internal) Draw intervals of partial.relationship in 2D graph.
		\\describe{
			\\item{data.split}{
				splitted partial relationship data for each group.
			}
		}
		"
		if (settings$draw.intervals) {
			for (i in names(settings$group.colors)) {
				current.data <- data.split[[i]]
				x <- current.data[[settings$x.names.numeric]]
				x <- c(x, rev(x))
				y <- c(current.data$lower, rev(current.data$upper))
				polygon(
					x, y, border = NA,
					col = trans.color(settings$group.colors[i])
				)
			}
		}
	}
)


#------------------------------------------------------------------------------
#	二次元偏依存性グラフを描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw.2d = function() {
		"
		Draw 2D partial relationship graph.
		"
		# Open new plot.
		open.new.plot(settings, .self$data)
		# Split data.
		if (length(settings$group.colors) == 1) {
			pr.data <- list(all = .self$data)
		} else {
			pr.data <- split(
				.self$data, .self$data[settings$x.names.factor],
				sep = settings$sep
			)
		}
		# Draw polygons.
		.self$draw.interval.2d(pr.data)
		# Draw partial relationships.
		.self$draw.relationship.2d(pr.data)
	}
)


#------------------------------------------------------------------------------
#	三次元偏依存性グラフを描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw.3d = function() {
		"
		Draw 3D partial relationship graph.
		"
		cat("3D plot is experimental yet... :-)\n")
		z.matrix <- matrix(.self$data$fit, nrow = settings$resolution)
		if (identical(settings$function.3d, image)) {
			col <- color.ramp(z.matrix, settings$col, unique.pal = TRUE)
		} else if (identical(settings$function.3d, persp)) {
			col <- pp.colors(settings)$colors.for.persp(z.matrix)
		} else {
			col <- color.ramp(z.matrix, settings$col)
		}
		args <- list(
			z = z.matrix,
			x = unique(.self$data[[settings$x.names[1]]]),
			y = unique(.self$data[[settings$x.names[2]]]),
			xlab = settings$xlab, ylab = settings$ylab, zlab = settings$zlab,
			col = col
		)
		args <- settings$set.function.args(args)
		do.call(settings$function.3d, args)
	}
)
