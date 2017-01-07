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
#'	@field data
#'		data.frame to keep calculated partial relationship data.
#'	@field settings
#'		class settings.
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
		factors <- get.unique.factors(settings)
		# calculate prediction.
		# 予測値を計算。
		at <- c(numerics, factors)
		rg <- ref.grid(
			settings$model, at, data = settings$data, type = "terms"
		)
		lsm <- summary(lsmeans(rg, settings$x.names))
		colnames(lsm)[colnames(lsm) == "lsmean"] <- "fit"
		colnames(lsm)[colnames(lsm) == "lower.CL"] <- "lower"
		colnames(lsm)[colnames(lsm) == "upper.CL"] <- "upper"
		colnames(lsm)[colnames(lsm) == "asymp.LCL"] <- "lower"
		colnames(lsm)[colnames(lsm) == "asymp.UCL"] <- "upper"
		# Remove predictions with out-ranged explanatory variable for each group.
		# 各グループの説明変数の範囲を外れた予測値を削除。
		if (length(factors) != 0) {
			lsm <- filter.result(lsm)
		}
		return(as.data.frame(lsm))
	}
)


#------------------------------------------------------------------------------
#	クラスターで予測値を計算するラッパー関数。
#------------------------------------------------------------------------------
partial.relationship$methods(
	predict.stats = function(newdata, predict.fun, new.value.grid, index) {
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
		#### TODO #####
		#### Quantileのハードコードをなんとかする
		quantiles <- quantile(
			prediction$fit, probs = c(0.05, 0.95), na.rm = TRUE
		)
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
		factors <- get.unique.factors(settings)
		grid <- do.call(expand.grid, c(numerics, factors))
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
			new.value.grid = grid
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
		"
		# Get list of unique factors.
		# 因子の一覧を作成。
		factors <- expand.grid(get.unique.factors(settings))
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
		"Draw partial relationship graph."
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
#	二次元偏依存性グラフを描画する。
#------------------------------------------------------------------------------
partial.relationship$methods(
	draw.2d = function() {
		"
		Draw 2D partial relationship graph.
		"
		# Open new plot.
		# 新しいプロットを開く。
		open.new.plot(settings, .self$data)
		# Prepare color palette.
		# カラーパレットを用意。
		color.palette <- set.group.color(settings, TRUE)
		# Split data.
		# データを分割。
		if (length(names(color.palette)) == 1) {
			pr.data<- list(all = .self$data)
		} else {
			pr.data <- split(
				.self$data, .self$data[settings$x.names.factor],
				sep = settings$sep
			)
		}
		# Draw polygons.
		# ポリゴンを描画
		for (i in names(color.palette)) {
			d <- pr.data[[i]]
			x <- d[[settings$x.names.numeric]]
			x <- c(x, rev(x))
			y <- c(d$lower, rev(d$upper))
			polygon(x, y, border = NA, col = trans.color(color.palette[i]))
		}
		# Draw partial relationships.
		# To handle valid graphic paramters in ... for lines, use do.call.
		# 関係式を描画。...の中からlinesで使えるグラフィックパラメーターだけを
		# 使うため、do.callを呼ぶ。
		for (i in names(color.palette)) {
			d <- pr.data[[i]]
			args <- list(
				x = d[[settings$x.names.numeric]],
				y = d$fit, col = color.palette[i]
			)
			lines.par <- c("lty", "lwd", "lend", "ljoin", "lmitre")
			args <- c(args, settings$other.pars[settings$other.pars %in% lines.par])
			do.call(lines, args)
		}
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
			col <- viridis(settings$resolution)
		} else {
			col <- "black"
		}
		settings$function.3d(
			z.matrix,
			x = unique(.self$data[[settings$x.names[1]]]),
			y = unique(.self$data[[settings$x.names[2]]]),
			xlab = settings$xlab, ylab = settings$ylab, zlab = settings$zlab,
			col = col
		)
	}
)

