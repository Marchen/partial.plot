#------------------------------------------------------------------------------
#	偏残差を計算するクラス。
#------------------------------------------------------------------------------
#'	(Internal) A reference class calculate partial residuals.
#'
#'	@field settings
#'		pp.settings object having settings of the class.
#'
#'	@field data
#'		a numeric vector having calculated partial residual data.
#'
#'	@include pp.settings.r
#------------------------------------------------------------------------------
partial.residual <- setRefClass(
	"partial.residual",
	fields = list(settings = "pp.settings", data = "numeric")
)


#------------------------------------------------------------------------------
#	偏残差計算クラスを初期化する。
#------------------------------------------------------------------------------
partial.residual$methods(
	initialize = function(settings) {
		"
		Initialize class and calculate residuals.

		\\describe{
			\\item{\\code{settings}}{\\code{\\link{pp.settings}} object.}
		}
		"
		# Initialize settings.
		if (missing(settings)) {
			return()
		}
		initFields(settings = settings)
		# Calculate partial residual if possible.
		if (identical(settings$adapter$link, binomial()$linkfun)) {
			message <- paste(
				"Currently, partial residual can't be calculated",
				"for logit link function."
			)
			warning(message)
		} else {
			.self$calculate.residuals()
			settings$set.residual(.self)
		}
	}
)


#------------------------------------------------------------------------------
#	偏残差を計算する。
#------------------------------------------------------------------------------
partial.residual$methods(
	calculate.residuals = function() {
		"
		Calculate partial residuals.
		"
		# Prepare data for partial residual.
		newdata <- settings$data
		for (i in settings$adapter$x.names(type = "base")) {
			if (is.numeric(newdata[[i]]) & !i %in% settings$x.names.numeric) {
				newdata[[i]] <- mean(newdata[[i]], na.rm = TRUE)
			}
		}
		# Calculate partial residual.
		pred <- settings$adapter$predict(newdata = newdata, type = "link")
		residual <- pred$fit[, "fit"] + settings$adapter$residuals("link")
		if (settings$type == "response") {
			residual <- settings$adapter$linkinv(residual)
		}
		.self$data <- residual
	}
)



