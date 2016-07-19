#-------------------------------------------------------------------------------
#	partial.plotのレジェンドを描画するクラス。
#-------------------------------------------------------------------------------
#'	(Internal) A reference class to make legend of partial.plot
#'
#'	This reference class keeps information used for partial.plot.
#'	Currently this class is only used for making legend.
#'	This class is not intended to be used by 
#'	users directly.
#'
#'	@field col
#'		a named character vector representing names and colors of partial.plot.
#'	@field title
#'		a character representing title of the legend.
#'	@field draw.residuals
#'		a logical representing that partial.plot drew residual points or not.
#'	@field draw.relationships
#'		a logical representing that partial.plot drew predicted relationships
#'		or not.
#'	@field other.pars
#		a list containing other graphic parameters passed to partial.plot().
#-------------------------------------------------------------------------------
pp.info <- setRefClass(
	"pp.info",
	fields = list(
		col = "character",
		title = "character",
		draw.residuals = "logical",
		draw.relationships = "logical",
		other.pars = "list"
	)
)

pp.info$methods(
	initialize = function(col, title, draw.residuals, draw.relationships, ...) {
		"
		Initialize pp.info object.
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


pp.info$methods(
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


