#-------------------------------------------------------------------------------
#	ggplot風の色を返す
#
#	Args:
#		n: 色の数
#-------------------------------------------------------------------------------
#'	Make ggplot like colors.
#'
#'	@param number of colors to make.
#'
#'	@return a character vector containing colors.
#'	@export
#'
#'	@examples
#'	barplot(1:30, col = gg.colors(30))
#-------------------------------------------------------------------------------
gg.colors <- function(n){
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}


#-------------------------------------------------------------------------------
#	カラーパレットを作成する。
#-------------------------------------------------------------------------------
#'	(Internal) Make color palette.
#'
#'	@param pal a color making function or character vector.
#'	@param x a vector of values that colors are made based on it.
#'	@param ... other parameters passed to color function.
#'
#'	@return
#'		If \code{x} is NULL, a non-named character vector of length 1.
#'		Otherwise, a named vector of colors with unique values in \code{x} for
#'		the name of it.
#-------------------------------------------------------------------------------
make.palette <- function(pal, x, ...) {
	UseMethod("make.palette")
}


#-------------------------------------------------------------------------------
#'	@describeIn make.palette
#'	Default S3 method, intended to be used for character vector.
#'	@method make.palette default
#-------------------------------------------------------------------------------
make.palette.default <- function(pal, x, ...) { 
	# If x is NULL, return not named vector of color palette
	if (is.null(x)) {
		if (length(pal) == 1) {
			names(pal) <- NULL
			return(pal)
		}
		stop("'pal' should be of length 1 if 'x' is NULL and 'pal' is character.")
	}
	# Define function retrieving unique values.
	get.unique <- if (is.factor(x)) levels else unique
	if (!is.null(names(pal))) {
		# If pal is a named vector, check all unique values of x exists.
		if (!identical(sort(names(pal)), sort(get.unique(x)))) {
			stop("'pal' should have values for all unique values in x")
		}
		return(pal)
	}
	if (length(pal) == 1) {
		# If pal is a non-named vector of length 1, repeat same color.
		pal <- rep(pal, length(get.unique(x)))
	} else {
		# If pal is a non-named vector of length > 1, use first colors.
		if (length(pal) < length(get.unique(x))) {
			stop("'pal' should have larger length than length of unique values in 'x'")
		}
		pal <- pal[1:length(get.unique(x))]
	}
	names(pal) <- sort(get.unique(x))
	return(pal)
}


#-------------------------------------------------------------------------------
#'	@describeIn make.palette
#'	S3 method for function.
#'	@method make.palette function
#-------------------------------------------------------------------------------
make.palette.function <- function(pal, x, ...) {
	# If x is NULL, return not named vector of color palette
	if (is.null(x)) {
		return(pal(1, ...))
	}
	# Define function retrieving unique values.
	get.unique <- if (is.factor(x)) levels else unique
	# If pal is a function, apply pal to make colors.
	pal <- pal(length(sort(get.unique(x))), ...)
	names(pal) <- sort(get.unique(x))
	return(pal)
}


#-------------------------------------------------------------------------------
#	plotのラベルに色をつけられるように色のベクトルを返す。
#
#	Args:
#		x: 
#			データ（今のところ、因子型と文字列型はうまく行くはず）。
#			将来的には数値型に対応したい。
#		pal:
#			カラーパレットを生成する関数。
#			もしくはカラーパレットを表す文字列ベクトル。
#			文字列ベクトルの場合、xの種類と長さが一致する必要あり。
#		...: 
#			色生成関数に渡される引数。
#		unique.pal:
#			これがTRUEなら色一覧を、FALSEならxと同じ長さの色のベクトルを返す。
#-------------------------------------------------------------------------------
#'	Make a color vector based on data.
#'
#'	@param x
#'		a vector of character or factor. 
#'		Based on this vector, color vector is generated.
#'
#'	@param factor.names
#'		a character vector specifying names of factors by which colors are made.
#'		If missing or length of factor.names is 0, all character variables and
#'		factors are used for making colors.
#'
#'	@param pal
#'		a function or character vector.
#'		If a function is specified for \code{pal}, \code{color.ramp} assumes
#'		that the function accepts number of params for the first argument
#'		 (\code{n}) and produces a charactor vector representing colors.
#'		e.g. \code{\link{gg.colors}}, \code{\link[grDevices]{rainbow}}, 
#'		\code{\link[grDevices]{terrain.colors}}, and 
#'		\code{\link[grDevices]{topo.colors}} can be used.
#'		If a character vector of length 1 is specified for \code{pal},
#'		\code{color.ramp} produces color vector with the single color.
#'		If a character vector of length equal to the number of unique values 
#'		of x is specified, this function assigns each color for each value of
#'		\code{x}.
#'
#'	@param unique.pal
#'		a logical determining return value of the function.
#'
#'	@return
#'		If \code{unique.pal} is TRUE, unique named character vector of colors.
#'		For this case, this function returns non-named color vector of 
#'		length 1 if \code{x} is NULL, 
#'		If \code{unique.pal} is FALSE, character vector representing colors 
#'		with length equal to the length of \code{x}. For this case, the
#'		character vector has "palette" attribute which contains named vector
#'		of colors used for the result.

#'
#'	@export
#'
#'	@examples
#'	#---------------------------------------------------------------------------
#'	# Example 1: set point color based on a character vector.
#'	#---------------------------------------------------------------------------
#'	data(iris)
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16,
#'		col = color.ramp(Species)
#'	)
#'	# Add a legend
#'	col <- color.ramp(iris$Species, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#'	
#'	
#'	#---------------------------------------------------------------------------
#'	# Example 2: using different color function.
#'	#---------------------------------------------------------------------------
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16, 
#'		col = color.ramp(Species, pal = rainbow)
#'	)
#'	col <- color.ramp(iris$Species, pal = rainbow, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#'
#'	
#'	#---------------------------------------------------------------------------
#'	# Examle 3: using named color palette.
#'	#---------------------------------------------------------------------------
#'	pal = c(setosa = "blue", versicolor = "red", virginica = "black")
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16, 
#'		col = color.ramp(Species, pal = pal)
#'	)
#'	col <- color.ramp(iris$Species, pal = pal, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 4: using data.frame.
#'	#---------------------------------------------------------------------------
#'	iris2 <- iris
#'	iris2$new.factor <- as.factor(letters[1:2])
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris2, pch = 16,
#'		col = color.ramp(iris2)
#'	)
#'	col <- color.ramp(iris2, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#-------------------------------------------------------------------------------
color.ramp <- function(x, pal = gg.colors, ..., unique.pal = FALSE) {
	UseMethod("color.ramp")
}


#-------------------------------------------------------------------------------
#'	@describeIn color.ramp
#'	default S3 method.
#'	@export
#-------------------------------------------------------------------------------
color.ramp.default <- function(x, pal = gg.colors, ..., unique.pal = FALSE){
	palette <- make.palette(pal, x, ...)
	if (unique.pal){
		return(palette)
	} else {
	   	if (!is.null(names(palette))) {
	   		result <- palette[as.character(x)]
	   	} else {
	   		result <- palette[as.numeric(as.factor(x))]
	   	}
		attr(result, "palette") <- palette
	}
	return(result)
}


#-------------------------------------------------------------------------------
#'	@describeIn color.ramp
#'	method for data.frame.
#'	@export
#'	@method color.ramp data.frame
#-------------------------------------------------------------------------------
color.ramp.data.frame <- function(
	x, factor.names, pal = gg.colors, ..., unique.pal = FALSE
) {
	if (missing(factor.names) | length(factor.names) == 0) {
		factor.names <- colnames(x)[
			sapply(x, is.factor) | sapply(x, is.character)
		]
	}
	if (!all(factor.names %in% colnames(x))) {
		stop("Some factors specified in 'factor.names' are not present in 'x'")	
	}
	combinations <- combine.columns(x[factor.names])
	col <- color.ramp.default(
		combinations, pal = pal, ..., unique.pal = unique.pal
	)
	return(col)
}


#-------------------------------------------------------------------------------
#	透明色を作る。
#	Args:
#		colors: 基本になる色。
#		alpha: 透明度。
#		mix: 基本になる色と混ぜる色。
#		ratio: 基本色と混合色を混ぜる割合。
#-------------------------------------------------------------------------------
#'	Make transparent colors.
#'
#'	@param colors a character vector representing base colors.
#'	@param alpha a numeric value between 0 to 1 representing transparency.
#'	@param mix 
#'		a character literal of color represent a color. The base colors
#'		are mixed with this color.
#'	@param ratio
#'		a numeric value between 0 to 1 representing mixture ratio of the
#'		base colors and mix color.
#'
#'	@return a character vector of transparent colors.
#'	@export
#'
#'	@examples
#'	#---------------------------------------------------------------------------
#'	# Example 1: change alpha.
#'	#---------------------------------------------------------------------------
#'	col1 <- "red"
#'	col2 <- trans.color(col1, alpha = 0.1, mix = "white", ratio = 0.7)
#'	col3 <- trans.color(col1, alpha = 0.5, mix = "white", ratio = 0.7)
#'	col4 <- trans.color(col1, alpha = 0.8, mix = "white", ratio = 0.7)
#'	barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 2: mix with black.
#'	#---------------------------------------------------------------------------
#'	col1 <- "red"
#'	col2 <- trans.color(col1, alpha = 0.1, mix = "black", ratio = 0.7)
#'	col3 <- trans.color(col1, alpha = 0.5, mix = "black", ratio = 0.7)
#'	col4 <- trans.color(col1, alpha = 0.8, mix = "black", ratio = 0.7)
#'	barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'
#'	#---------------------------------------------------------------------------
#'	# Example 3: change ratio.
#'	#---------------------------------------------------------------------------
#'	col1 <- "red"
#'	col2 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.1)
#'	col3 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.4)
#'	col4 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.9)
#'	barplot(1:4, col = c(col1, col2, col3, col4))
#-------------------------------------------------------------------------------
trans.color <- function(colors, alpha = 0.3, mix = "white", ratio = 0.7) {
	# Error checks / エラーチェック。
	if (length(mix) != 1) {
		stop("Length of 'mix' should be 1.")
	}
	if (all(ratio < 0 | ratio > 1)) {
		stop("'ratio' should be between 0 and 1.")
	}
	if (all(alpha < 0 | alpha > 1)) {
		stop("'alpha' should be between 0 and 1.")
	}
	# Convert colors / 色を変換。
	col1 <- col2rgb(colors) * ratio
	col2 <- col2rgb(mix) * (1 - ratio)
	col <- apply(col2, 2, "+", col1)
	col <- rgb(t(col), max = 255, alpha = 255 * alpha)
	return(col)
}




