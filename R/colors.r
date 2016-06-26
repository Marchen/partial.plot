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
#'		If missing all character variables and factors are used for making
#'		colors.
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
#'		If \code{unique.pal} is FALSE, character vector representing colors 
#'		with length equal to the length of \code{x}. For this case, the
#'		character vector has "palette" attribute which contains named vector
#'		of colors used for the result.
#'
#'	@export
#'
#'	@examples
#'	# Example 1: set point color based on a character vector.
#'	data(iris)
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16,
#'		col = color.ramp(Species)
#'	)
#'	# Add a legend
#'	col <- color.ramp(iris$Species, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 2)
#'	
#'	# Example 2: use different color function.
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16, 
#'		col = color.ramp(Species, pal = rainbow)
#'	)
#'	col <- color.ramp(iris$Species, pal = rainbow, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 2)
#'
#'	Examle 3: use named color palette.
#'	pal = c(setosa = "blue", versicolor = "red", virginica = "black")
#'	plot(
#'		Sepal.Length ~ Petal.Length, data = iris, pch = 16, 
#'		col = color.ramp(Species, pal = pal)
#'	)
#'	col <- color.ramp(iris$Species, pal = pal, unique.pal = TRUE)
#'	legend("topleft", legend = names(col), col = col, pch = 16, cex = 2)
#'
#'	# Example 4: using data.frame.
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
	if (!is.function(pal)) {
		if (length(unique(x)) != length(pal) & length(pal) != 1){
			stop("Length of pal must be same as length(unique(x)).")
		}
	}
	UseMethod("color.ramp", x)
}


#-------------------------------------------------------------------------------
#'	@describeIn color.ramp
#'	default S3 method.
#'	@export
#-------------------------------------------------------------------------------
color.ramp.default <- function(x, pal = gg.colors, ..., unique.pal = FALSE){
	if (is.null(x)){
		return("black")
	}
	fn <- if (is.factor(x)) levels else unique
	if (is.function(pal)){
		pal <- pal(length(sort(fn(x))), ...)
		names(pal) <- sort(fn(x))
	}
	if (length(pal) == 1) {
		pal <- rep(pal, length(fn(x)))
		names(pal) <- sort(fn(x))
	}
	if (unique.pal){
		return(pal)
	} else {
	   	if (!is.null(names(pal))) {
	   		result <- pal[as.character(x)]
	   	} else {
	   		result <- pal[as.numeric(as.factor(x))]
	   	}
		attr(result, "palette") <- pal
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
	x, factor.names, pal = gg.colors, ..., unique.pal = TRUE
) {
	if (missing(factor.names)) {
		factor.names <- colnames(x)[
			sapply(x, is.factor) | sapply(x, is.character)
		]
	}
	combinations <- expand.grid(get.unique.factors(x, factor.names))
	combinations <- combine.columns(combinations)
	col <- color.ramp.default(combinations, pal)
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
#'	# Example 1: change alpha.
#'	col1 <- "red"
#'	col2 <- trans.color(col1, alpha = 0.1, mix = "white", ratio = 0.7)
#'	col3 <- trans.color(col1, alpha = 0.5, mix = "white", ratio = 0.7)
#'	col4 <- trans.color(col1, alpha = 0.8, mix = "white", ratio = 0.7)
#'	barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'	# Example 2: mix with black.
#'	col1 <- "red"
#'	col2 <- trans.color(col1, alpha = 0.1, mix = "black", ratio = 0.7)
#'	col3 <- trans.color(col1, alpha = 0.5, mix = "black", ratio = 0.7)
#'	col4 <- trans.color(col1, alpha = 0.8, mix = "black", ratio = 0.7)
#'	barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'	# Example 3: change ratio.
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




