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
}


#-------------------------------------------------------------------------------
#'	(Internal) Make color vector based on multiple factors.
#'
#'	@param data a data.frame containing factors.
#'	@param factor.names 
#'		a character vector of names of columns used for color making.
#'	@param pal
#'		a function or character vector.
#'		For the detail, see \code{\link{color.ramp}}.
#'	@param sep
#'		a character literal used for separator of factors.
#'	@return
#'		same as \code{\link{color.ramp}}.
#-------------------------------------------------------------------------------
#	複数の因子から色を作成する。
#-------------------------------------------------------------------------------
brew.colors <- function(data, factor.names, pal = gg.colors, sep = ".") {
	# Make combinations of factors / 因子の組み合わせを作成。
	combinations <- factor.combinations(data, factor.names)
	combinations <- apply(
		sapply(combinations, as.character), 1, paste, collapse = sep
	)
	# Make colors / 色を作成。
	col <- color.ramp(combinations, pal)
	attr(col, "palette") <- color.ramp(combinations, pal, unique.pal = TRUE)
	return(col)
}

