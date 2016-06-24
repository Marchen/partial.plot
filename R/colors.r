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
#' @examples
#'	barplot(1:30, col = gg.colors(30))
#-------------------------------------------------------------------------------
gg.colors <- function(n){
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}


