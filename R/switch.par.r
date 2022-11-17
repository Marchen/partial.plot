
#------------------------------------------------------------------------------
#' Make ggplot like colors.
#'
#' @param number of colors to make.
#'
#' @return a character vector containing colors.
#' @export
#'
#' @examples
#' barplot(1:30, col = gg.colors(30))
#------------------------------------------------------------------------------
gg.colors <- function(n){
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


#------------------------------------------------------------------------------
#' Make transparent colors.
#'
#' @param colors a character vector representing base colors.
#' @param alpha a numeric value between 0 to 1 representing transparency.
#' @param mix
#'    a character literal of color represent a color. The base colors
#'    are mixed with this color.
#' @param ratio
#'    a numeric value between 0 to 1 representing mixture ratio of the
#'    base colors and mix color.
#'
#' @return a character vector of transparent colors.
#' @export
#'
#' @examples
#' #---------------------------------------------------------------------------
#' # Example 1: change alpha.
#' #---------------------------------------------------------------------------
#' col1 <- "red"
#' col2 <- trans.color(col1, alpha = 0.1, mix = "white", ratio = 0.7)
#' col3 <- trans.color(col1, alpha = 0.5, mix = "white", ratio = 0.7)
#' col4 <- trans.color(col1, alpha = 0.8, mix = "white", ratio = 0.7)
#' barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'
#' #---------------------------------------------------------------------------
#' # Example 2: mix with black.
#' #---------------------------------------------------------------------------
#' col1 <- "red"
#' col2 <- trans.color(col1, alpha = 0.1, mix = "black", ratio = 0.7)
#' col3 <- trans.color(col1, alpha = 0.5, mix = "black", ratio = 0.7)
#' col4 <- trans.color(col1, alpha = 0.8, mix = "black", ratio = 0.7)
#' barplot(1:4, col = c(col1, col2, col3, col4))
#'
#'
#' #---------------------------------------------------------------------------
#' # Example 3: change ratio.
#' #---------------------------------------------------------------------------
#' col1 <- "red"
#' col2 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.1)
#' col3 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.4)
#' col4 <- trans.color(col1, alpha = 0.3, mix = "white", ratio = 0.9)
#' barplot(1:4, col = c(col1, col2, col3, col4))
#------------------------------------------------------------------------------
trans.color <- function(colors, alpha = 0.3, mix = "white", ratio = 0.7) {
    # Error checks
    if (length(mix) != 1) {
        stop("Length of 'mix' should be 1.")
    }
    if (all(ratio < 0 | ratio > 1)) {
        stop("'ratio' should be between 0 and 1.")
    }
    if (all(alpha < 0 | alpha > 1)) {
        stop("'alpha' should be between 0 and 1.")
    }
    # Convert colors
    col1 <- col2rgb(colors) * ratio
    col2 <- col2rgb(mix) * (1 - ratio)
    col <- apply(col2, 2, "+", col1)
    col <- rgb(t(col), max = 255, alpha = 255 * alpha)
    return(col)
}


#------------------------------------------------------------------------------
#' (Internal) Make palette of a graphic parameter.
#'
#' @param pal
#'    a graphic parameter making function or character vector.
#' @param x
#'    a vector of values that the parameter is made based on it.
#' @param ...
#'    other parameters passed to the graphic parameter making function.
#'
#' @return
#'    If \code{x} is NULL, a non-named vector of the graphic parameter of
#'    length 1.
#'    Otherwise, a named vector of graphic parameters with unique values in
#'    \code{x} for the name of it.
#------------------------------------------------------------------------------
make.palette <- function(pal, x, ...) {
    UseMethod("make.palette")
}


#------------------------------------------------------------------------------
#' @describeIn make.palette
#' Default S3 method, intended to be used for character vector.
#' @method make.palette default
#------------------------------------------------------------------------------
make.palette.default <- function(pal, x, ...) {
    # If x is NULL, return not named vector of the graphic parameter.
    if (is.null(x)) {
        if (length(pal) == 1) {
            names(pal) <- NULL
            return(pal)
        }
        stop.with.message(
            "'pal' should be of length 1 if 'x' is NULL",
            "and 'pal' is character."
        )
    }
    # Define function retrieving unique values.
    get.unique <- if (is.factor(x)) levels else unique
    if (!is.null(names(pal))) {
        # If pal is a named vector, check all unique values of x exists.
        if (!identical(sort(names(pal)), sort(get.unique(x)))) {
            stop("'pal' should have values for all unique values in x")
        }
        return(pal[sort(names(pal))])
    }
    if (length(pal) == 1) {
        # If pal is a non-named vector of length 1,
        # repeat same graphic parameter.
        pal <- rep(pal, length(get.unique(x)))
    } else {
        # If pal is a non-named vector of length > 1,
        # use the first graphic parameter.
        if (length(pal) < length(get.unique(x))) {
            stop.with.message(
                "'pal' should have larger length than length of",
                "unique values in 'x'"
            )
        }
        pal <- pal[seq_along(get.unique(x))]
    }
    names(pal) <- sort(get.unique(x))
    return(pal)
}


#------------------------------------------------------------------------------
#' @describeIn make.palette
#' S3 method for function.
#' @method make.palette function
#------------------------------------------------------------------------------
make.palette.function <- function(pal, x, ...) {
    # If x is NULL, return not named vector of the grpahic parameter
    if (is.null(x)) {
        return(pal(1, ...))
    }
    # Define function retrieving unique values.
    get.unique <- if (is.factor(x)) levels else unique
    # If pal is a function, apply pal to make vector of graphic parameters.
    pal <- pal(length(sort(get.unique(x))), ...)
    names(pal) <- sort(get.unique(x))
    return(pal)
}


#------------------------------------------------------------------------------
#' Make a vector of graphic parameters based on data.
#'
#' \code{color.ramp} is an alias of \code{switch.par}, which having different
#' default value for \code{pal} and may enhance readability in some cases.
#'
#' @param x
#'    a vector of character or factor.
#'    Based on this vector, the vector of graphic parameteres is generated.
#'    Making colors for numeric mode of \code{x} is experimentally supported.
#'
#' @param pal
#'    a function or vector.
#'    If a function is specified for \code{pal}, this function assumes
#'    that the function accepts number of params for the first argument
#'     (\code{n}) and produces a vector of graphic parameter.
#'    e.g. \code{\link{gg.colors}}, \code{\link[grDevices]{rainbow}},
#'    \code{\link[grDevices]{terrain.colors}}, and
#'    \code{\link[grDevices]{topo.colors}} can be used.
#'    If a vector of length 1 is specified for \code{pal}, this function
#'    produces vector of graphic parameters consists of same values.
#'    If a vector of length equal to the number of unique values of \code{x}
#'    is specified, this function assigns each value of grpahic parameter for
#'    each value of \code{x}.
#'
#' @param unique.pal
#'    a logical determines return value of the function.
#'
#' @return
#'    If \code{unique.pal} is TRUE, this function returns unique named vector
#'    of graphic parameters. For this case, this function returns non-named
#'    vector of graphic parameter with length 1 if \code{x} is NULL.
#'    If \code{unique.pal} is FALSE, this function returns vector of graphic
#'    parameters with length equal to the length of \code{x}. For this case,
#'    the vector has "palette" attribute which contains named vector
#'    of graphic parameters used for the result.
#'
#' @export
#'
#' @examples
#' #---------------------------------------------------------------------------
#' # Example 1: set point color and character based on a character vector.
#' #---------------------------------------------------------------------------
#' data(iris)
#' plot(
#'     Sepal.Length ~ Petal.Length, data = iris,
#'     col = color.ramp(Species),
#'     pch = switch.par(Species)
#' )
#' # Add a legend
#' col <- color.ramp(iris$Species, unique.pal = TRUE)
#' pch <- switch.par(iris$Species, unique.pal = TRUE)
#' legend("topleft", legend = names(col), col = col, pch = pch, cex = 1)
#'
#'
#' #---------------------------------------------------------------------------
#' # Example 2: using different color function.
#' #---------------------------------------------------------------------------
#' plot(
#'     Sepal.Length ~ Petal.Length, data = iris, pch = 16,
#'     col = color.ramp(Species, pal = rainbow)
#' )
#' col <- color.ramp(iris$Species, pal = rainbow, unique.pal = TRUE)
#' legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#'
#'
#' #---------------------------------------------------------------------------
#' # Examle 3: using named color palette.
#' #---------------------------------------------------------------------------
#' pal = c(setosa = "blue", versicolor = "red", virginica = "black")
#' plot(
#'     Sepal.Length ~ Petal.Length, data = iris, pch = 16,
#'     col = color.ramp(Species, pal = pal)
#' )
#' col <- color.ramp(iris$Species, pal = pal, unique.pal = TRUE)
#' legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#'
#'
#' #---------------------------------------------------------------------------
#' # Example 4: using data.frame.
#' #---------------------------------------------------------------------------
#' iris2 <- iris
#' iris2$new.factor <- as.factor(letters[1:2])
#' plot(
#'     Sepal.Length ~ Petal.Length, data = iris2, pch = 16,
#'     col = color.ramp(iris2)
#' )
#' col <- color.ramp(iris2, unique.pal = TRUE)
#' legend("topleft", legend = names(col), col = col, pch = 16, cex = 1)
#------------------------------------------------------------------------------
switch.par <- function(
    x, pal = function(n, ...) 1:n, ..., unique.pal = FALSE
) {
    UseMethod("switch.par")
}


#------------------------------------------------------------------------------
#' Alias of \code{switch.par}.
#' @export
#------------------------------------------------------------------------------
color.ramp <- function(x, pal = gg.colors, ..., unique.pal = FALSE) {
    switch.par(x = x, pal = pal, ..., unique.pal = unique.pal)
}


#------------------------------------------------------------------------------
#' @describeIn switch.par default S3 method.
#' @export
#------------------------------------------------------------------------------
switch.par.default <- function(
    x, pal = function(n, ...) 1:n, ..., unique.pal = FALSE
) {
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


#------------------------------------------------------------------------------
#' @describeIn switch.par method for numeric.
#'
#' @param n.class
#'    number of groups. If NULL, n.class become number of unique values
#'    in x.
#' @param method method of grouping. Currently, only "quantile" is supported.
#' @export
#------------------------------------------------------------------------------
switch.par.numeric <- function(
    x, pal = gg.colors, n.class = NULL,
    method = "quantile", ..., unique.pal = FALSE
) {
    # If pal is not a function, make color making function.
    if (!is.function(pal)) {
        pal <- colorRampPalette(pal)
    }
    # If n.class is null, n.class become number of unique values in x.
    if (is.null(n.class)) {
        n.class <- length(unique(x))
    }
    # Assign color.
    quantiles <- quantile(x, probs = seq(0, 1, 1 / n.class), na.rm = TRUE)
    groups <- cut(x, breaks = unique(quantiles), include.lowest = TRUE)
    palette <- make.palette(pal, groups)
    if (unique.pal) {
        return(palette)
    }
    col <- palette[as.numeric(groups)]
    attr(col, "palette") <- palette
    return(col)
}
