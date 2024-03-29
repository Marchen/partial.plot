#------------------------------------------------------------------------------
#' Draw legend of partial plot.
#'
#' @param settings
#'    \code{\link{pp.settings}} object resulted by\code{\link{partial.plot}}.
#' @param x
#'    position of the legend. For the detail, see
#'    \code{\link[grahpic]{legend}} function.
#'
#' @param ...
#'    other graphic parameters passed to \code{\link[grahpic]{legend}}
#'    function.
#'
#' @export
#'
#' @examples
#'
#' data(iris)
#' model <- lm(
#'     Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
#' )
#' info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
#' partial.plot.legend(info, "topleft")
#------------------------------------------------------------------------------
pp.legend <- function(settings, x, ...) {
    if (!is(settings, "pp.settings")) {
        stop("'object' should be an instance of 'pp.settings' class")
    }
    # Prepare a list containing arguments specified in '...' named as if
    # they are arguments of legend().
    call <- match.call()
    call$settings <- NULL
    call <- match.call(legend, call)
    call <- as.list(call)
    call[[1]] <- NULL
    # Make arguments for legend().
    legend.args <- prepare.args.for.legend(settings, as.list(call))
    legend.args <- settings$set.function.args(legend.args, legend)
    do.call(legend, legend.args)
}


#------------------------------------------------------------------------------
#' (Internal) Prepare arguments used for legend.
#'
#' This function overwrite arguments of legend() that users did not specify
#' manually with default value.
#'
#' @param settings
#'    an object of \code{\link{pp.settings}} object having settings of
#'    partial.plot.
#' @param legend.args
#'    a list containing arguments used for \code{\link[grahpic]{legend}}
#'    function manually specified by users.
#'
#' @return
#'    A list containing arguments used for \code{\link[grahpic]{legend}}
#'    function
#------------------------------------------------------------------------------
prepare.args.for.legend = function(settings, legend.args) {
    # Prepare arguments of legend() used in partial.plot().
    args.from.partial.plot <- c(
        settings$parman$legend.pars(),
        list(title = paste0(settings$x.names.factor, collapse = settings$sep))
    )
    args.from.partial.plot <- c(args.from.partial.plot, settings$other.pars)
    # Set line type based on the setting of partial.plot.
    if (settings$draw.relationship) {
        if (is.null(args.from.partial.plot$lty)) {
            args.from.partial.plot$lty <- "solid"
        }
    } else {
        args.from.partial.plot$lty <- NULL
    }
    # Set plot character based on the setting of partial.plot.
    if (settings$draw.residual) {
        if (is.null(args.from.partial.plot$pch)) {
            args.from.partial.plot$pch <- 1
        }
    } else {
        args.from.partial.plot$pch <- NULL
    }
    # Join arguments specified by user and those from partial.plot.
    for (i in names(args.from.partial.plot)) {
        if (is.null(legend.args[[i]])) {
            legend.args[[i]] <- args.from.partial.plot[[i]]
        }
    }
    return(legend.args)
}
