#------------------------------------------------------------------------------
#' (Internal) A reference class to handle graphic pars used for plotting.
#'
#' @field group
#'    a character vector of group names made from factors specified by
#'    x.names.
#'
#' @field col,lty,lwd,pch
#'    a vector of graphic parameters or a function generate graphic
#'    parameters. For the detail, see \code{\link{switch.par}}.
#------------------------------------------------------------------------------
par.manager <- setRefClass(
    "par.manager",
    fields = list(
        group = "character",
        col = "ANY",
        lty = "ANY",
        lwd = "numeric",
        pch = "ANY"
    )
)


#------------------------------------------------------------------------------
par.manager$methods(
    initialize = function(group, col, lty, lwd, pch, ...) {
        "Initialize class and set group field."
        # Initialize fields.
        if (missing(group)) {
            return()
        }
        initFields(group = group, col = col, lty = lty, lwd = lwd, pch = pch)
    }
)


#------------------------------------------------------------------------------
par.manager$methods(
    par.names = function() {
        "Returns implimented par names"
        pars <- names(as.list(args(.self$initialize)))
        pars <- pars[!pars %in% c("group", "...", "")]
        return(pars)
    }
)


#------------------------------------------------------------------------------
par.manager$methods(
    par.group = function(group.name = NULL) {
        "Make a list of graphic parameters for each group."
        pars <- list()
        for (i in .self$par.names()) {
            pars[[i]] <- switch.par(
                .self$group, pal = .self[[i]], unique.pal = TRUE
            )
        }
        if (!is.null(group.name)) {
            pars <- lapply(pars, function(x) x[group.name])
        }
        return(pars)
    }
)


#------------------------------------------------------------------------------
par.manager$methods(
    par.obs = function() {
        "Make a list of graphic parameters for each observation."
        pars <- list()
        for (i in .self$par.names()) {
            pars[[i]] <- switch.par(.self$group, pal = .self[[i]])
        }
        return(pars)
    }
)


#------------------------------------------------------------------------------
par.manager$methods(
    colors.for.persp = function(z.matrix) {
        "
        Make color vector used for \\code{\\link[graphics]{persp}} funciton.
        \\describe{
            \\item{\\code{z.matrix}}{
                a matrix used for \\code{z} option of \\code{persp}.
            }
        }
        "
        m <- matrix(nrow = nrow(z.matrix) - 1, ncol = ncol(z.matrix) - 1)
        for (row in 1:(nrow(z.matrix) - 1)) {
            for (column in 1:(nrow(z.matrix) - 1)) {
                m[row, column] <- mean(
                    z.matrix[row:(row + 1), column:(column + 1)]
                )
            }
        }
        color <- color.ramp(m, .self$col)
        return(color)
    }
)


#------------------------------------------------------------------------------
par.manager$methods(
    legend.pars = function() {
        "Prepare pars for \\code{\\link{pp.legend}} function."
        pars <- .self$par.group()
        pars$legend = names(pars$col)
        return(pars)
    }
)
