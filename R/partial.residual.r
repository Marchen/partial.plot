#------------------------------------------------------------------------------
#   The threshold value of nuber of rows by which calculate.residual() method
#   separates large and small dataset.
#------------------------------------------------------------------------------
THRESHOLD_NROW_FOR_SMALL_DATASET <- 1000


#------------------------------------------------------------------------------
#' (Internal) A reference class calculate partial residuals.
#'
#' @field settings
#'    pp.settings object having settings of the class.
#'
#' @field data
#'    a numeric vector having calculated partial residual data.
#'
#' @include pp.settings.r
#------------------------------------------------------------------------------
partial.residual <- setRefClass(
    "partial.residual",
    fields = list(settings = "pp.settings", data = "numeric")
)


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
            lsmeans.compatible <- !any(
                class(settings$model) %in% LSMEANS_INCOMPATIBLE_MODELS
            )
            if (lsmeans.compatible) {
                .self$calculate.residuals.lsmeans()
            } else {
                .self$calculate.residuals()
            }
            settings$set.residual(.self)
        }
    }
)


#------------------------------------------------------------------------------
#   Assume that creating a model predicts petal length (PL) by petal width (PW)
#   and sepal length (SL) and species (SP) and their interactions.
#       i: intercept
#       r: residual
#
#   PL      = i + PW + SL + SP + PW:SP + SL:SP + r      # Response variable
#   PL      = i + PW + SL + SP + PW:SP + SL:SP          # FULL MODEL
#   PL      =                                  + r      # Residual
#   predict = i + PW + SL + SP + PW:SP + SL:SP          # Result of predict()
#
#   To see the partial relationship between PL and SL...
#
#   If we want to plot contrast plot, partial residual is:
#   PL      = i + PW    0 + SP + PW:SP       0 + r
#
#   To see this, we need to remove:
#   PL      =        + SL              + SL:SP          # Remove these effects
#
#   lsmeans() calculates:
#   LSMEANS = i + MM + SL + SP + MM:SP + SL:SP
#       where MM is mean of petal width (PW).
#
#   So when using lsmeans(), i.e., conditional plot, partial residual is:
#   LSMEANS = i + MM + SL + SP + MM:SP + SL:SP + r
#
#   This can be achieved using predict() with using mean values for
#   petal width (PW) and adding residual for it.
#   However, becase MCMCglmm show deviated values from this calculation,
#   currently partial residual for MCMCglmm (and models compatible with
#   lsmeans) are calculated by special method.
#
#   For non-linear models with interaction such as Random Forest, population
#   predicted values for given value of focal explanatory variable cannot be
#   calculated by fixing values of other explanatory variables to their
#   mean/median.
#   Therefore, the predicted values for given set of values of focal
#   explanatory variable are calculated using all dataset as similar way to
#   calculate partial relationship.
#
#   For performance issue, this method change the method used for calculating
#   fitted value. For small dataset, get.fit.for.small.data() method is called.
#   For large dataset, get.fit.for.large.data() method is called.
#------------------------------------------------------------------------------
partial.residual$methods(
    calculate.residuals = function() {
        "
        Calculate partial residuals.
        "
        # Prepare data for partial residual.
        newdata <- lapply(
            1:nrow(.self$settings$data), "[.data.frame",
            x = .self$settings$data,
        )
        # Change calculation method based on the sample size.
        if (nrow(.self$settings$data) < THRESHOLD_NROW_FOR_SMALL_DATASET) {
            fit <- .self$settings$cluster.apply(
                newdata, .self$get.fit.for.small.data
            )
        } else {
            relationship <- .self$extended.partial.relationship()
            fit <- .self$settings$cluster.apply(
                newdata, .self$get.fit.for.large.data, relationship
            )
        }
        fit <- unlist(fit)
        residual <- fit + .self$settings$adapter$residuals(.self$settings$type)
        if (.self$settings$type == "response") {
            residual <- .self$settings$adapter$linkinv(residual)
        }
        .self$data <- residual
    }
)


#------------------------------------------------------------------------------
partial.residual$methods(
    get.fit.for.small.data = function(x) {
        "
        Calculate fitted value for a observation.

        \\describe{
            \\item{x}{a data.frame of one row representing one observation.}
        }
        "
        on.exit(gc())
        newdata <- .self$settings$data
        for (i in .self$settings$x.names) {
            newdata[[i]] <- x[[i]]
        }
        fit <- .self$settings$adapter$predict(newdata = newdata, type = "link")
        fit <- mean(fit$fit[, "fit"])
        return(fit)
    }
)


#------------------------------------------------------------------------------
partial.residual$methods(
    get.fit.for.large.data = function(x, relationship) {
        "
        Calculate fitted value for a observation.

        This function uses approximation using estimated relationship
        for calculating fitted value.

        \\describe{
            \\item{x}{a data.frame of one row representing one observation.}
            \\item{relationship}{a data.frame having predicted relationship.}
        }
        "
        on.exit(gc())
        for (i in .self$settings$x.names.factor) {
            relationship <- relationship[relationship[[i]] == x[[i]], ]
        }
        index <- relationship[.self$settings$x.names.numeric]
        for (i in .self$settings$x.names.numeric) {
            index[[i]] <- index[[i]] > x[[i]]
        }
        index <- Position(function(x) x, apply(index, 1, all))
        if (length(settings$x.names.numeric) == 1) {
            f <- as.formula(
                sprintf("fit ~ %s", .self$settings$x.names.numeric)
            )
            d <- relationship[c(index - 1, index),]
        } else {
            x.names <- paste(.self$settings$x.names.numeric, collapse = "+")
            f <- as.formula(sprintf("fit ~ %s", x.names))
            res <- .self$settings$resolution
            d <- relationship[
                c(index, index - 1, index - res, index - res - 1),
            ]
        }
        r <- lm(f, data = d)
        fit <- predict(r, newdata = x)
        return(fit)
    }
)


#------------------------------------------------------------------------------
partial.residual$methods(
    extended.partial.relationship = function() {
        sequences <- .self$extend.sequences(.self$settings$numeric.sequences)
        grid <- do.call(
            expand.grid, c(sequences, .self$settings$factor.levels)
        )
        new.settings <- .self$settings$copy()
        pr <- partial.relationship(new.settings)
        result <- pr$calculate.relationship(grid)
        return(result)
    }
)


#------------------------------------------------------------------------------
partial.residual$methods(
    extend.sequences = function(sequences) {
        "
        Return extended 'numeric.sequences' of pp.settings.
        "
        for (i in names(sequences)) {
            len <- length(sequences[[i]])
            new.value <-  2 * sequences[[i]][len] - sequences[[i]][len - 1]
            sequences[[i]] <- c(sequences[[i]], new.value)
        }
        return(sequences)
    }
)


#------------------------------------------------------------------------------
partial.residual$methods(
    calculate.residuals.lsmeans = function() {
        "
        Calculate partial residuals for lsmeans compatible models.
        "
        # Prepare names of numeric variables.
        x.names <- .self$settings$adapter$x.names(type = "base")
        all.numerics <- x.names[
            sapply(.self$settings$data[x.names], is.numeric)
        ]
        other.numerics <- all.numerics[
            !all.numerics %in% .self$settings$x.names.numeric
        ]
        # Calculate prediction 1.
        data1 <- .self$settings$data
        for (i in other.numerics) {
            data1[[i]] <- data1[[i]] - mean(data1[[i]])
        }
        data1[.self$settings$x.names.numeric] <- 0
        pred1 <- .self$settings$adapter$predict(
          newdata = data1, type = "link", interval = "prediction"
        )
        pred1 <- pred1$fit[, "fit"]
        # Calculate prediction 2.
        data2 <- .self$settings$data
        data2[all.numerics] <- 0
        pred2 <- .self$settings$adapter$predict(
          newdata = data2, type = "link", interval = "prediction"
        )
        pred2 <- pred2$fit[, "fit"]
        # Calculate partial residual.
        result <- (
            .self$settings$adapter$link(
                .self$settings$data[[.self$settings$adapter$y.names()]]
            )
            - (pred1 - pred2)
        )
        if (.self$settings$type == "response") {
            result <- .self$settings$adapter$linkinv(result)
        }
        .self$data <- result
    }
)
