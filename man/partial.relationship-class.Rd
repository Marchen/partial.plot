% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partial.relationship.r
\docType{class}
\name{partial.relationship-class}
\alias{partial.relationship-class}
\alias{partial.relationship}
\title{(Internal) A reference class calculates partial relationship.}
\description{
(Internal) A reference class calculates partial relationship.
}
\section{Fields}{

\describe{
\item{\code{data}}{data.frame to keep calculated partial relationship data.}

\item{\code{data.split}}{a list having splitted data.frame for each group specified in x.names.}

\item{\code{settings}}{class settings.}
}}

\section{Methods}{

\describe{
\item{\code{calculate.relationship(grid)}}{Calculate partial relationship.
\describe{
    \item{\code{grid}}{
        explanatory variables where predicted values are calculated.
    }
}}

\item{\code{filter.result(prediction)}}{        Remove out-ranged values from result of lsmeans.

        This internal function removes predicted values those explanatory
        variable is out of range of original data used for modeling for each
        group and returns a resultant data.frame.

        \describe{
            \item{prediction}{result of lsmeans.}
        }
        }

\item{\code{initialize(settings)}}{        Initialize class and calculate partial.relationship data.

        \describe{
            \item{settings}{pp.settings object having function settings.}
        }
        }

\item{\code{partial.relationship.internal()}}{Calculate partial relationship data internally.
Mainly used for machine learning methods.}

\item{\code{partial.relationship.lsmeans()}}{        Calculate partial regression lines and intervals.

        This function calculates data for regression lines and intervals using
        \code{\link[emmeans]{lsmeans}} function and returns a data.frame.
        }

\item{\code{predict.stats(newdata, predict.fun, new.value.grid, index, levels, type)}}{        Calculate prediction in cluster.

        \describe{
            \item{\code{newdata}}{newdata for prediction.}
            \item{\code{predict.fun}}{predict function.}
            \item{\code{new.value.grid}}{
                a data.frame having all combination of focal values of
                partial relationship grid.
            }
            \item{\code{index}}{
                row index of \code{new.value.grid} where predicted values
                are calculated.
            }
            \item{\code{levels}}{levels of quantiles.}
        }
        }

\item{\code{summarize.ref.grid(rg)}}{Calculate predictions and intervals using lsmeans.}

\item{\code{summarize.ref.grid.with.mcmc(lsm)}}{Calculate predictions and intervals using lsmeans with mcmc.}

\item{\code{summarize.ref.grid.without.mcmc(lsm)}}{Calculate predictions and intervals using lsmeans without mcmc.}
}}

