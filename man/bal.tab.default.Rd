% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bal.tab.default.R
\name{bal.tab.default}
\alias{bal.tab.default}
\title{Balance Statistics for Other Objects}
\usage{
\method{bal.tab}{default}(
  x,
  stats,
  int = FALSE,
  poly = 1,
  distance = NULL,
  addl = NULL,
  data = NULL,
  continuous,
  binary,
  s.d.denom,
  thresholds = NULL,
  weights = NULL,
  cluster = NULL,
  imp = NULL,
  pairwise = TRUE,
  s.weights = NULL,
  abs = FALSE,
  subset = NULL,
  quick = TRUE,
  ...
)
}
\arguments{
\item{x}{An object containing information about conditioning. See Details.}

\item{stats}{\code{character}; which statistic(s) should be reported. See \code{\link[=balance-statistics]{stats}} for allowable options. For binary and multi-category treatments, \code{"mean.diffs"} (i.e., mean differences) is the default. For continuous treatments, \code{"correlations"} (i.e., treatment-covariate Pearson correlations) is the default. Multiple options are allowed.}

\item{int}{\code{logical} or \code{numeric}; whether or not to include 2-way interactions of covariates included in \code{covs} and in \code{addl}. If \code{numeric}, will be passed to \code{poly} as well.}

\item{poly}{\code{numeric}; the highest polynomial of each continuous covariate to display. For example, if 2, squares of each continuous covariate will be displayed (in addition to the covariate itself); if 3, squares and cubes of each continuous covariate will be displayed, etc. If 1, the default, only the base covariate will be displayed. If \code{int} is numeric, \code{poly} will take on the value of \code{int}.}

\item{distance}{an optional formula or data frame containing distance values (e.g., propensity scores) or a character vector containing their names. If a formula or variable names are specified, \code{bal.tab()} will look in the argument to \code{data}, if specified. For longitudinal treatments, can be a list of allowable arguments, one for each time point.}

\item{addl}{an optional formula or data frame containing additional covariates for which to present balance or a character vector containing their names. If a formula or variable names are specified, \code{bal.tab()} will look in the arguments to the input object, \code{covs}, and \code{data}, if specified. For longitudinal treatments, can be a list of allowable arguments, one for each time point.}

\item{data}{an optional data frame containing variables named in other arguments. For some input object types, this is required.}

\item{continuous}{whether mean differences for continuous variables should be standardized (\code{"std"}) or raw (\code{"raw"}). Default \code{"std"}. Abbreviations allowed. This option can be set globally using \code{\link[=set.cobalt.options]{set.cobalt.options()}}.}

\item{binary}{whether mean differences for binary variables (i.e., difference in proportion) should be standardized (\code{"std"}) or raw (\code{"raw"}). Default \code{"raw"}. Abbreviations allowed. This option can be set globally using \code{\link[=set.cobalt.options]{set.cobalt.options()}}.}

\item{s.d.denom}{\code{character}; how the denominator for standardized mean differences should be calculated, if requested. See \code{\link[=col_w_smd]{col_w_smd()}} for allowable options. If weights are supplied, each set of weights should have a corresponding entry to \code{s.d.denom}. Abbreviations allowed. If left blank and weights, subclasses, or matching strata are supplied, \code{bal.tab()} will figure out which one is best based on the \code{estimand}, if given (for ATT, \code{"treated"}; for ATC, \code{"control"}; otherwise \code{"pooled"}) and other clues if not.}

\item{thresholds}{a named vector of balance thresholds, where the name corresponds to the statistic (i.e., in \code{stats}) that the threshold applies to. For example, to request thresholds on mean differences and variance ratios, one can set \code{thresholds = c(m = .05, v = 2)}. Requesting a threshold automatically requests the display of that statistic. When specified, extra columns are inserted into the Balance table describing whether the requested balance statistics exceeded the threshold or not. Summary tables tallying the number of variables that exceeded and were within the threshold and displaying the variables with the greatest imbalance on that balance measure are added to the output.}

\item{weights}{a vector, list, or \code{data.frame} containing weights for each unit, or a string containing the names of the weights variables in \code{data}, or an object with a \code{\link[=get.w]{get.w()}} method or a list thereof. The weights can be, e.g., inverse probability weights or matching weights resulting from a matching algorithm.}

\item{cluster}{either a vector containing cluster membership for each unit or a string containing the name of the cluster membership variable in \code{data} or the input object. See \code{\link{class-bal.tab.cluster}} for details.}

\item{imp}{either a vector containing imputation indices for each unit or a string containing the name of the imputation index variable in \code{data} or the input object. See \code{\link{class-bal.tab.imp}} for details. Not necessary if \code{data} is a \code{mids} object.}

\item{pairwise}{whether balance should be computed for pairs of treatments or for each treatment against all groups combined. See \code{\link[=class-bal.tab.multi]{bal.tab.multi()}} for details. This can also be used with a binary treatment to assess balance with respect to the full sample.}

\item{s.weights}{Optional; either a vector containing sampling weights for each unit or a string containing the name of the sampling weight variable in \code{data}. These function like regular weights except that both the adjusted and unadjusted samples will be weighted according to these weights if weights are used.}

\item{abs}{\code{logical}; whether displayed balance statistics should be in absolute value or not.}

\item{subset}{a \code{logical} or \code{numeric} vector denoting whether each observation should be included or which observations should be included. If \code{logical}, it should have length equal to the number of units. \code{NA}s will be treated as \code{FALSE}. This can be used as an alternative to \code{cluster} to examine balance on subsets of the data.}

\item{quick}{\code{logical}; if \code{TRUE}, will not compute any values that will not be displayed. Set to \code{FALSE} if computed values not displayed will be used later.}

\item{...}{other arguments that would be passed to \code{\link[=bal.tab.formula]{bal.tab.formula()}}, \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}, or \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}. See Details.}
}
\value{
For point treatments, if clusters and imputations are not specified, an object of class \code{"bal.tab"} containing balance summaries for the specified treatment and covariates. See \code{\link[=bal.tab]{bal.tab()}} for details.

If clusters are specified, an object of class \code{"bal.tab.cluster"} containing balance summaries within each cluster and a summary of balance across clusters. See \code{\link{class-bal.tab.cluster}} for details.

If imputations are specified, an object of class \code{"bal.tab.imp"} containing balance summaries for each imputation and a summary of balance across imputations, just as with clusters. See \code{\link{class-bal.tab.imp}} for details.

If multi-category treatments are used, an object of class \code{"bal.tab.multi"} containing balance summaries for each pairwise treatment comparison and a summary of balance across pairwise comparisons. See \code{\link[=class-bal.tab.multi]{bal.tab.multi()}} for details.

If longitudinal treatments are used, an object of class \code{"bal.tab.msm"} containing balance summaries at each time point. Each balance summary is its own \code{bal.tab} object. See \code{\link{class-bal.tab.msm}} for more details.
}
\description{
Generates balance statistics using an object for which there is not a defined method.
}
\details{
\code{bal.tab.default()} processes its input and attempt to extract enough information from it to display covariate balance for \code{x}. The purpose of this method is to allow users who have created their own objects containing conditioning information (i.e., weights, subclasses, treatments, covariates, etc.) to access the capabilities of \code{bal.tab()} without having a special method written for them. By including the correct items in \code{x}, \code{bal.tab.default()} can present balance tables as if the input was the output of one of the specifically supported packages (e.g., \pkg{MatchIt}, \pkg{twang}, etc.).

The function will search \code{x} for the following named items and attempt to process them:
\describe{
\item{\code{treat}}{A vector (\code{numeric}, \code{character}, \code{factor}) containing the values of the treatment for each unit or the name of the column in \code{data} containing them. Essentially the same input to \code{treat} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{treat.list}}{A list of vectors (\code{numeric}, \code{character}, \code{factor}) containing, for each time point, the values of the treatment for each unit or the name of the column in \code{data} containing them. Essentially the same input to \code{treat.list} in \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{covs}}{A \code{data.frame} containing the values of the covariates for each unit. Essentially the same input to \code{covs} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{covs.list}}{A list of \code{data.frame}s containing, for each time point, the values of the covariates for each unit. Essentially the same input to \code{covs.list} in \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{formula}}{A \code{formula} with the treatment variable as the response and the covariates for which balance is to be assessed as the terms. Essentially the same input to \code{formula} in \code{\link[=bal.tab.formula]{bal.tab.formula()}}.
}
\item{\code{formula.list}}{A list of \code{formula}s with, for each time point, the treatment variable as the response and the covariates for which balance is to be assessed as the terms. Essentially the same input to \code{formula.list} in \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{data}}{A \code{data.frame} containing variables with the names used in other arguments and components (e.g., \code{formula}, \code{weights}, etc.). Essentially the same input to \code{data} in \code{\link[=bal.tab.formula]{bal.tab.formula()}}, \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}, or \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{weights}}{A vector, list, or \code{data.frame} containing weights for each unit or a string containing the names of the weights variables in \code{data}. Essentially the same input to \code{weights} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}} or \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{distance}}{
A vector, formula, or data frame containing distance values (e.g., propensity scores) or a character vector containing their names. If a formula or variable names are specified, \code{bal.tab()} will look in the argument to \code{data}, if specified. Essentially the same input to \code{distance} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{formula.list}}{A list of vectors or \code{data.frame}s containing, for each time point, distance values (e.g., propensity scores) for each unit or a string containing the name of the distance variable in \code{data}. Essentially the same input to \code{distance.list} in \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{subclass}}{A vector containing subclass membership for each unit or a string containing the name of the subclass variable in \code{data}. Essentially the same input to \code{subclass} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{match.strata}}{A vector containing matching stratum membership for each unit or a string containing the name of the matching stratum variable in \code{data}. Essentially the same input to \code{match.strata} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{estimand}}{A \code{character} vector; whether the desired estimand is the "ATT", "ATC", or "ATE" for each set of weights. Essentially the same input to \code{estimand} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{s.weights}}{A vector containing sampling weights for each unit or a string containing the name of the sampling weight variable in \code{data}. Essentially the same input to \code{s.weights} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}} or \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}.
}
\item{\code{focal}}{The name of the focal treatment when multi-category treatments are used. Essentially the same input to \code{focal} in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}.
}
\item{\code{call}}{A \code{call} object containing the function call, usually generated by using \code{\link[=match.call]{match.call()}} inside the function that created \code{x}.
}
}
Any of these items can also be supplied directly to \code{bal.tab.default}, e.g., \code{bal.tab.default(x, formula = treat ~ x1 + x2)}. If supplied, it will override the object with the same role in \code{x}. In addition, any arguments to \code{\link[=bal.tab.formula]{bal.tab.formula()}}, \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}}, and \code{\link[=bal.tab.time.list]{bal.tab.time.list()}} are allowed and perform the same function.

At least some inputs containing information to create the treatment and covariates are required (e.g., \code{formula} and \code{data} or \code{covs} and \code{treat}). All other arguments are optional and have the same defaults as those in \code{\link[=bal.tab.data.frame]{bal.tab.data.frame()}} or \code{\link[=bal.tab.time.list]{bal.tab.time.list()}}. If \code{treat.list}, \code{covs.list}, or \code{formula.list} are supplied in \code{x} or as an argument to \code{bal.tab.default()}, the function will proceed considering a longitudinal treatment. Otherwise, it will proceed considering a point treatment.

\code{bal.tab.default()}, like other \code{bal.tab()} methods, is just a shortcut to supply arguments to \code{bal.tab.data.frame()} or \code{bal.tab.time.list()}. Therefore, any matters regarding argument priority or function are described in the documentation for these methods.
}
\examples{
data("lalonde", package = "cobalt")
covs <- subset(lalonde,  select = -c(treat, re78))

##Writing a function the produces output for direct
##use in bal.tab.default

ate.weights <- function(treat, covs) {
    data <- data.frame(treat, covs)
    formula <- formula(data)
    ps <- glm(formula, data = data, 
              family = "binomial")$fitted.values
    weights <- treat/ps + (1-treat)/(1-ps)
    call <- match.call()
    out <- list(treat = treat,
                covs = covs,
                distance = ps,
                weights = weights,
                estimand = "ATE",
                call = call)
    return(out)
}

out <- ate.weights(lalonde$treat, covs)

bal.tab(out, un = TRUE)
}
\seealso{
\itemize{
\item \code{\link[=bal.tab.formula]{bal.tab.formula()}} and \code{\link[=bal.tab.time.list]{bal.tab.time.list()}} for additional arguments to be supplied.
\item \code{\link[=bal.tab]{bal.tab()}} for output and details of calculations.
\item \code{\link{class-bal.tab.cluster}} for more information on clustered data.
\item \code{\link{class-bal.tab.imp}} for more information on multiply imputed data.
\item \code{\link[=class-bal.tab.multi]{bal.tab.multi()}} for more information on multi-category treatments.
}
}
