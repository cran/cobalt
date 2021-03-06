\name{bal.tab}
\alias{bal.tab}

\title{
Display Balance Statistics in a Table
}
\description{
Generates balance statistics on covariates in relation to an observed treatment variable. It is a generic function that dispatches to the method corresponding to the class of the first argument. This page links to each method page and documents the calculation and details of aspects all \code{bal.tab()} methods use. For information on the use of \code{bal.tab()} with specific types of objects, use the following links:
\itemize{
\item{\fun{bal.tab.matchit} for the method for objects returned by \pkg{MatchIt}.
}
\item{\fun{bal.tab.weightit} for the method for \code{weightit} and \code{weightitMSM} objects returned by \pkg{WeightIt}.
}
\item{\fun{bal.tab.ps} for the method for \code{ps}, \code{mnps}, and \code{iptw} objects returned by \pkg{twang} and for \code{ps.cont} objects returned by \pkg{WeightIt}.
}
\item{\fun{bal.tab.Match} for the method for objects returned by \pkg{Matching}.
}
\item{\fun{bal.tab.optmatch} for the method for objects returned by \pkg{optmatch}.
}
\item{\fun{bal.tab.cem.match} for the method for objects returned by \pkg{cem}.
}
\item{\fun{bal.tab.CBPS} for the method for objects returned by \pkg{CBPS}.
}
\item{\fun{bal.tab.ebalance} for the method for objects returned by \pkg{ebal}.
}
\item{\fun{bal.tab.designmatch} for the method for objects returned by \pkg{designmatch}.
}
\item{\fun{bal.tab.mimids} for the method for objects returned by \pkg{MatchThem}.
}
\item{\code{\link[=bal.tab.sbwcau]{bal.tab.sbw()}} for the method for objects returned by \pkg{sbw}.
}
\item{\fun{bal.tab.formula} and \fun{bal.tab.data.frame} for the methods for \code{formula} and data frame interfaces when the user has covariate values and weights (including matching weights) or subclasses or wants to evaluate balance on an unconditioned data set. For data that corresponds to a longitudinal treatment (i.e., to be analyzed with a marginal structural model), see \fun{bal.tab.time.list}.
}
\item{\fun{bal.tab.default} for the method for objects that do not come from one of the explicitly supported conditioning packages.
}
}
}
\usage{
bal.tab(x, \dots)
}

\arguments{
  \item{x}{
an input object on which to assess balance. Can be the output of a call to a balancing function in another package or a formula or data frame. Input to this argument will determine which \code{bal.tab()} method is used.
  }
  \item{\dots}{
arguments passed to other methods. These arguments may be data-related, computation-related, or print-related. See the help pages (linked above) for the individual methods and pages linked below for other arguments that can be supplied.
}
}
\details{
\code{bal.tab()} performs various calculations on the the data objects given, and some of these calculations are not transparent on the help pages of the individual methods. This page details the calculations that are used across \code{bal.tab()} methods.

\subsection{With Binary Point Treatments}{

Balance statistics can be requested with the \code{\link[=balance.stats]{stats}} argument. The default balance statistic for mean differences for continuous variables is the standardized mean difference, which is the difference in the means divided by a measure of spread (i.e., a d-type effect size measure). This is the default because it puts the mean differences on the same scale for comparison with each other and with a given threshold. For binary variables, the default balance statistic is the raw difference in proportion. Although standardized differences in proportion can be computed, raw differences in proportion for binary variables are already on the same scale, and computing the standardized difference in proportion can obscure the true difference in proportion by dividing the difference in proportion by a number that is itself a function of the observed proportions.

Standardized mean differences are calculated using \fun{col_w_smd} as follows: the numerator is the mean of the treated group minus the mean of the control group, and the denominator is a measure of spread calculated in accordance with the argument to \code{s.d.denom} or the default of the specific method used. Common approaches in the literature include using the standard deviation of the treated group or using the "pooled" standard deviation (i.e., the square root of the mean of the group variances) in calculating standardized mean differences. The computed spread \code{bal.tab()} uses is always that of the full, unadjusted sample (i.e., before matching, weighting, or subclassification), as recommended by Stuart (2010).

Prior to computation, all variables are checked for variable type, which allows users to differentiate balance statistic calculations based on type using the arguments to \code{continuous} and \code{binary}. First, if a given covariate is numeric and has only 2 levels, it is converted into a binary (0,1) variable. If 0 is a value in the original variable, it retains its value and the other value is converted to 1; otherwise, the lower value is converted to 0 and the other to 1. Next, if the covariate is not numeric or logical (i.e., is a character or factor variable), it will be split into new binary variables, named with the original variable and the value, separated by an underscore. Otherwise, the covariate will be used as is and treated as a continuous variable.

Variance ratios are computed within-sample using \fun{col_w_vr}, with the larger of the two variances in the numerator if \code{abs = TRUE}, yielding values greater than or equal to 1. Variance ratios are not calculated for binary variables since they are only a function of the group proportions and thus provide the same information as differences in proportion.

When weighting or matching are used, an "effective sample size" is calculated for each group using the following formula: \eqn{(\sum w)^2 / \sum w^2}. The effective sample size is "approximately the number of observations from a simple random sample that yields an estimate with sampling variation equal to the sampling variation obtained with the weighted comparison observations" (Ridgeway et al., 2016). The calculated number tends to underestimate the true effective sample size of the weighted samples. The number depends on the variability of the weights, so sometimes trimming units with large weights can actually increase the effective sample size, even though units are being down-weighted. When matching is used, an additional "unweighted" sample size will be displayed indicating the total number of units contributing to the weighted sample.

When subclassification is used, the balance tables for each subclass stored in \code{$Subclass.Balance} use values calculated as described above. For the aggregate balance table stored in \code{$Balance.Across.Subclass}, the values of each statistic are computed as a weighted average of the statistic across subclasses, weighted by the proportion of units in each subclass. See \code{\link[=class-bal.tab.subclass]{bal.tab.subclass}} for more details.}

\subsection{With Continuous Point Treatments}{

When continuous treatment variables are considered, the balance statistic calculated is the Pearson correlation between the covariate and treatment. The correlation after adjustment is computed using \fun{col_w_cov} as the weighted covariance between the covariate and treatment divided by the product of the standard deviations of the unweighted covariate and treatment, in an analogous way to how how the weighted standardized mean difference uses an unweighted measure of spread in its denominator, with the purpose of avoiding the analogous paradox (i.e., where the covariance decreases but is accompanied by a change in the standard deviations, thereby distorting the actual resulting balance computed using the weighted standard deviations).
}
\subsection{With Multi-Category Point Treatments}{

For information on using \code{bal.tab()} with multi-category treatments, see \code{\link[=class-bal.tab.multi]{bal.tab.multi}}. Essentially, \code{bal.tab()} compares pairs of treatment groups in a standard way.
}
\subsection{With Longitudinal Treatments}{

For information on using \code{bal.tab()} with longitudinal treatments, see \code{\link[=class-bal.tab.msm]{bal.tab.msm}}. Essentially, \code{bal.tab()} summarizes balance at each time point and summarizes across time points.
}
\subsection{With Clustered or Multiply Imputed Data}{

For information on using \code{bal.tab()} with clustered data, see \code{\link[=class-bal.tab.cluster]{bal.tab.cluster}}. For information on using \code{bal.tab()} with multiply imputed data, see \code{\link[=class-bal.tab.imp]{bal.tab.imp}}. 
}
\subsection{Quick}{

Calculations can take some time, especially when there are many variables, interactions, or clusters. When certain values are not printed, by default they are not computed. In particular, variance ratios, KS statistics, and summary tables are not computed when their display has not been requested. This can speed up the overall production of the output when these values are not to be used later. However, when they are to be used later, such as when output is to be further examined with \code{print()} or is to be used in some other way after the original call to \code{bal.tab()}, it may be useful to compute them even if they are not to be printed initially. To do so, users can set \code{quick = FALSE}, which will cause \code{bal.tab()} to calculate all values and components it can. Note that \code{love.plot()} is fully functional even when \code{quick = TRUE} and values are requested that are otherwise not computed in \code{bal.tab()} with \code{quick = TRUE}.
}
\subsection{Missing Data}{

If there is missing data in the covariates (i.e., \code{NA}s in the covariates provided to \code{bal.tab()}), a few additional things happen. A warning will appear mentioning that missing values were present in the data set. The computed balance summaries will be for the variables ignoring the missing values. New variables will be created representing missingness indicators for each variable, named \code{var:<NA>} (with \code{var} replaced by the actual name of the variable). If \code{int = TRUE}, balance for the pairwise interactions between the missingness indicators will also be computed. These variables are treated like regular variables once created.
}
}
\value{
An object of class \code{"bal.tab"}. The use of continuous treatments, subclasses, clusters, and/or imputations will also cause the object to inherit other classes. The class \code{"bal.tab"} has its own \code{print()} method (\fun{print.bal.tab}), which formats the output nicely and in accordance with print-related options given in the call to \code{bal.tab()}, and which can be called with its own options.

For scenarios with binary point treatments and no subclasses, imputations, or clusters, the following are the elements of the \code{bal.tab} object:

\item{Balance}{A data frame containing balance information for each covariate. Balance contains the following columns, with additional columns present when other balance statistics are requested:
\itemize{
\item{\code{Type}: Whether the covariate is binary, continuous, or a measure of distance (e.g., the propensity score).}
\item{\code{M.0.Un}: The mean of the control group prior to adjusting.}
\item{\code{SD.0.Un}: The standard deviation of the control group prior to adjusting.}
\item{\code{M.1.Un}: The mean of the treated group prior to adjusting.}
\item{\code{SD.1.Un}: The standard deviation of the treated group prior to adjusting.}
\item{\code{Diff.Un}: The (standardized) difference in means between the two groups prior to adjusting. See the \code{binary} and \code{continuous} arguments on the \code{bal.tab} method pages to determine whether standardized or raw mean differences are being reported. By default, the standardized mean difference is displayed for continuous variables and the raw mean difference (difference in proportion) is displayed for binary variables.}
\item{\code{M.0.Adj}: The mean of the control group after adjusting.}
\item{\code{SD.0.Adj}: The standard deviation of the control group after adjusting.}
\item{\code{M.1.Adj}: The mean of the treated group after adjusting.}
\item{\code{SD.1.Adj}: The standard deviation of the treated group after adjusting.}
\item{\code{Diff.Adj}: The (standardized) difference in means between the two groups after adjusting. See the \code{binary} and \code{continuous} arguments on the \code{bal.tab} method pages to determine whether standardized or raw mean differences are being reported. By default, the standardized mean difference is displayed for continuous variables and the raw mean difference (difference in proportion) is displayed for binary variables.}
\item{\code{M.Threshold}: Whether or not the calculated mean difference after adjusting exceeds or is within the threshold given by \code{thresholds}.  If a threshold for mean differences is not specified, this column will be \code{NA}.}
}}
\item{Balanced.Means}{If a threshold on mean differences is specified, a table tallying the number of variables that exceed or are within the threshold.}
\item{Max.Imbalance.Means}{If a threshold on mean differences is specified, a table displaying the variable with the greatest absolute mean difference.}
\item{Observations}{A table displaying the sample sizes before and after adjusting. Often the effective sample size (ESS) will be displayed. See Details.}
\item{call}{The original function call, if adjustment was performed by a function in another package.}

If the treatment is continuous, instead of producing mean differences, \code{bal.tab()} will produce correlations between the covariates and the treatment. The corresponding entries in the output will be "Corr.Un", "Corr.Adj", and "R.Threshold" (and accordingly for the balance tally and maximum imbalance tables).

If multiple weights are supplied, \code{"Adj"} in \code{Balance} will be replaced by the provided names of the sets of weights, and extra columns will be added for each set of weights. Additional columns and rows for other items in the output will be created as well.

For \code{bal.tab} output with subclassification, see \code{\link[=class-bal.tab.subclass]{bal.tab.subclass}}.
}

\references{
Ridgeway, G., McCaffrey, D., Morral, A., Burgette, L., & Griffin, B. A. (2016). Toolkit for Weighting and Analysis of Nonequivalent Groups: A tutorial for the twang package. R vignette. RAND.

Stuart, E. A. (2010). Matching Methods for Causal Inference: A Review and a Look Forward. Statistical Science, 25(1), 1-21. \doi{10.1214/09-STS313}
}
\author{
Noah Greifer 
}
\keyword{tables}
