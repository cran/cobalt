\name{f.build}
\alias{f.build}
\title{Convenient Formula Generation}
\description{
\code{f.build()} returns a \code{\link{formula}} of the form \code{y ~ x1 + x2 + ...} from a data frame input. It can be much quicker to use \code{f.build()} than to hand-write the precise formula, which may contain errors. It can be used in place of a formula in, for example, \fun{glm}, \code{matchit()}, or \fun{bal.tab}. It provides similar functionality to \fun{reformulate}.
}
\usage{
f.build(y, rhs)
}
\arguments{
\item{y}{the quoted name of the response (left hand side) variable in the formula. Only one variable is supported. If missing, \code{NULL}, or the empty string (\code{""}), the formula will have no response variable. If \code{rhs} is not supplied, \code{y} will replace \code{rhs} and \code{y} will be set to \code{""}.}

\item{rhs}{a data frame whose variable names will be the terms on the right hand side of the formula, or a character vector whose values will be the terms on the right hand side of the formula. If missing, the argument to \code{y} will replace \code{rhs} and \code{y} will be set to \code{""}; in essence, \code{f.build("x")} is the same as \code{f.build("", "x")}, both producing \code{~ x}.}
}
\value{
a \code{formula} object.
}
\seealso{
\fun{reformulate}
}


\examples{
data(lalonde)
covs <- subset(lalonde, select = -c(treat, re78))
lm(f.build("treat", covs), data = lalonde)
}

\keyword{support functions}