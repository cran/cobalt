## ---- include = FALSE---------------------------------------------------------
  knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 5)
#knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("MatchIt"); library("cobalt")
data("lalonde", package = "cobalt")

m.out <- matchit(treat ~ race*(age + educ + married + nodegree + re74 + re75), 
                 data = lalonde, method = "nearest", exact = "race", 
                 replace = TRUE, ratio = 2)

## -----------------------------------------------------------------------------
bal.tab(m.out, cluster = "race")

## -----------------------------------------------------------------------------
#Just for black and hispan
bal.tab(m.out, cluster = "race", which.cluster = c("black", "hispan"),
        cluster.summary = FALSE)

#Just the balance summary across clusters with only the mean
bal.tab(m.out, cluster = "race", which.cluster = .none, cluster.fun = "mean")

## -----------------------------------------------------------------------------
bal.plot(m.out, var.name = "age", cluster = "race", which = "both")

## -----------------------------------------------------------------------------
love.plot(m.out, cluster = "race")

## -----------------------------------------------------------------------------
love.plot(m.out, cluster = "race", which.cluster = .none, agg.fun = "mean")

## -----------------------------------------------------------------------------
love.plot(m.out, cluster = "race", which.cluster = .none, agg.fun = "range")

## -----------------------------------------------------------------------------
library("MatchThem"); library("cobalt"); library("mice")
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 10 #number of imputed data sets
imp.out <- mice(lalonde_mis, m = m, print = FALSE) 


#Performing generalized propensity score weighting in each imputation
wt.out <- weightthem(educ ~ age + race + married + 
                      re74 + re75, datasets = imp.out, 
                     approach = "within", method = "ps")

## -----------------------------------------------------------------------------
#Checking balance on the output object
bal.tab(wt.out)

## -----------------------------------------------------------------------------
bal.tab(wt.out, which.imp = 1, imp.summary = FALSE)

## -----------------------------------------------------------------------------
bal.plot(wt.out, which.imp = 1, var.name = "age", which = "both")

## -----------------------------------------------------------------------------
love.plot(wt.out, threshold = .05)

## -----------------------------------------------------------------------------
#Estimate weights within each imputation using propensity scores
wt3.out <- weightthem(race ~ age + educ + married + 
                      nodegree + re74 + re75, 
                     datasets = imp.out, approach = "within", 
                     method = "ps", estimand = "ATE")

## -----------------------------------------------------------------------------
bal.tab(wt3.out)

## -----------------------------------------------------------------------------
bal.plot(wt3.out, var.name = "married", which.imp = 1,
         which = "both")

## -----------------------------------------------------------------------------
love.plot(wt3.out, threshold = .1, agg.fun = "mean")

