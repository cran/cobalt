## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
#CBPS unavailable
knitr::opts_chunk$set(eval = FALSE)
library("cobalt")

## ------------------------------------------------------------------------
#  data("lalonde", package = "cobalt")
#  covs <- subset(lalonde, select = -c(treat, re78))
#  f.build("treat", covs)

## ---- eval = FALSE-------------------------------------------------------
#  # Generating propensity scores using logistic regression
#  p.score <- glm(f.build("treat", covs), data = lalonde, family = "binomial")$fitted.values
#  
#  # Using matchit() from the MatchIt package
#  library("MatchIt")
#  m.out <- matchit(f.build("treat", covs), data = lalonde, method = "nearest")

## ------------------------------------------------------------------------
#  head(lalonde)
#  lalonde.split <- splitfactor(lalonde, "race")
#  head(lalonde.split)

## ------------------------------------------------------------------------
#  lalonde.unsplit <- unsplitfactor(lalonde.split, "race",
#                                   dropped.level = "black")
#  head(lalonde.unsplit)

## ---- include=FALSE------------------------------------------------------
#  if (!requireNamespace("twang")) knitr::opts_chunk$set(eval = FALSE)

## ---- warning = FALSE----------------------------------------------------
#  library("twang")
#  data("lalonde", package = "cobalt") ##If not yet loaded
#  covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))
#  
#  ps.out <- ps(f.build("treat", covs0), data = lalonde,
#               stop.method = c("es.mean", "es.max"),
#               estimand = "ATT", n.trees = 1000, verbose = FALSE)
#  bal.tab(ps.out, stop.method = "es.mean")

## ---- include=FALSE, eval=TRUE-------------------------------------------
#knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE------------------------------------------------------
#  #if (!requireNamespace("Matching")) knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  library("Matching")
#  data("lalonde", package = "cobalt") #If not yet loaded
#  covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))
#  
#  fit <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")
#  p.score <- fit$fitted.values
#  match.out <- Match(Tr = lalonde$treat, X = p.score, estimand = "ATT")
#  
#  bal.tab(match.out, formula = f.build("treat", covs0), data = lalonde)

## ---- eval = FALSE-------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0)

## ---- include=FALSE, eval=TRUE-------------------------------------------
#knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE------------------------------------------------------
#  #if (!requireNamespace("optmatch")) knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  #Optimal full matching on the propensity score
#  library("optmatch")
#  data("lalonde", package = "cobalt") #If not yet loaded
#  covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))
#  
#  fit <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")
#  lalonde$p.score <- fit$fitted.values #get the propensity score
#  fm <- fullmatch(treat ~ p.score, data = lalonde)
#  
#  bal.tab(fm, formula = f.build("treat", covs0), data = lalonde)

## ---- include=FALSE, eval=TRUE-------------------------------------------
#knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE------------------------------------------------------
#  #if (!requireNamespace("CBPS")) knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  library("CBPS")
#  data("lalonde", package = "cobalt") #If not yet loaded
#  covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))
#  
#  #Generating covariate balancing propensity score weights for ATT
#  cbps.out <- CBPS(f.build("treat", covs0), data = lalonde)
#  
#  bal.tab(cbps.out)

## ---- include=FALSE, eval=TRUE-------------------------------------------
#knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE------------------------------------------------------
#  #if (!requireNamespace("ebal")) knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  library("ebal")
#  data("lalonde", package = "cobalt") #If not yet loaded
#  covs0 <- subset(lalonde, select = -c(treat, re78, race))
#  
#  #Generating entropy balancing weights
#  e.out <- ebalance(lalonde$treat, covs0)
#  
#  bal.tab(e.out, treat = lalonde$treat, covs = covs0)

## ---- include=FALSE------------------------------------------------------
#  #knitr::opts_chunk$set(eval = TRUE)
