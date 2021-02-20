## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
library("cobalt")

## -----------------------------------------------------------------------------
data("lalonde", package = "cobalt")
covs <- subset(lalonde, select = -c(treat, re78))
f.build("treat", covs)

## ---- eval = FALSE------------------------------------------------------------
#  # Generating propensity scores using logistic regression
#  p.score <- glm(f.build("treat", covs), data = lalonde, family = "binomial")$fitted.values
#  
#  # Using matchit() from the MatchIt package
#  library("MatchIt")
#  m.out <- matchit(f.build("treat", covs), data = lalonde, method = "nearest")

## -----------------------------------------------------------------------------
head(lalonde)
lalonde.split <- splitfactor(lalonde, "race")
head(lalonde.split)

## -----------------------------------------------------------------------------
lalonde.unsplit <- unsplitfactor(lalonde.split, "race", 
                                 dropped.level = "black")
head(lalonde.unsplit)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("twang", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## ---- warning = FALSE---------------------------------------------------------
library("twang")
data("lalonde", package = "cobalt") ##If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))

ps.out <- ps(f.build("treat", covs0), data = lalonde, 
             stop.method = c("es.mean", "es.max"), 
             estimand = "ATT", n.trees = 1000, verbose = FALSE)
bal.tab(ps.out, stop.method = "es.mean")

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("Matching", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("Matching")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))

fit <- glm(f.build("treat", covs0), data = lalonde, family = binomial)
p.score <- fit$fitted.values
match.out <- Match(Tr = lalonde$treat, X = p.score, estimand = "ATT")

bal.tab(match.out, formula = f.build("treat", covs0), data = lalonde,
        distance = ~ p.score)

## ---- eval = FALSE------------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0,
#          distance = ~ p.score)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("optmatch", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#Optimal full matching on the propensity score
library("optmatch")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))

fit <- glm(f.build("treat", covs0), data = lalonde, family = binomial)
p.score <- fit$fitted.values #get the propensity score
fm <- fullmatch(treat ~ p.score, data = lalonde)

bal.tab(fm, covs = covs0, distance = ~ p.score)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("CBPS", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("CBPS")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))

#Generating covariate balancing propensity score weights for ATT
cbps.out <- CBPS(f.build("treat", covs0), data = lalonde)

bal.tab(cbps.out)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("ebal", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("ebal")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, race))

#Generating entropy balancing weights
e.out <- ebalance(lalonde$treat, covs0)

bal.tab(e.out, treat = lalonde$treat, covs = covs0)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("designmatch", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("designmatch")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, race))

#Matching for balance on covariates
dmout <- bmatch(lalonde$treat,
                dist_mat = NULL,
                subset_weight = NULL,
                mom = list(covs = covs0,
                           tols = absstddif(covs0, lalonde$treat, .005)),
                n_controls = 1,
                total_groups = 185)

bal.tab(dmout, treat = lalonde$treat, covs = covs0)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("sbw", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("sbw")
data("lalonde", package = "cobalt") #If not yet loaded
lalonde_split <- splitfactor(lalonde, drop.first = "if2")
cov.names <- c("age", "educ", "race_black", "race_hispan", 
               "race_white", "married", "nodegree", 
               "re74", "re75")

#Estimating balancing weights for the ATT
sbw.out <- sbw(lalonde_split,
               ind = "treat",
               bal = list(bal_cov = cov.names,
                          bal_alg = FALSE, 
                          bal_tol = .001),
               par = list(par_est = "att"))
bal.tab(sbw.out, un = TRUE, disp.means = TRUE)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("MatchThem", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("mice"); library("MatchThem")
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 10 #number of imputed data sets
imp.out <- mice(lalonde_mis, m = m, print = FALSE) 

#Matching for balance on covariates
wt.out <- weightthem(treat ~ age + educ + married +
                       race + re74 + re75, 
                     datasets = imp.out,
                     approach = "within", 
                     method = "ps",
                     estimand = "ATE")

bal.tab(wt.out)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (any(!sapply(c("cem", "mice"), requireNamespace, quietly = TRUE))) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("cem")
data("lalonde", package = "cobalt") #If not yet loaded

#Matching for balance on covariates
cem.out <- cem("treat", data = lalonde, drop = "re78")

bal.tab(cem.out, data = lalonde, stats = c("m", "ks"))

## -----------------------------------------------------------------------------
library("mice"); library("cem")
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 10 #number of imputed data sets
imp.out <- mice(lalonde_mis, m = m, print = FALSE) 
imp.data.list <- lapply(1:m, complete, data = imp.out)

#Match within each imputed dataset
cem.out.imp <- cem("treat", datalist = imp.data.list, drop = "re78")

bal.tab(cem.out.imp, data = imp.out)


## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("optweight", quietly = TRUE)) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("optweight")
data("lalonde", package = "cobalt")

#Estimate the weights using optimization
ow.out <- optweight(treat ~ age + educ + married + race + re74 + re75,
                    data = lalonde, estimand = "ATE", tols = .01)

#Note the contents of the output object:
names(ow.out)

#Use bal.tab() directly on the output
bal.tab(ow.out)

## ---- include=FALSE, eval=TRUE------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

