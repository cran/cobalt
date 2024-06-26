## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
library("cobalt")

## -----------------------------------------------------------------------------
head(lalonde)
lalonde.split <- splitfactor(lalonde, "race")
head(lalonde.split)

## -----------------------------------------------------------------------------
lalonde.unsplit <- unsplitfactor(lalonde.split, "race", 
                                 dropped.level = "black")
head(lalonde.unsplit)

## ----warning = FALSE, eval = requireNamespace("twang", quietly = TRUE)--------
#GBM PS weighting for the ATT
data("lalonde", package = "cobalt") ##If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))
f <- reformulate(names(covs0), "treat")

ps.out <- twang::ps(f, data = lalonde, 
                    stop.method = c("es.mean", "es.max"), 
                    estimand = "ATT", n.trees = 1000,
                    verbose = FALSE)
bal.tab(ps.out, stop.method = "es.mean")

## ----eval = requireNamespace("Matching", quietly = TRUE)----------------------
#1:1 NN PS matching w/ replacement
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))
f <- reformulate(names(covs0), "treat")

fit <- glm(f, data = lalonde, family = binomial)
p.score <- fit$fitted.values
match.out <- Matching::Match(Tr = lalonde$treat, X = p.score,
                             estimand = "ATT")

bal.tab(match.out, formula = f, data = lalonde,
        distance = ~ p.score)

## ----eval = FALSE-------------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0,
#          distance = ~ p.score)

## ----eval = requireNamespace("optmatch", quietly = TRUE)----------------------
#Optimal full matching on the propensity score
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))
f <- reformulate(names(covs0), "treat")

fit <- glm(f, data = lalonde, family = binomial)
p.score <- fit$fitted.values #get the propensity score
fm <- optmatch::fullmatch(treat ~ p.score, data = lalonde)

bal.tab(fm, covs = covs0, distance = ~ p.score)

## ----eval = requireNamespace("CBPS", quietly = TRUE)--------------------------
#CBPS weighting
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78))
f <- reformulate(names(covs0), "treat")

#Generating covariate balancing propensity score weights for ATT
cbps.out <- CBPS::CBPS(f, data = lalonde)

bal.tab(cbps.out)

## ----eval = requireNamespace("ebal", quietly = TRUE)--------------------------
#Entropy balancing
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, race))

#Generating entropy balancing weights
e.out <- ebal::ebalance(lalonde$treat, covs0)

bal.tab(e.out, treat = lalonde$treat, covs = covs0)

## ----eval = requireNamespace("designmatch", quietly = TRUE)-------------------
#Mixed integer programming matching
library("designmatch")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, race))

#Matching for balance on covariates
dmout <- bmatch(lalonde$treat,
                dist_mat = NULL,
                subset_weight = NULL,
                mom = list(covs = covs0,
                           tols = absstddif(covs0, lalonde$treat, .05)),
                n_controls = 1,
                total_groups = 185)

bal.tab(dmout, treat = lalonde$treat, covs = covs0)

## ----eval = requireNamespace("sbw", quietly = TRUE)---------------------------
#Optimization-based weighting
data("lalonde", package = "cobalt") #If not yet loaded
lalonde_split <- splitfactor(lalonde, drop.first = "if2")
cov.names <- setdiff(names(lalonde_split), c("treat", "re78"))

#Estimating balancing weights for the ATT
sbw.out <- sbw::sbw(lalonde_split,
                    ind = "treat",
                    bal = list(bal_cov = cov.names,
                               bal_alg = FALSE, 
                               bal_tol = .001),
                    par = list(par_est = "att"))
bal.tab(sbw.out, un = TRUE, disp.means = TRUE)

## ----eval = requireNamespace("MatchThem", quietly = TRUE) && requireNamespace("mice", quietly = TRUE)----
#PS weighting on multiply imputed data
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 10 #number of imputed data sets
imp.out <- mice::mice(lalonde_mis, m = m, print = FALSE) 

#Matching for balance on covariates
mt.out <- MatchThem::matchthem(treat ~ age + educ + married +
                                   race + re74 + re75, 
                               datasets = imp.out,
                               approach = "within", 
                               method = "nearest",
                               estimand = "ATT")

bal.tab(mt.out)

#Weighting for balance on covariates
wt.out <- MatchThem::weightthem(treat ~ age + educ + married +
                                    race + re74 + re75, 
                                datasets = imp.out,
                                approach = "within", 
                                method = "glm",
                                estimand = "ATE")

bal.tab(wt.out)

## ----eval = FALSE && requireNamespace("cem", quietly = TRUE)------------------
#  #Coarsened exact matching
#  data("lalonde", package = "cobalt") #If not yet loaded
#  
#  #Matching for balance on covariates
#  cem.out <- cem::cem("treat", data = lalonde, drop = "re78")
#  
#  bal.tab(cem.out, data = lalonde, stats = c("m", "ks"))

## ----message = F, eval = FALSE && all(sapply(c("cem", "mice"), requireNamespace, quietly = TRUE))----
#  #Coarsened exact matching on multiply imputed data
#  data("lalonde_mis", package = "cobalt")
#  
#  #Generate imputed data sets
#  m <- 10 #number of imputed data sets
#  imp.out <- mice::mice(lalonde_mis, m = m, print = FALSE)
#  imp.data.list <- mice::complete(imp.out, "all")
#  
#  #Match within each imputed dataset
#  cem.out.imp <- cem::cem("treat", datalist = imp.data.list,
#                          drop = "re78")
#  
#  bal.tab(cem.out.imp, data = imp.out)

## ----eval = requireNamespace("optweight", quietly = TRUE)---------------------
#Optimization-based weighting
data("lalonde", package = "cobalt")

#Estimate the weights using optimization
ow.out <- optweight::optweight(treat ~ age + educ + married + race + re74 + re75,
                               data = lalonde, estimand = "ATE", tols = .01)

#Note the contents of the output object:
names(ow.out)

#Use bal.tab() directly on the output
bal.tab(ow.out)

