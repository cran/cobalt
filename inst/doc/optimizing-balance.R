## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

gbm.ok <- requireNamespace("gbm", quietly = TRUE)
weightit.ok <- requireNamespace("WeightIt", quietly = TRUE)
br.ok <- requireNamespace("brglm2", quietly = TRUE)

## -----------------------------------------------------------------------------
library(cobalt)
data("lalonde", package = "cobalt")

covs <- subset(lalonde, select = -c(treat, race, re78))

# Initialize the object with the balance statistic,
# treatment, and covariates
smd.init <- bal.init(covs,
                     treat = lalonde$treat,
                     stat = "smd.max",
                     estimand = "ATT")

# Compute balance with no weights
bal.compute(smd.init)

# Can also compute the statistic directly using bal.compute():
bal.compute(covs,
            treat = lalonde$treat,
            stat = "smd.max",
            estimand = "ATT")

## -----------------------------------------------------------------------------
bal.tab(covs,
        treat = lalonde$treat,
        binary = "std",
        estimand = "ATT",
        thresholds = .05)

## ----eval = weightit.ok-------------------------------------------------------
library("WeightIt")
w.out <- weightit(treat ~ age + educ + married + nodegree +
                      re74 + re75, data = lalonde,
                  method = "glm", estimand = "ATT",
                  link = "probit")

# Compute the balance statistic on the estimated weights
bal.compute(smd.init, get.w(w.out))

## ----eval = weightit.ok-------------------------------------------------------
w.out <- weightit(treat ~ age + educ + married + nodegree +
                      re74 + re75, data = lalonde,
                  method = "glm", estimand = "ATT",
                  link = "logit")

# Compute the balance statistic on the estimated weights
bal.compute(smd.init, get.w(w.out))

## ----eval = weightit.ok && br.ok----------------------------------------------
w.out <- weightit(treat ~ age + educ + married + nodegree +
                      re74 + re75, data = lalonde,
                  method = "glm", estimand = "ATT",
                  link = "br.logit")

# Compute the balance statistic on the estimated weights
bal.compute(smd.init, get.w(w.out))

## ----eval = weightit.ok && br.ok----------------------------------------------
# Initialize object to compute the largest SMD
smd.init <- bal.init(covs,
                     treat = lalonde$treat,
                     stat = "smd.max",
                     estimand = "ATT")

# Create vector of tuning parameters
links <- c("probit", "logit", "cloglog",
           "br.probit", "br.logit", "br.cloglog")

# Apply each link to estimate weights
# Can replace sapply() with purrr::map()
weights.list <- sapply(links, function(link) {
    w.out <- weightit(treat ~ age + educ + married + nodegree +
                      re74 + re75, data = lalonde,
                  method = "glm", estimand = "ATT",
                  link = link)
    get.w(w.out)
}, simplify = FALSE)

# Use each set of weights to compute balance
# Can replace sapply() with purrr:map_vec()
stats <- sapply(weights.list, bal.compute,
                x = smd.init)

# See which set of weights is the best
stats
stats[which.min(stats)]

## ----eval = weightit.ok && br.ok----------------------------------------------
bal.tab(covs,
        treat = lalonde$treat,
        binary = "std",
        weights = weights.list[["br.cloglog"]])

## ----eval = gbm.ok------------------------------------------------------------
data("lalonde")

# Initialize balance
covs <- subset(lalonde, select = -c(treat, re78))
ks.init <- bal.init(covs,
                    treat = lalonde$treat,
                    stat = "ks.max",
                    estimand = "ATT")

# Fit a GBM model using `WeightIt` and `twang` defaults
fit <- gbm::gbm(treat ~ age + educ + married + race +
                    nodegree + re74 + re75,
                data = lalonde,
                distribution = "bernoulli",
                n.trees = 4000, interaction.depth = 3,
                shrinkage = .01, bag.fraction = 1)

trees_to_test <- seq(1, 4000)

p.mat <- predict(fit, type = "response",
                 n.trees = trees_to_test)

stats <- apply(p.mat, 2, function(p) {
    # Compute ATT weights
    w <- ifelse(lalonde$treat == 1, 1, p/(1-p))
    
    bal.compute(ks.init, weights = w)
})

stats[which.min(stats)]

## ----fig.width=7, fig.height=3, eval = gbm.ok---------------------------------
library("ggplot2")
ggplot() +
    geom_line(aes(x = trees_to_test, y = stats)) +
    theme_bw() +
    labs(y = "ks.max", x = "n.trees")

## ----warning=FALSE, eval = weightit.ok && gbm.ok------------------------------
library("WeightIt")
w.out <- weightit(treat ~ age + educ + married + race +
                    nodegree + re74 + re75,
                data = lalonde, estimand = "ATT",
                method = "gbm", n.trees = 4000,
                stop.method = "ks.max")

# Display the best tree:
w.out$info$best.tree

# ks.max from weightit()
bal.compute(ks.init, weights = get.w(w.out))

