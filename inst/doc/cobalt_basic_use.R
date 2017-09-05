## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(message = FALSE)

## ---- eval = 2-----------------------------------------------------------
#install.packages("cobalt")
library("cobalt")

## ------------------------------------------------------------------------
data("lalonde", package = "cobalt")
covs <- subset(lalonde, select = -c(treat, re78))
f.build("treat", covs)

## ---- eval = FALSE-------------------------------------------------------
#  # Generating propensity scores using logistic regression
#  p.score <- glm(f.build("treat", covs), data = lalonde, family = "binomial")$fitted.values
#  
#  # Using matchit() from the MatchIt package
#  library("MatchIt")
#  m.out <- matchit(f.build("treat", covs), data = lalonde, method = "nearest")

## ------------------------------------------------------------------------
head(lalonde)
lalonde.split <- splitfactor(lalonde, "race")
head(lalonde.split)

## ------------------------------------------------------------------------
lalonde.unsplit <- unsplitfactor(lalonde.split, "race", 
                                   dropped.level = "black")
head(lalonde.unsplit)

## ------------------------------------------------------------------------
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Generating ATT weights as specified in Austin (2011)
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, 
                       family = "binomial")$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights,
        method = "weighting")


## ------------------------------------------------------------------------
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        distance = "p.score", method = "weighting")

## ------------------------------------------------------------------------
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        method = "weighting", binary = "std", continuous = "std")

## ------------------------------------------------------------------------
# Balance on all covariates in data set, including interactions and squares
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        method = "weighting", addl = c("nodegree", "married"), int = TRUE)

## ------------------------------------------------------------------------
# Balance tables with variance ratios and statistics for the unadjusted sample
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        method = "weighting", disp.v.ratio = TRUE, un = TRUE)

## ------------------------------------------------------------------------
# Balance tables with thresholds for mean differences and variance ratios
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        method = "weighting", m.threshold = .1, v.threshold = 2)

## ------------------------------------------------------------------------
# Generating ATT weights with different covariates
lalonde$p.score2 <- glm(treat ~ age + I(age^2) + race + educ + re74, 
                        data = lalonde, family = "binomial")$fitted.values
lalonde$att.weights2 <- with(lalonde, treat + (1-treat)*p.score2/(1-p.score2))

bal.tab(f.build("treat", covs0), data = lalonde, weights = c("att.weights", "att.weights2"),
        method = "weighting", estimand = "ATT")

## ------------------------------------------------------------------------
# Subclassification for ATT with 6 subclasses
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, 
                       family = "binomial")$fitted.values
nsub <- 6 #number of subclasses
lalonde$subclass <- findInterval(lalonde$p.score, 
                                 quantile(lalonde$p.score[lalonde$treat == 1], 
                                          seq(0, 1, length.out = nsub + 1)), 
                                 all.inside = T)

bal.tab(f.build("treat", covs0), data = lalonde, subclass = "subclass", 
        method = "subclassification", disp.subclass = TRUE)

## ------------------------------------------------------------------------
data("lalonde", package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 2:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build("treat", covs0), data = lalonde, method = "nearest", 
                 ratio = 1,  replace = TRUE)

bal.tab(m.out)

## ---- warning = FALSE----------------------------------------------------
library("twang")
data("lalonde", package = "cobalt") ##If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

ps.out <- ps(f.build("treat", covs0), data = lalonde, 
             stop.method = c("es.mean", "es.max"), 
             estimand = "ATT", n.trees = 1000, verbose = FALSE)
bal.tab(ps.out, stop.method = "es.mean")

## ------------------------------------------------------------------------
library("Matching")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")
p.score <- fit$fitted.values
match.out <- Match(Tr = lalonde$treat, X = p.score, estimand = "ATT")

bal.tab(match.out, formula = f.build("treat", covs0), data = lalonde)

## ---- eval = FALSE-------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0)

## ------------------------------------------------------------------------
#Optimal full matching on the propensity score
library("optmatch")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")
lalonde$p.score <- fit$fitted.values #get the propensity score
fm <- fullmatch(treat ~ p.score, data = lalonde)

bal.tab(fm, formula = f.build("treat", covs0), data = lalonde)

## ------------------------------------------------------------------------
library("CBPS")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

#Generating covariate balancing propensity score weights for ATT
cbps.out <- CBPS(f.build("treat", covs0), data = lalonde)

bal.tab(cbps.out)

## ------------------------------------------------------------------------
library("ebal")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, race))

#Generating entorpy balancing weights
e.out <- ebalance(lalonde$treat, covs0)

bal.tab(e.out, treat = lalonde$treat, covs = covs0)

## ---- fig.show = "hold"--------------------------------------------------
data("lalonde", package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Generating ATT weights as specified in Austin (2011)
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, 
                       family = "binomial")$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.plot(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting",
         estimand = "ATT", var.name = "age")
bal.plot(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting",
         estimand = "ATT", var.name = "race")

## ---- fig.width = 5------------------------------------------------------
#Before and after weighting; which = "both"
bal.plot(f.build("treat", covs0), data = lalonde, var.name = "p.score",
         weights = "att.weights", distance = "p.score", 
         method = "weighting", which = "both")


## ---- fig.width = 5------------------------------------------------------
data("lalonde", package = "cobalt")
covs <- subset(lalonde, select = -c(treat, re78))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build("treat", covs), data = lalonde, method = "nearest", replace = TRUE)

love.plot(bal.tab(m.out), threshold = .1)

## ---- fig.width = 5------------------------------------------------------
v <- data.frame(old = c("age", "educ", "race_black", "race_hispan", 
                        "race_white", "married", "nodegree", "re74", "re75", "distance"),
                new = c("Age", "Years of Education", "Black", 
                        "Hispanic", "White", "Married", "No Degree Earned", 
                        "Earnings 1974", "Earnings 1975", "Propensity Score"))
                
love.plot(bal.tab(m.out), stat = "mean.diffs", threshold = .1, 
          var.order = "unadjusted", var.names = v, abs = TRUE,
          line = TRUE, limits = c(0, 1))

## ------------------------------------------------------------------------
data("lalonde", package = "cobalt")
library("CBPS")
cov.c <- subset(lalonde, select = -c(treat, re78, re75))

#Generating propensity scores with re75 as the continuous treatment
cbps.c <- CBPS(f.build("re75", cov.c), data = lalonde)

## ------------------------------------------------------------------------
#Assessing balance numerically
bal.tab(cbps.c, un = TRUE, r.threshold = .1, int = TRUE)

## ---- fig.width = 5------------------------------------------------------
#Assessing balance graphically
bal.plot(cbps.c, "re74", which = "both")

bal.plot(cbps.c, "married", which = "both")

## ---- fig.width = 5------------------------------------------------------
#Summarizing balance in a Love plot
love.plot(bal.tab(cbps.c), threshold = .1, abs = TRUE, var.order = "unadjusted",
          line = TRUE)

## ------------------------------------------------------------------------
bal.tab(f.build("treat", covs0), data = lalonde, 
        weights = data.frame(GBM = get.w(ps.out),
                             CBPS = get.w(cbps.out)),
        method = "weighting", disp.v.ratio = TRUE)

## ---- fig.width=7--------------------------------------------------------
bal.plot(f.build("treat", covs0), data = lalonde, 
         weights = data.frame(GBM = get.w(ps.out),
                              CBPS = get.w(cbps.out)),
         method = "weighting", var.name = "age", which = "both")

## ---- fig.width=5--------------------------------------------------------
love.plot(bal.tab(f.build("treat", covs0), data = lalonde, 
                  weights = data.frame(GBM = get.w(ps.out),
                                       CBPS = get.w(cbps.out)),
                  method = "weighting"), var.order = "unadjusted",
          abs = TRUE, colors = c("red", "blue", "darkgreen"))

## ------------------------------------------------------------------------
ctrl.data <- lalonde[lalonde$treat == 0,]
ctrl.fit <- glm(re78 ~ age + educ + race + 
                married + nodegree + re74 + re75,
                data = ctrl.data)
lalonde$prog.score <- predict(ctrl.fit, lalonde)

bal.tab(m.out, distance = lalonde[, "prog.score", drop = FALSE])

## ---- echo = FALSE, fig.show = 'hold', fig.width = 5---------------------
plot(ps.out, plots = "es", subset = 1)
love.plot(bal.tab(ps.out, full.stop.method = "es.mean.att"), threshold = .1,
          abs = TRUE, var.order = "u", color = c("red", "blue"), line = TRUE,
          drop.distance = TRUE)

