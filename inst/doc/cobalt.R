## ---- message = FALSE, eval = 2------------------------------------------
install.packages("cobalt")
library("cobalt")

## ------------------------------------------------------------------------
data(lalonde, package = "cobalt")
covs <- subset(lalonde, select = -c(treat, re78))
f.build(treat, covs)

## ---- eval = FALSE-------------------------------------------------------
#  # Generating propensity scores using logistic regression
#  p.score <- glm(f.build(treat, covs), data = lalonde, family = "binomial")$fitted.values
#  
#  # Using matchit() from the MatchIt package
#  library("MatchIt")
#  m.out <- matchit(f.build(treat, covs), data = lalonde, method = "nearest")

## ---- message = FALSE----------------------------------------------------
data(lalonde, package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build(treat, covs0), data = lalonde, method = "nearest", replace = TRUE)
bal.tab(m.out)

## ------------------------------------------------------------------------
bal.tab(m.out, binary = "std", continuous = "std") #Same as MatchIt's summary(..., standardize = TRUE)

## ------------------------------------------------------------------------
# Balance on all covariates in data set, including interactions and squares
bal.tab(m.out, addl = lalonde[,c("nodegree", "married")], int = TRUE)

## ------------------------------------------------------------------------
# Balance tables with variance ratios and statistics for the unadjusted sample
bal.tab(m.out, disp.v.ratio = TRUE, un = TRUE)

## ------------------------------------------------------------------------
# Balance tables with thresholds for mean differences and variance ratios
bal.tab(m.out, m.threshold = .1, v.threshold = 2)

## ------------------------------------------------------------------------
# Subclassification with 6 subclasses (the default in MatchIt)
m.out.sub <- matchit(f.build(treat, covs0), data = lalonde, method = "subclass")

bal.tab(m.out.sub, disp.subclass = TRUE)

## ---- message = FALSE, warning = FALSE-----------------------------------
library("twang")
data(lalonde, package = "cobalt") ##If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

ps.out <- ps(f.build(treat, covs0), data = lalonde, stop.method = c("es.mean",
             "es.max"), estimand = "ATT", n.trees = 1000, verbose = FALSE)
bal.tab(ps.out, full.stop.method = "es.mean.att")

## ---- message = FALSE----------------------------------------------------
library("Matching")
data(lalonde, package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build(treat, covs0), data = lalonde, family = "binomial")
p.score <- fit$fitted.values
match.out <- Match(Tr = lalonde$treat, X = p.score, estimand = "ATT")

bal.tab(match.out, formula = f.build(treat, covs0), data = lalonde)

## ---- eval = FALSE-------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0)

## ---- message = FALSE----------------------------------------------------
library("CBPS")
data(lalonde, package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

cbps.out <- CBPS(f.build(treat, covs0), data = lalonde, standardize = FALSE)

bal.tab(cbps.out)

## ------------------------------------------------------------------------
# Generating ATT weights as specified in Austin (2011)

data(lalonde, package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build(treat, covs0), data = lalonde, family = "binomial")
lalonde$p.score <- fit$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights,
        method = "weighting")


## ------------------------------------------------------------------------
bal.tab(f.build(treat, covs0), data = lalonde, weights = "att.weights",
        distance = "p.score", method = "weighting")

## ---- message = FALSE, fig.show = "hold"---------------------------------
data(lalonde, package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build(treat, covs0), data = lalonde, method = "nearest", replace = TRUE)

bal.plot(m.out, "age")
bal.plot(m.out, "black")

## ---- message = FALSE----------------------------------------------------
library("ggplot2")
bp <- bal.plot(m.out, "age")
bp + theme_bw() + scale_fill_manual( values = c("black","white")) + 
    labs(title = "Distributional Balance for Age", x = "Age")

## ---- fig.show = "hold"--------------------------------------------------
data(lalonde, package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build(treat, covs0), data = lalonde, family = "binomial")
lalonde$p.score <- fit$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.plot(f.build(treat, covs0), data = lalonde, var.name = ".distance",
         weights = "att.weights", distance = "p.score", 
         method = "weighting", un = TRUE)
bal.plot(f.build(treat, covs0), data = lalonde, var.name = ".distance",
         weights = "att.weights", distance = "p.score", 
         method = "weighting", un = FALSE)

## ---- fig.width = 5------------------------------------------------------
data(lalonde, package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build(treat, covs0), data = lalonde, method = "nearest", replace = TRUE)

love.plot(bal.tab(m.out), threshold = .1)

## ---- fig.width = 5------------------------------------------------------
v <- data.frame(old = c("age", "educ", "black", "hispan", 
                        "re74", "re75"),
                new = c("Age", "Years of Education", "Black", 
                        "Hispanic", "Earnings 1974", 
                        "Earnings 1975"))
                
love.plot(bal.tab(m.out), stat = "mean.diffs", threshold = .1, 
          var.order = "unadjusted", var.names = v, abs = TRUE)

## ------------------------------------------------------------------------
ctrl.data <- lalonde[lalonde$treat == 0,]
ctrl.fit <- glm(re78 ~ age + educ + black + hispan + 
                married + nodegree + re74 + re75,
                data = ctrl.data)
lalonde$prog.score <- predict(ctrl.fit, lalonde)

bal.tab(m.out, addl = data.frame(prog.score = lalonde$prog.score))

## ---- echo = FALSE, message = FALSE, fig.show = 'hold', fig.width = 5----
plot(ps.out, plots = "es", subset = 1)
love.plot(bal.tab(ps.out, full.stop.method = "es.max.att"), threshold = .1,
          abs = TRUE, var.order = "u")

