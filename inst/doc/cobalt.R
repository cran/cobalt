## ---- message = FALSE, eval = 2------------------------------------------
install.packages("cobalt")
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

## ---- message = FALSE----------------------------------------------------
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Generating ATT weights as specified in Austin (2011)
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, 
                       family = "binomial")$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights,
        method = "weighting")


## ---- message = FALSE----------------------------------------------------
bal.tab(f.build("treat", covs0), data = lalonde, weights = "att.weights",
        distance = "p.score", method = "weighting")

## ---- message = FALSE----------------------------------------------------
bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting", 
        binary = "std", continuous = "std")

## ---- message = FALSE----------------------------------------------------
# Balance on all covariates in data set, including interactions and squares
bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting", 
        addl = lalonde[,c("nodegree", "married")], int = TRUE)

## ---- message = FALSE----------------------------------------------------
# Balance tables with variance ratios and statistics for the unadjusted sample
bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting", 
        disp.v.ratio = TRUE, un = TRUE)

## ---- message = FALSE----------------------------------------------------
# Balance tables with thresholds for mean differences and variance ratios
bal.tab(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting", 
        m.threshold = .1, v.threshold = 2)

## ------------------------------------------------------------------------
#Create cluster variable "zone"
lalonde$zone <- sample(LETTERS[1:5], nrow(lalonde), replace = TRUE)
covs.clust <- data.frame(covs0, zone = lalonde$zone)

#Generating ATT weights with zones as model fixed effects
lalonde$p.score.clust <- glm(f.build("treat", covs.clust), data = lalonde, 
                             family = "binomial")$fitted.values
lalonde$att.weights.clust <- with(lalonde, treat + (1-treat)*p.score.clust/(1-p.score.clust))

bal.tab(covs.clust, treat = lalonde$treat, weights = lalonde$att.weights.clust, 
        method = "weighting", cluster = lalonde$zone, which.cluster = c("A", "B"), 
        cluster.summary = TRUE)

## ------------------------------------------------------------------------
# Subclassification for ATT with 6 subclasses
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")$fitted.values
lalonde$subclass <- findInterval(lalonde$p.score, 
                                 quantile(lalonde$p.score[lalonde$treat==1], (0:6)/6), all.inside = T)

bal.tab(covs0, treat = lalonde$treat, subclass = lalonde$subclass, 
        method = "subclassification", disp.subclass = TRUE)

## ---- message = FALSE----------------------------------------------------
data("lalonde", package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build("treat", covs0), data = lalonde, method = "nearest", replace = TRUE)

bal.tab(m.out)

## ---- message = FALSE, warning = FALSE-----------------------------------
library("twang")
data("lalonde", package = "cobalt") ##If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

ps.out <- ps(f.build("treat", covs0), data = lalonde, stop.method = c("es.mean",
             "es.max"), estimand = "ATT", n.trees = 1000, verbose = FALSE)
bal.tab(ps.out, full.stop.method = "es.mean.att")

## ---- message = FALSE----------------------------------------------------
library("Matching")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

fit <- glm(f.build("treat", covs0), data = lalonde, family = "binomial")
p.score <- fit$fitted.values
match.out <- Match(Tr = lalonde$treat, X = p.score, estimand = "ATT")

bal.tab(match.out, formula = f.build("treat", covs0), data = lalonde)

## ---- eval = FALSE-------------------------------------------------------
#  bal.tab(match.out, treat = lalonde$treat, covs = covs0)

## ---- message = FALSE----------------------------------------------------
library("CBPS")
data("lalonde", package = "cobalt") #If not yet loaded
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

#Generating covariate balancing propensity score weights for ATT
cbps.out <- CBPS(f.build("treat", covs0), data = lalonde, standardize = FALSE)

bal.tab(cbps.out)

## ---- message = FALSE, fig.show = "hold"---------------------------------
data("lalonde", package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Generating ATT weights as specified in Austin (2011)
lalonde$p.score <- glm(f.build("treat", covs0), data = lalonde, 
                       family = "binomial")$fitted.values
lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))

bal.plot(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting",
         var.name = "age")
bal.plot(covs0, treat = lalonde$treat, weights = lalonde$att.weights, method = "weighting",
         var.name = "black")

## ---- message = FALSE----------------------------------------------------
library("ggplot2")
bp <- bal.plot(m.out, "age")
bp + theme_bw() + scale_fill_manual( values = c("black","white")) + 
    labs(title = "Distributional Balance for Age", x = "Age")

## ---- fig.show = "hold"--------------------------------------------------
#Before weighting; un = TRUE
bal.plot(f.build("treat", covs0), data = lalonde, var.name = ".distance",
         weights = "att.weights", distance = "p.score", 
         method = "weighting", un = TRUE)

#After weighting
bal.plot(f.build("treat", covs0), data = lalonde, var.name = ".distance",
         weights = "att.weights", distance = "p.score", 
         method = "weighting", un = FALSE)

## ---- fig.width = 5------------------------------------------------------
data("lalonde", package = "cobalt")
covs0 <- subset(lalonde, select = -c(treat, re78, nodegree, married))

# Nearest neighbor 1:1 matching with replacement
library("MatchIt") #if not yet loaded
m.out <- matchit(f.build("treat", covs0), data = lalonde, method = "nearest", replace = TRUE)

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
data("lalonde", package = "cobalt")
library("CBPS")
cov.c <- subset(lalonde, select = -c(treat, re78, re75))

#Generating propensity scxores with re75 as the continuous treatment
cbps.c <- CBPS(f.build("re75", cov.c), data = lalonde, standardize = FALSE)

## ------------------------------------------------------------------------
#Assessing balance numerically
bal.tab(cbps.c, un = TRUE, r.threshold = .1, int = TRUE)

## ---- fig.show = "hold"--------------------------------------------------
#Assessing balance graphically
bal.plot(cbps.c, "re74", un = T) #Clear dependence
bal.plot(cbps.c, "re74")         #Balance improvement

## ---- fig.show = "hold"--------------------------------------------------
bal.plot(cbps.c, "married", un = T) #Clear dependence
bal.plot(cbps.c, "married")         #Remaining dependence, even though numerical
                                    #Summary indicates balance

## ---- fig.width = 5------------------------------------------------------
#Summarizing balance in a Love plot
love.plot(bal.tab(cbps.c), threshold = .1, abs = TRUE, var.order = "unadjusted")

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

