## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, fig.width=6, fig.height = 4)
if (any(!sapply(c("WeightIt", "CBPS"), requireNamespace, quietly = TRUE))) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library(cobalt)
data("lalonde", package = "cobalt")

w.out1 <- WeightIt::weightit(
    treat ~ age + educ + married + nodegree + race + re74 + re75,
    data = lalonde, estimand = "ATE", method = "ps")

## -----------------------------------------------------------------------------
set.cobalt.options(binary = "std")

## -----------------------------------------------------------------------------
love.plot(w.out1)

## ---- eval = FALSE------------------------------------------------------------
#  #This produces the same output as the prior block but with
#  #the additional covariates included in the formula.
#  love.plot(treat ~ age + educ + married + nodegree + race + re74 + re75 +
#              I(age^2) + I(educ^2), data = lalonde, weights = get.w(w.out1),
#            method = "weighting", estimand = "ATE")

## -----------------------------------------------------------------------------
love.plot(w.out1, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1))

## -----------------------------------------------------------------------------
new.names <- c(age = "Age (Years)",
               educ = "Education (Years)",
               married = "Married (Y/N)",
               nodegree = "Degree Earned (Y/N)",
               race_white = "Race: White",
               race_black = "Race: Black",
               race_hispan = "Race: Hispanic",
               re74 = "Earnings in 1974 ($)",
               re75 = "Earnings in 1975 ($)"
)

## -----------------------------------------------------------------------------
love.plot(w.out1, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = new.names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"))

## -----------------------------------------------------------------------------
library(ggplot2)

love.plot(w.out1, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = new.names,
          colors = c("red", "blue"),
          shapes = c("triangle filled", "circle filled"),
          sample.names = c("Unweighted", "PS Weighted"),
          limits = c(0, .82),
          position = c(.75, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

## -----------------------------------------------------------------------------
w.out2 <- WeightIt::weightit(
    treat ~ age + educ + married + nodegree + race + re74 + re75,
    data = lalonde, estimand = "ATE", method = "cbps")

love.plot(treat ~ age + educ + married + nodegree + race + re74 + re75,
          data = lalonde, estimand = "ATE",
          weights = list(w1 = get.w(w.out1),
                         w2 = get.w(w.out2)),
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = new.names,
          colors = c("red", "blue", "darkgreen"),
          shapes = c("triangle filled", "circle filled", "square filled"),
          sample.names = c("Unweighted", "PS Weighted", "CBPS Weighted"),
          limits = c(0, .82)) +
  theme(legend.position = c(.75, .3),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

## -----------------------------------------------------------------------------
love.plot(treat ~ age + educ + married + nodegree + race + re74 + re75,
          data = lalonde, estimand = "ATE",
          stats = "ks.statistics",
          weights = list(w1 = get.w(w.out1),
                         w2 = get.w(w.out2)),
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = new.names,
          colors = c("red", "blue", "darkgreen"),
          shapes = c("triangle filled", "circle filled", "square filled"),
          sample.names = c("Unweighted", "PS Weighted", "CBPS Weighted"),
          limits = c(0, .45)) +
  theme(legend.position = c(.75, .25),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

## -----------------------------------------------------------------------------
love.plot(treat ~ age + educ + married + nodegree + race + re74 + re75,
          data = lalonde, estimand = "ATE",
          stats = c("mean.diffs", "ks.statistics"),
          weights = list(w1 = get.w(w.out1),
                         w2 = get.w(w.out2)),
          var.order = "unadjusted",
          abs = TRUE,
          line = FALSE, 
          thresholds = c(m = .1, ks = .05),
          var.names = new.names,
          colors = c("red", "blue", "darkgreen"),
          shapes = c("triangle filled", "circle filled", "square filled"),
          sample.names = c("Unweighted", "PS Weighted", "CBPS Weighted"),
          limits = list(m = c(0, .82),
                        ks = c(0, .45)),
          wrap = 20,
          position = "top") 

## -----------------------------------------------------------------------------
love.plot(w.out1, abs = FALSE,
          stats = c("mean.diffs", "variance.ratios"),
          drop.distance = TRUE,
          var.names = new.names,
          thresholds = c(v = 2),
          limits = list(m = c(-.9, .9),
                        v = c(.3, 6)),
          shapes = c("circle filled", "circle"),
          position = "none",
          labels = TRUE,
          title = NULL,
          wrap = 20,
          themes = list(v = theme(legend.position = c(.75, 1.09), 
                                  legend.title = element_blank(), 
                                  legend.key.size = unit(.02, "npc"))))


## -----------------------------------------------------------------------------
love.plot(w.out1, binary = "raw",
          stars = "raw",
          drop.distance = TRUE,
          var.names = new.names)

