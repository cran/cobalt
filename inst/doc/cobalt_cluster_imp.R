## ---- include = FALSE----------------------------------------------------
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
knitr::opts_chunk$set(fig.width = 5)

## ------------------------------------------------------------------------
library("MatchIt"); library("cobalt")
data("lalonde", package = "cobalt")

m.out <- matchit(treat ~ race*(age + educ + married + nodegree + re74 + re75), 
                 data = lalonde, method = "nearest", exact = "race", 
                 replace = TRUE, ratio = 2)

## ------------------------------------------------------------------------
bal.tab(m.out, cluster = "race")

## ------------------------------------------------------------------------
#Just for black and hispan
bal.tab(m.out, cluster = "race", which.cluster = c("black", "hispan"),
        cluster.summary = FALSE)

#Just the balance summary across clusters
bal.tab(m.out, cluster = "race", which.cluster = NA)

## ------------------------------------------------------------------------
bal.plot(m.out, cluster = "race", var.name = "age")

## ------------------------------------------------------------------------
love.plot(bal.tab(m.out, cluster = "race"), agg.fun = "mean")

## ------------------------------------------------------------------------
love.plot(bal.tab(m.out, cluster = "race"), agg.fun = "range")

## ------------------------------------------------------------------------
love.plot(bal.tab(m.out, cluster = "race", which.cluster = 1:3))

## ------------------------------------------------------------------------
library("MatchIt"); library("cobalt"); library("mice")
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 10 #number of imputed data sets
imp <- mice(lalonde_mis, m = m, print = FALSE) 
imp.data <- complete(imp, "long", include = FALSE)
imp.data <- imp.data[with(imp.data, order(.imp, .id)),]

#Estimate propensity scores and perform matching within each one
ps <- match.weight <- rep(0, nrow(imp.data))
for (i in levels(imp.data$.imp)) {
    in.imp <- imp.data$.imp == i
    ps[in.imp] <- glm(treat ~ age + educ + race + married + nodegree +
                          re74 + re75, data = imp.data[in.imp,], 
                      family = "binomial")$fitted.values
    m.out <- matchit(treat ~ age, data = imp.data[in.imp,], distance = ps[in.imp])
    match.weight[in.imp] <- m.out$weights
}
imp.data <- cbind(imp.data, ps = ps, match.weight = match.weight)

## ------------------------------------------------------------------------
bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
        data = imp.data, weights = "match.weight", method = "matching", 
        imp = ".imp")

## ------------------------------------------------------------------------
bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
        data = imp.data, weights = "match.weight", method = "matching", 
        imp = ".imp", which.imp = 1, imp.summary = FALSE)

## ------------------------------------------------------------------------
#Compute the average propensity for each ID
imp.agg <- aggregate(ps ~ treat + .id, data = imp.data, FUN = mean)
names(imp.agg)[names(imp.agg) == "ps"] <- "ps.ave"

#Perform matching on the aggregated data
m.out.ave <- matchit(treat ~ ps.ave, data = imp.agg, 
                     distance = imp.agg$ps.ave)
imp.agg$match.weight.ave <- m.out.ave$weights

## ------------------------------------------------------------------------
#Merge the data sets; ps.ave and match.weight.ave will remain
imp.data <- merge(imp.data, imp.agg, all.x = TRUE)

bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
        data = imp.data, weights = "match.weight.ave", 
        method = "matching", imp = ".imp")

## ------------------------------------------------------------------------
bal.plot(treat ~ age + educ + race + married + nodegree + re74 + re75, 
         data = imp.data, weights = "match.weight", method = "matching", 
         imp = ".imp", which.imp = 1, var.name = "age")

## ------------------------------------------------------------------------
love.plot(bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                  data = imp.data, weights = "match.weight", method = "matching", 
                  imp = ".imp", which.imp = 1), 
          var.order = "unadjusted", threshold = .2)

## ------------------------------------------------------------------------
love.plot(bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                  data = imp.data, weights = "match.weight", method = "matching", 
                  imp = ".imp"), 
          agg.fun = "range", threshold = .2)

## ------------------------------------------------------------------------
library("MatchIt"); library("cobalt"); library("mice")
data("lalonde_mis", package = "cobalt")

#Generate imputed data sets
m <- 5 #number of imputed data sets
imp <- mice(lalonde_mis, m = m, print = FALSE) 
imp.data <- complete(imp, "long", include = FALSE)

#Estimate propensity scores and perform matching within each one
ps <- match.weight <- rep(0, nrow(imp.data))
for (i in levels(imp.data$.imp)) {
    in.imp <- imp.data$.imp == i
    m.out <- matchit(treat ~ race*(age + educ + married + nodegree + re74 + re75), 
                     data = imp.data[in.imp,], method = "nearest", exact = "race", 
                     replace = TRUE, ratio = 2)
    match.weight[in.imp] <- m.out$weights
}
imp.data <- cbind(imp.data, match.weight = match.weight)

## ------------------------------------------------------------------------
bal.tab(treat ~ age + educ + married + nodegree + re74 + re75, 
        data = imp.data, weights = "match.weight", method = "matching", 
        imp = ".imp", cluster = "race")

## ------------------------------------------------------------------------
bal.plot(treat ~ age + educ + married + nodegree + re74 + re75, 
         data = imp.data, weights = "match.weight", method = "matching", 
         imp = ".imp", cluster = "race", which.imp = 1, 
         which.cluster = NULL, var.name = "age")

## ------------------------------------------------------------------------
#2)
love.plot(bal.tab(treat ~ age + educ + married + nodegree + re74 + re75, 
                  data = imp.data, weights = "match.weight", 
                  method = "matching", imp = ".imp", cluster = "race", 
                  which.imp = NULL, which.cluster = 1:3), 
          agg.fun = "range")

#4)
love.plot(bal.tab(treat ~ age + educ + married + nodegree + re74 + re75, 
                  data = imp.data, weights = "match.weight", 
                  method = "matching", imp = ".imp", cluster = "race", 
                  which.imp = 1, which.cluster = 1:3))

