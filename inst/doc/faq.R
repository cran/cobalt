## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cobalt)

## -----------------------------------------------------------------------------
treat <- rep(1:0, each = 20)
X1 <- c(rep(0:1, c(1, 19)), rep(0:1, c(3, 17)))
X2 <- c(rep(0:1, c(9, 11)), rep(0:1, c(11, 9)))

bal.tab(treat ~ X1 + X2,
        binary = "raw",
        disp = "means",
        s.d.denom = "treated")

## -----------------------------------------------------------------------------
bal.tab(treat ~ X1 + X2,
        binary = "std",
        s.d.denom = "treated")

## -----------------------------------------------------------------------------
data("lalonde")

b <- bal.tab(treat ~ age + educ + race + married + re74,
             data = lalonde, s.d.denom = "treated",
             disp = "means", stats = c("m", "v"))

# View the structure of the object
str(b, give.attr = FALSE)

b$Balance

## -----------------------------------------------------------------------------
# PS Subclassification
msub <- MatchIt::matchit(treat ~ age + educ + race + married + re74,
                         data = lalonde, method = "subclass",
                         estimand = "ATE", min.n = 4)

# Balance in the first subclass
bal.tab(msub, which.sub = 1, binary = "std")

## -----------------------------------------------------------------------------
m0 <- mean(lalonde$age[lalonde$treat == 0 & msub$subclass == 1])
m1 <- mean(lalonde$age[lalonde$treat == 1 & msub$subclass == 1])

s0 <- sd(lalonde$age[lalonde$treat == 0])
s1 <- sd(lalonde$age[lalonde$treat == 1])

(m1 - m0) / sqrt((s1^2 + s0^2) / 2)

## -----------------------------------------------------------------------------
# SMDs across subclasses for age
smds <- sapply(1:6, function(s) {
    m0 <- mean(lalonde$age[lalonde$treat == 0 & msub$subclass == s])
    m1 <- mean(lalonde$age[lalonde$treat == 1 & msub$subclass == s])
    
    s0 <- sd(lalonde$age[lalonde$treat == 0])
    s1 <- sd(lalonde$age[lalonde$treat == 1])
    
    (m1 - m0) / sqrt((s1^2 + s0^2) / 2)
})

# Sample size in each subclass
ns <- table(msub$subclass)

# Summary SMD for age
weighted.mean(smds, ns)

bal.tab(msub)

## -----------------------------------------------------------------------------
# Compute proportion of treated units in each subclass
prop1 <- sapply(1:6, function(s) mean(lalonde$treat[msub$subclass == s]))

# Assign to each unit
ps <- prop1[msub$subclass]

# Compute ATE weights
w <- ifelse(lalonde$treat == 1, 1 / ps, 1 / (1 - ps))

# Compute weighted KS statistic
col_w_ks(lalonde$age, treat = lalonde$treat,
         weights = w)

bal.tab(msub, stats = "ks")

