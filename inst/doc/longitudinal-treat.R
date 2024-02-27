## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, fig.width=5)
if (any(!sapply(c("WeightIt"), requireNamespace, quietly = TRUE))) knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
library("cobalt")
data("msmdata", package = "WeightIt")
head(msmdata)

## -----------------------------------------------------------------------------
bal.tab(list(A_1 ~ X1_0 + X2_0,
             A_2 ~ X1_1 + X2_1 +
                 A_1 + X1_0 + X2_0,
             A_3 ~ X1_2 + X2_2 +
                 A_2 + X1_1 + X2_1 +
                 A_1 + X1_0 + X2_0),
        data = msmdata)

## -----------------------------------------------------------------------------
bal.tab(list(A_1 ~ X1_0 + X2_0,
             A_2 ~ X1_1 + X2_1 +
                 A_1 + X1_0 + X2_0,
             A_3 ~ X1_2 + X2_2 +
                 A_2 + X1_1 + X2_1 +
                 A_1 + X1_0 + X2_0),
        data = msmdata,
        which.time = .all)

## -----------------------------------------------------------------------------
Wmsm <- WeightIt::weightitMSM(
    list(A_1 ~ X1_0 + X2_0,
         A_2 ~ X1_1 + X2_1 +
             A_1 + X1_0 + X2_0,
         A_3 ~ X1_2 + X2_2 +
             A_2 + X1_1 + X2_1 +
             A_1 + X1_0 + X2_0),
    data = msmdata,
    method = "glm")

## -----------------------------------------------------------------------------
bal.tab(Wmsm, un = TRUE, which.time = .all, msm.summary = TRUE)

## ----fig.height=4-------------------------------------------------------------
bal.plot(Wmsm, var.name = "X1_0", which = "both",
         type = "histogram")

## ----fig.height=4-------------------------------------------------------------
bal.plot(Wmsm, var.name = "X2_1", which = "both")

## -----------------------------------------------------------------------------
love.plot(Wmsm, binary = "std")

## -----------------------------------------------------------------------------
love.plot(Wmsm, binary = "std", which.time = .all)

