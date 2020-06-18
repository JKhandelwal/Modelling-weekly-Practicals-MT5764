# Libraries
library(mgcv) # penalised regression splines
library(MRSea) # SALSA
# Read simulated dataset from Moodle
df <- read.csv(file.path("SimulatedData.csv"))


ASE <- function(yTruth, yFit) {
# ASE: Compute the mean average squared error
# yObs: the observed response y
# yFit: the fitted response y
return(mean((yTruth - yFit)^2))
}


RSS <- function(yObs, yFit) {
# RSS: Compute the residual sums of squares
# yObs: the observed response y
# yFit: the fitted response y
return(sum((yObs - yFit)^2))
}

poly_result_ASE =c()
poly_result_RSS =c()

plot(df$x, df$response, pch=16, col="grey")
lines(df$x,df$mu, lwd=2)

for (i in seq(100)) {
    # Subset dataset
    dfSub <- subset(df, ID %in% i) # Fit initial NULL model
    model <- lm(response ~ stats::poly(x,6), data=dfSub)

    poly_result_ASE = c(poly_result_ASE, ASE(dfSub$mu, fitted(model)))
    poly_result_RSS = c(poly_result_RSS, RSS(dfSub$response, fitted(model)))

    lines(dfSub$x,model$fitted.values, col=2)
}


splines_result_ASE =c()
splines_result_RSS =c()

plot(df$x, df$response, pch=16, col="grey")
lines(df$x,df$mu, lwd=2)
for (i in seq(100)) {
    # Subset dataset
    dfSub <- subset(df, ID %in% i) # Fit initial NULL model
    model <- mgcv::gam(response ~ s(x), data=dfSub)

    splines_result_ASE = c(splines_result_ASE, ASE(dfSub$mu, fitted(model)))
    splines_result_RSS = c(splines_result_RSS, RSS(dfSub$response, fitted(model)))
    lines(dfSub$x,model$fitted.values, col=2)
}


salsa_result_ASE =c()
salsa_result_RSS =c()

plot(df$x, df$response, pch=16, col="grey")
lines(df$x,df$mu, lwd=2)
for (i in seq(100)) {
    # Subset dataset
    dfSub <- subset(df, ID %in% i) # Fit initial NULL model
    initialModel <- glm(response ~ 1, data=dfSub)
    # Set SALSA arguments
    varList <- c("x")
    salsa1DList <- list(fitnessMeasure="BIC",
                        minKnots_1d=2, maxKnots_1d=40,
                        startKnots_1d=10, degree=2,
                        maxIterations=10, gaps=0)
    # Run SALSA
    salsa <- MRSea::runSALSA1D(initialModel=initialModel, salsa1dlist=salsa1DList,
                              varlist=varList, factorlist=NULL, datain=dfSub,
                              splineParams=NULL, suppress.printout=TRUE)


    salsa_result_ASE = c(salsa_result_ASE, ASE(dfSub$mu, salsa$bestModel$fitted.values))
    salsa_result_RSS = c(salsa_result_RSS, RSS(dfSub$response, salsa$bestModel$fitted.values))

    lines(dfSub$x,salsa$bestModel$fitted.values, col=2)
}



Poly_ASE = poly_result_ASE
Poly_RSS = poly_result_RSS

PRS_ASE = splines_result_ASE
PRS_RSS = splines_result_RSS

salsa_ASE = salsa_result_ASE
salsa_RSS = salsa_result_RSS


salsa_ASE <- salsa_ASE[101:200]
salsa_RSS <- salsa_RSS[101:200]


mean(Poly_ASE)
mean(Poly_RSS)

mean(PRS_ASE)
mean(PRS_RSS)

mean(salsa_ASE)
mean(salsa_RSS)
