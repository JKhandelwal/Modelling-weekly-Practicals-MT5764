# Libraries
library(tidyverse) # ggplot(.)
library(mgcv) # gam(.)
library(splines) # bs(.) (B-splines)
library(MuMIn) # dredge(.)
library(fields) # quilt.plot(.)
library(lawstat) # runs.test(.)
# Read dataset
df <- read.csv("../HornsRev.csv")
# Set Impact as factor
df$Impact <- as.factor(df$Impact)

PRS <- mgcv::gam(Nhat ~ s(XPos) + s(YPos) + s(Depth) + Impact, data=df, family=quasipoisson, offset=log(Area))
summary(PRS)



par(mfrow=c(1,2))
# Replace s(Depth) with bs(Depth, knots=20) in the model
PRS_B <- stats::update(PRS, .~. -s(Depth) + splines::bs(Depth, knots=20))
summary(PRS_B)
stats::termplot(PRS_B, se=T)


options(na.action="na.fail") # fail-safe
head(dredge(PRS, rank="QAIC", chat=summary(PRS)$dispersion))



predData <- read.csv("../HornsRevPredictionData.csv")

# Predict on the link scale
NhatPredLink <- predict(PRS, newdata=predData, se=T, type="link")
# Predict on the response scale
NhatPredRes <- predict(PRS, newdata=predData, se=T, type="response")


PRS_Int <- mgcv::gam(Nhat ~ s(XPos, by=Impact) + s(YPos) + s(Depth) + Impact, data=df, family=quasipoisson, offset=log(Area))
summary(PRS_Int)



NhatPredLink_1 <- predict(PRS_Int, newdata=predData, se=T, type="link")
# Predict on the response scale
NhatPredRes_1 <- predict(PRS_Int, newdata=predData, se=T, type="response")


runs.test(resid(PRS_Int, type='pearson'))
