# Libraries
library(tidyverse) # ggplot(.)
library(car) # vif(.)
library(lawstat) # runs.test(.)
# Read dataset
df <- read.csv("../HornsRev.csv")
# Set Impact as factor
df$Impact <- as.factor(df$Impact)
glmFitOD2 <- glm(Nhat ~ Impact + Depth + XPos + YPos, offset=log(Area), family=quasipoisson, data=df)
summary(glmFitOD2)
glmFitOD3 <- glm(Nhat ~ Impact*XPos + Impact*YPos + Depth, offset=log(Area), family=quasipoisson, data=df)
summary(glmFitOD3)





covariates <- c("XPos", "YPos", "Depth")
pairs(subset(df, select=covariates),upper.panel=NULL, pch=19, cex=0.3)


car::vif(glmFitOD2)
car::vif(glmFitOD3)


xmatrix <- model.matrix(glmFitOD3)
# which is equivalent to
xmatrix <- model.matrix(~Impact*XPos + Impact*YPos + Depth, data=df)
xmatrix


set.seed(101) # for reproducibility
testVals <- rnorm(50)

plot(sign(testVals), type="l")


 plot(sign(residuals(glmFitOD3, type="pearson")[1:800]), type="l", ylab="Sign of the residuals")


lawstat::runs.test(residuals(glmFitOD3, type="pearson"))

residualPlots(glmFitOD3, type="pearson",
terms=~Depth,
quadratic=TRUE,
smooth=list(smoother=gamLine, col="#377eb8"), fitted=FALSE,
col.quad="#e41a1c",
col="grey",
pch=19,
cex=0.3,
ylim=c(-20, 20))


lawstat::runs.test(residuals(glmFitOD3, type="pearson")[order(df$Depth)])



residualPlots(glmFitOD3, type="pearson",
terms=~XPos,
quadratic=TRUE,
smooth=list(smoother=gamLine, col="#377eb8"), fitted=FALSE,
col.quad="#e41a1c",
col="grey",
pch=19,
cex=0.3,
ylim=c(-20, 20))


lawstat::runs.test(residuals(glmFitOD3, type="pearson")[order(df$XPos)])


residualPlots(glmFitOD3, type="pearson",
terms=~YPos,
quadratic=TRUE,
smooth=list(smoother=gamLine, col="#377eb8"), fitted=FALSE,
col.quad="#e41a1c",
col="grey",
pch=19,
cex=0.3,
ylim=c(-20, 20))


lawstat::runs.test(residuals(glmFitOD3, type="pearson")[order(df$YPos)])
