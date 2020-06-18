# Libraries
library(tidyverse) # ggplot(.)
library("glmnet") # for regularised regression


# Read dataset
df <- read.csv("../HornsRev.csv")
# Set Impact as factor
df$Impact <- as.factor(df$Impact)


glmFitOD3Scale <- glm(Nhat ~ Impact*scale(XPos) + Impact*scale(YPos)
+ scale(Depth), offset=log(Area), family=quasipoisson, data=df)


xmatrix <- model.matrix(glmFitOD3Scale)
xmatrix <- xmatrix[, 2:ncol(xmatrix)]
head(xmatrix)

# ----------------------------------------------------------------------------
ridge <- glmnet(xmatrix, df$Nhat, family="poisson", offset=log(df$Area), alpha=0)
cvridge <- cv.glmnet(xmatrix, df$Nhat, family="poisson",
offset=log(df$Area), alpha=0, nfolds=10)

par(mfrow=c(1, 2))
plot(ridge, xvar="lambda")
abline(v=log(cvridge$lambda.min))
plot(cvridge)
abline(v=log(cvridge$lambda.min))
abline(v=log(cvridge$lambda.1se), lty=2)




log(cvridge$lambda)
log(cvridge$lambda.min)


# quasi-Poisson fit
coefGLM <- as.data.frame(coef(glmFitOD3Scale))
colnames(coefGLM) <- "GLM"
coefGLM$Covariate <- row.names(coefGLM)
# Add 95% confidence intervals
confInt <- as.data.frame(confint(glmFitOD3Scale, level=0.95))
colnames(confInt) <- c("CI_Lower", "CI_Upper")
confInt$Covariate <- row.names(confInt)
# Merge together
coefGLM <- dplyr::inner_join(coefGLM, confInt, by="Covariate")
# Ridge regression
coefRidge <- as.data.frame(as.matrix(coef(cvridge, s="lambda.min")))
colnames(coefRidge) <- "Ridge"
coefRidge$Covariate <- row.names(coefRidge)
# Merge data frames
mdlCoefs <- dplyr::inner_join(coefGLM, coefRidge, by="Covariate")
# Display differences
print(mdlCoefs)


# Plot (ignore intercept term just for scaling reasons) # blue = GLM (with CIs)
# red = Ridge regression
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
col="#377eb8") + geom_point(aes(x=Covariate, y=Ridge), col="#e41a1c") +
ylab("Compare GLM to ridge coefficients") + theme(axis.text.x=element_text(angle=90),
legend.position="none")




 ifelse(mdlCoefs$Ridge > mdlCoefs$CI_Lower & mdlCoefs$Ridge < mdlCoefs$CI_Upper, TRUE, FALSE)


# ----------------------------------------------------------------------- Lasso
 LASSO <- glmnet(xmatrix, df$Nhat, family="poisson", offset=log(df$Area), alpha=1)
 cvLASSO <- cv.glmnet(xmatrix, df$Nhat, family="poisson",
 offset=log(df$Area), alpha=1, nfolds=10)


 par(mfrow=c(1, 2))
 plot(LASSO, xvar="lambda")
 abline(v=log(cvLASSO$lambda.min))
 plot(cvLASSO)
 abline(v=log(cvLASSO$lambda.min))
 abline(v=log(cvLASSO$lambda.1se), lty=2)



 log(cvLASSO$lambda)
 log(cvLASSO$lambda.min)


 # quasi-Poisson fit
 coefGLM <- as.data.frame(coef(glmFitOD3Scale))
 colnames(coefGLM) <- "GLM"
 coefGLM$Covariate <- row.names(coefGLM)
 # Add 95% confidence intervals
 confInt <- as.data.frame(confint(glmFitOD3Scale, level=0.95))
 colnames(confInt) <- c("CI_Lower", "CI_Upper")
 confInt$Covariate <- row.names(confInt)
 # Merge together
 coefGLM <- dplyr::inner_join(coefGLM, confInt, by="Covariate")
 # LASSO regression
 coefLASSO <- as.data.frame(as.matrix(coef(cvLASSO, s="lambda.min")))
 colnames(coefLASSO) <- "LASSO"
 coefLASSO$Covariate <- row.names(coefLASSO)
 # Merge data frames
 mdlCoefs <- dplyr::inner_join(coefGLM, coefLASSO, by="Covariate")
 # Display differences
 print(mdlCoefs)



 # Plot (ignore intercept term just for scaling reasons) # blue = GLM (with CIs)
 # red = LASSO regression
 ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
 geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
 col="#377eb8") + geom_point(aes(x=Covariate, y=LASSO), col="#e41a1c") +
 ylab("Compare GLM to LASSO coefficients") + theme(axis.text.x=element_text(angle=90),
 legend.position="none")


ifelse(mdlCoefs$LASSO > mdlCoefs$CI_Lower & mdlCoefs$LASSO < mdlCoefs$CI_Upper, TRUE, FALSE)


# ---------------------------------------------------------------------------- Elastic net

ElasticNet <- glmnet(xmatrix, df$Nhat, family="poisson", offset=log(df$Area), alpha=0.6)
cvElasticNet <- cv.glmnet(xmatrix, df$Nhat, family="poisson",
offset=log(df$Area), alpha=0.6, nfolds=10)


par(mfrow=c(1, 2))
plot(ElasticNet, xvar="lambda")
abline(v=log(cvElasticNet$lambda.min))
plot(cvElasticNet)
abline(v=log(cvElasticNet$lambda.min))
abline(v=log(cvElasticNet$lambda.1se), lty=2)




log(cvElasticNet$lambda)
log(cvElasticNet$lambda.min)


# quasi-Poisson fit
coefGLM <- as.data.frame(coef(glmFitOD3Scale))
colnames(coefGLM) <- "GLM"
coefGLM$Covariate <- row.names(coefGLM)
# Add 95% confidence intervals
confInt <- as.data.frame(confint(glmFitOD3Scale, level=0.95))
colnames(confInt) <- c("CI_Lower", "CI_Upper")
confInt$Covariate <- row.names(confInt)
# Merge together
coefGLM <- dplyr::inner_join(coefGLM, confInt, by="Covariate")
# ElasticNet regression
coefElasticNet <- as.data.frame(as.matrix(coef(cvElasticNet, s="lambda.min")))
colnames(coefElasticNet) <- "ElasticNet"
coefElasticNet$Covariate <- row.names(coefElasticNet)
# Merge data frames
mdlCoefs <- dplyr::inner_join(coefGLM, coefElasticNet, by="Covariate")
# Display differences
print(mdlCoefs)



# Plot (ignore intercept term just for scaling reasons) # blue = GLM (with CIs)
# red = ElasticNet regression
ggplot(subset(mdlCoefs, !Covariate %in% "(Intercept)")) +
geom_point(aes(x=Covariate, y=GLM), col="#377eb8") + geom_linerange(aes(x=Covariate, ymin=CI_Lower, ymax=CI_Upper),
col="#377eb8") + geom_point(aes(x=Covariate, y=ElasticNet), col="#e41a1c") +
ylab("Compare GLM to ElasticNet coefficients") + theme(axis.text.x=element_text(angle=90),
legend.position="none")




ifelse(mdlCoefs$ElasticNet > mdlCoefs$CI_Lower & mdlCoefs$ElasticNet < mdlCoefs$CI_Upper, TRUE, FALSE)


mdlCoefsFake = data.frame("GLM"=coefGLM$GLM, "CI_Lower"=confInt$CI_Lower,
                        "CI_Upper"=confInt$CI_Upper, "Ridge"=coefRidge$Ridge,
                        "LASSO"=coefLASSO$LASSO, "ElasticNet"=coefElasticNet$ElasticNet,
                        "Covariate"=confInt$Covariate)
mdlCoefsFake


glmCoef <- mdlCoefsFake[, c("GLM", "CI_Lower", "CI_Upper", "Covariate")]
 # Remove GLM results from mdlCoefsFake
 # Note: the function "select" conflicts with the one in the MASS package
 # which is loaded by glmnet so you have to be explicit when calling this function
 mdlCoefsFake <- mdlCoefsFake %>% dplyr::select(-c("GLM", "CI_Lower", "CI_Upper"))
 head(mdlCoefsFake)


mdlCoefsFake <- mdlCoefsFake %>% gather(-Covariate, key="Model", value="Coef")
head(mdlCoefsFake)

ggplot() +
geom_pointrange(aes(x=Covariate, y=GLM, ymin=CI_Lower, ymax=CI_Upper),
col="black", data=glmCoef) + geom_point(aes(x=Covariate, y=Coef, col=Model), data=mdlCoefsFake,
position=position_dodge(width=0.5)) + ylab("Comparing regression coefficients") +
theme(axis.text.x=element_text(angle=90))
