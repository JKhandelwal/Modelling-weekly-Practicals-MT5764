# Libraries
library(tidyverse) # ggplot()
# Read dataset
df <- read.csv("HornsRev.csv")
# Set Impact as factor
df$Impact <- as.factor(df$Impact)
df$Density <-  df$Nhat/df$Area
dim(df)
head(df)

ggplot(df, aes(x=Density, fill=Impact)) + geom_histogram(alpha=0.5) + xlim(c(1,2000))

pre <- df %>% filter(Impact ==0)
post <- df %>% filter(Impact ==1)

# ------------------------------------------------------------------------------
# APPROACH 1

a <- qt(0.01, nrow(pre)-1, lower.tail=FALSE)
b <- qt(0.01, nrow(pre)-1, lower.tail=FALSE)

pre_mean  <- mean(pre$Density)
pre_se <- sd(pre$Density) / sqrt(nrow(pre))
pre_se

cf_l <- pre_mean - a * pre_se
cf_h <- pre_mean + a * pre_se
c(cf_l, cf_h)

post_mean  <- mean(post$Density)
post_se <- sd(post$Density) / sqrt(nrow(post))
post_se

cf_l <- post_mean - b * post_se
cf_h <- post_mean + b * post_se
c(cf_l, cf_h)
# ------------------------------------------------------------------------------
# Approach 2
a <- qnorm(0.01, 0, 1,  lower.tail=FALSE)
b <- qnorm(0.01, 0, 1,  lower.tail=FALSE)
a
pre_mean  <- mean(pre$Density)
pre_se <- sqrt(mean(pre$Density) / nrow(pre))
pre_se

cf_l <- pre_mean - a * pre_se
cf_h <- pre_mean + a * pre_se
c(cf_l, cf_h)

post_mean  <- mean(post$Density)
post_se <- sqrt(mean(post$Density) / nrow(post))
post_se

cf_l <- post_mean - b * post_se
cf_h <- post_mean + b * post_se
c(cf_l, cf_h)
# ------------------------------------------------------------------------------
# Approach 3

a <- qnorm(0.01, 0, 1,  lower.tail=FALSE)
b <- qnorm(0.01, 0, 1,  lower.tail=FALSE)

pre_mean  <- mean(pre$Density)
pre_se <- sd(pre$Density) / sqrt(nrow(pre))
pre_se

cf_l <- pre_mean - a * pre_se
cf_h <- pre_mean + a * pre_se
c(cf_l, cf_h)

post_mean  <- mean(post$Density)
post_se <- sd(post$Density) / sqrt(nrow(post))
post_se

cf_l <- post_mean - b * post_se
cf_h <- post_mean + b * post_se
c(cf_l, cf_h)

# Bootstrap Code

set.seed(145) # set this to reproduce results
NBOOT <- 1000 # no. of bootstrap samples
alpha <- 0.02 # alpha level of confidence

# For pre-imact (Impact=0)
dfPre <- subset(df, Impact==0)
muHat <- sapply(seq(NBOOT),
          function(x) mean(sample(x=dfPre$Nhat/dfPre$Area,
                                  size=nrow(dfPre),
                                  replace=TRUE)))
CI <- quantile(muHat, c(alpha/2, 1-(alpha/2)))
CI
set.seed(145) # set this to reproduce results
dfPost <- subset(df, Impact==1)
muHat <- sapply(seq(NBOOT),
          function(x) mean(sample(x=dfPost$Nhat/dfPost$Area,
                                  size=nrow(dfPost),
                                  replace=TRUE)))
CI <- quantile(muHat, c(alpha/2, 1-(alpha/2)))
CI



# PLots??????

pois1 <- glm(Nhat ~ Impact + offset(log(Area)), data=df, family=poisson)
summary(pois1)
Anova(pois1)

qpois1 <- glm(Nhat ~ Impact + offset(log(Area)), data=df, family=quasipoisson)
summary(qpois1)
Anova(qpois1)


pois2 <- glm(Nhat ~ Impact + Depth + XPos + YPos + offset(log(Area)), data=df, family=poisson)
summary(pois2)
Anova(pois2)

qpois2 <- glm(Nhat ~ Impact + Depth + XPos + YPos + offset(log(Area)), data=df, family=quasipoisson)
summary(qpois2)
Anova(qpois2)


pois3 <- glm(Nhat ~ Impact + Depth + XPos + YPos + offset(log(Area)) + Impact:XPos + Impact:YPos, data=df, family=poisson)
summary(pois3)
Anova(pois3)

qpois3 <- glm(Nhat ~ Impact + Depth + XPos + YPos + offset(log(Area)) + Impact:XPos + Impact:YPos, data=df, family=quasipoisson)
summary(qpois3)
Anova(qpois3)






# lkdmlfk
