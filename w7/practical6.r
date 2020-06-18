# Libraries
library(mgcv) # gam(.)
library(fields) # quilt.plot(.)
library(dplyr)
# Read dataset
df <- read.csv("../HornsRev.csv")
# Set Impact as factor
df$Impact <- as.factor(df$Impact)

pre <- df %>% filter(Impact == 0)
post <- df %>% filter(Impact == 1)
fields::quilt.plot(pre$XPos, pre$YPos, pre$Nhat)
fields::quilt.plot(post$XPos, post$YPos, post$Nhat)


PRS_2D <- mgcv::gam(Nhat ~ s(XPos, YPos) + s(Depth) + Impact,
                    family=quasipoisson, data=df, offset=log(Area))
summary(PRS_2D)

PRS_2DInt <- mgcv::gam(Nhat ~ s(XPos, YPos, by=Impact) + s(Depth) , data=df,
                      family=quasipoisson, offset=log(Area))
summary(PRS_2DInt)

PRS_2DInt <- mgcv::gam(Nhat ~ s(XPos, YPos, by=Impact, k=40) + s(Depth, k=40) , data=df,
                      family=quasipoisson, offset=log(Area))
summary(PRS_2DInt)
