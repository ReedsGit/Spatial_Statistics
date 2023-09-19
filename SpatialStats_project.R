library(gstat)
library(sp)
library(dplyr)
library(ggplot2)
library(patchwork)
library(latticeExtra)
library(nimble)
library(rgdal)
library(sf)
library(spdep)
library(GGally)
library(coda)

#Read in data
load("scot_coords.RData")
polts <- read.csv("Pollutants.csv")
adm <- read.csv("Admissions.csv")

#Organising data
#remove PM10 data
polts <- select(polts,-PM10) 
summary(polts)
nrow(polts)
#filter for just the data from 2012
polts <- filter(polts, year==2012) 
nrow(polts)
summary(polts)

nrow(adm)
adm <- filter(adm, Year==2012)
nrow(adm)
summary(adm)

###
#Look at data on map
ggplot(polts)+
  #add border for scotland
geom_polygon(data=as.data.frame(scot_coords), aes(scot_coords[,1], scot_coords[,2]),
             fill="white", colour="black")+
  geom_point(aes(x,y, colour=NO2))+
  scale_colour_viridis_c()+
  coord_equal()+
  ggtitle("Location Map")+
  # There is an observation that is not from Scotland
  scale_y_continuous(limits=range(scot_coords[,2])) +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank()) -> p1

ggplot(polts)+
  geom_point(aes(x,y, colour=NO2))+
  scale_colour_viridis_c()+
  coord_equal() +
  ggtitle("NO2 observations")-> p2

p1+p2


#Create spatial object
polts.sp <- polts
coordinates(polts.sp) <- c("x", "y")

#Create grid of predictions locations
min(polts$x); max(polts$x)
min(polts$y); max(polts$y)

xseq <- seq(250500, 269500, length.out= 500)
yseq <- seq(656500, 672500, length.out= 500)
xygrid <- expand.grid(xseq=xseq, yseq=yseq)
grid.sp <- xygrid
coordinates(grid.sp) <- c("xseq", "yseq")

#IDW interpolation
idw1 <- idw(NO2~1, polts.sp, grid.sp, idp=1)
idw2 <- idw(NO2~1, polts.sp, grid.sp, idp=5)
idw3 <- idw(NO2~1, polts.sp, grid.sp, idp=10)
#idw4 <- idw(NO2~1, polts.sp, grid.sp, idp=6)
#idw5 <- idw(NO2~1, polts.sp, grid.sp, idp=5)
#idw8 <- idw(NO2~1, polts.sp, grid.sp, idp=8)

xygrid$pred1 <- idw1$var1.pred
xygrid$pred2 <- idw2$var1.pred
xygrid$pred3 <- idw3$var1.pred
#xygrid$pred4 <- idw4$var1.pred
#xygrid$pred5 <- idw4$var1.pred
#xygrid$pred8 <- idw4$var1.pred

ggplot(xygrid)+ 
  geom_raster(aes(xseq, yseq, fill=pred1))+
  scale_fill_viridis_c(option="C", limits=range(xygrid$pred1, xygrid$pred2, 
                                                xygrid$pred3, xygrid$pred4), name="Prediction")+
  coord_equal()+
  ggtitle("p=1") -> ipred1

ggplot(xygrid)+ 
  geom_raster(aes(xseq, yseq, fill=pred2))+
  scale_fill_viridis_c(option="C", limits=range(xygrid$pred1, xygrid$pred2, 
                                                xygrid$pred3, xygrid$pred4), name="Prediction")+
  coord_equal()+
  ggtitle("p=5") -> ipred2

ggplot(xygrid)+ 
  geom_raster(aes(xseq, yseq, fill=pred3))+
  scale_fill_viridis_c(option="C", limits=range(xygrid$pred1, xygrid$pred2, 
                                                xygrid$pred3, xygrid$pred4), name="Prediction")+
  coord_equal()+
  ggtitle("p=10") -> ipred3

#ggplot(xygrid)+ 
 # geom_raster(aes(xseq, yseq, fill=pred4))+
  #scale_fill_viridis_c(option="C", limits=range(xygrid$pred1, xygrid$pred2, 
   #                                             xygrid$pred3, xygrid$pred4), name="Prediction")+
  #coord_equal()+
  #ggtitle("p=6") -> ipred4

ipred1|ipred2|ipred3|plot_layout(guides="collect")

### Consider linear regression for prediction
ggplot(polts)+
  geom_point(aes(x, NO2))+
  ggtitle("easting")->p1a
ggplot(polts)+
  geom_point(aes(y, NO2))+
  ggtitle("northing")->p1b
p1a+p1b

#(Effectively) use backwards selection
mod1 <- lm(NO2 ~ poly(x, 2) + poly(y, 2), data=polts)
summary(mod1)

#Check model residuals
polts$resid <- resid(mod1)
ggplot(polts)+
  geom_point(aes(x,y, colour=resid))+
  scale_colour_viridis_c()+
  coord_equal()+
  ggtitle("Residuals with NO2")->P1  
P1

ggplot(polts)+
  geom_point(aes(x, resid))+ 
ggtitle("Residual Plots")-> p2a
ggplot(polts)+
  geom_point(aes(y, resid)) -> p2b

p2a+p2b

#try log(NO2)
mod2 <- lm(log(NO2) ~ poly(x, 2) + poly(y, 2), data=polts)
summary(mod2)

polts$resid <- resid(mod2)
ggplot(polts)+
  geom_point(aes(x,y, colour=resid))+
  scale_colour_viridis_c()+
  coord_equal()+
  ggtitle("Residuals with log(NO2)")->P2  

P1+P2

#Return to using NO2 model rather than log(NO2)
mod1 <- lm(NO2 ~ poly(x, 2) + poly(y, 2), data=polts)
polts$resid <- resid(mod1)

ggplot(polts)+
  geom_point(aes(x, resid))+ 
  ggtitle("Residual Plots")-> p1
ggplot(polts)+
  geom_point(aes(y, resid)) -> p2

#Check residuals v fitted and qq plot for further assumption check of NO2 model with quad poly terms
ggplot(polts)+
  geom_point(aes(mod1$fitted.values, resid))+
  xlab("fitted values")+
  ggtitle("Residual/Fitted values\nNO2 and quadratic poly covariates")-> pl1

ggplot(polts)+
  geom_qq(aes(sample=resid))+
  geom_qq_line(aes(sample=resid))+
  ggtitle("QQ Plot\nNO2 and quadratic poly covariates")-> pl2

pl1+pl2

mod3 <- lm(log(NO2) ~ poly(x, 3) + poly(y, 3), data=polts)
polts$resid <- resid(mod3)
polts.sp$resid <- resid(mod3)

ggplot(polts)+
  geom_point(aes(x, resid))+ 
  ggtitle("Residual Plots")-> p3
ggplot(polts)+
  geom_point(aes(y, resid)) -> p4

(p1+p2)/(p3+p4)

#Check residuals v fitted and qq plot for further assumption check log(NO2) and cubic poly terms
ggplot(polts)+
  geom_point(aes(mod3$fitted.values, resid))+
  xlab("fitted values")+
  ggtitle("Residual/Fitted values\nlog(NO2) and cubic poly covariates")-> pl3

ggplot(polts)+
  geom_qq(aes(sample=resid))+
  geom_qq_line(aes(sample=resid))+
  ggtitle("QQ Plot\nlog(NO2) and cubic poly covariates")-> pl4

(pl1+pl2)/(pl3+pl4)

### Sample variogram
#set cutoff value to 1/3 max separating value
xran <- range(polts$x)
yran <- range(polts$y)
dist <- sqrt(diff(xran)^2 + diff(yran)^2)
cutoff <- max(dist)/3
cutoff # ~ 8280

#choose bin width
vgm1 <- variogram(log(NO2)~1, polts.sp, cutoff=cutoff, width=300)
vgm2 <- variogram(log(NO2)~1, polts.sp, cutoff=9000, width=300)
vgm3 <- variogram(log(NO2)~1, polts.sp, cutoff=15000, width=300)

ggplot(vgm1)+
  geom_point(aes(dist, gamma))+
  scale_y_continuous(limits=c(0,0.11))+
  ggtitle("cutoff=8280") ->c1
ggplot(vgm2)+
  geom_point(aes(dist, gamma))+
  scale_y_continuous(limits=c(0,0.11))+
  ggtitle("cutoff=9000") ->c2
ggplot(vgm3)+
  geom_point(aes(dist, gamma))+
  scale_y_continuous(limits=c(0,0.11))+
  ggtitle("cutoff=15000") ->c3

c1+c2+c3

#Variogram model - which type - try exponential and spherical first - using vgm2
expon <- fit.variogram(vgm2, vgm(0.8, "Exp",7500, 0.01)) #no convergence
spher <- fit.variogram(vgm2, vgm(0.8, "Sph",7500, 0.01)) #no convergence
gaus <- fit.variogram(vgm2, vgm(0.8, "Gau",7500, 0.01)) #converges

#Check how well variogram model fits to sample variogram
ggplot(vgm2, aes(dist, gamma))+
  geom_point()+
  scale_y_continuous(limits=c(0, 0.125))+
  geom_line(data=variogramLine(gaus, maxdist=9000))+
  ggtitle("Gaussian variogram") -> g1
g1

vgm_resid <- variogram(resid~1, polts.sp, cutoff=9000, width=300)
ggplot(vgm_resid)+
  geom_point(aes(dist, gamma))+
  scale_y_continuous(limits=c(0, NA))+
  ggtitle("Sample variogram of regression model residuals")

max(vgm_resid$gamma) #help work out sill

exponr <- fit.variogram(vgm_resid, vgm(0.017, "Exp",5500, 0.009)) #converges
spherr <- fit.variogram(vgm_resid, vgm(0.017, "Sph",5500, 0.009)) #converges
gausr <- fit.variogram(vgm_resid, vgm(0.017, "Gau",5500, 0.009)) #no convergence

#Check how well variogram model fits to sample variogram
ggplot(vgm_resid, aes(dist, gamma))+
  geom_point()+
  scale_y_continuous(limits=c(0, 0.125))+
  geom_line(data=variogramLine(exponr, maxdist=9000))+
  ggtitle("Exponential variogram - residuals") -> g2
g1+g2

### Kriging ###
#set up no. of folds
nfold <- 10
#set up data partitions
part <- sample(1:nfold, nrow(polts), replace=T)
#set up somewhere to store the results for each model
ok <- numeric(nrow(polts))
uk <- numeric(nrow(polts))
for(i in 1:nfold){
  #extract the partitions
  modelling <- polts.sp[part!=i,]
  test <- polts.sp[part==i,]
  
#Ordinary Kriging
vgm_ok <- variogram(log(NO2)~1, modelling) #fit to modelling set
vfit_ok <- fit.variogram(vgm_ok, vgm(0.08, "Gau", 7500, 0.01))
ok[part==i] <- krige(log(NO2)~1, modelling, test, vfit_ok)$var1.pred

#Universal kriging
vgm_uk <- variogram(log(NO2) ~ poly(x,3) + poly(y, 3), modelling) #fit to modelling set
vfit_uk <- fit.variogram(vgm_uk, vgm(0.017, "Exp",5500, 0.009))
uk[part==i] <- krige(log(NO2) ~ poly(x,3) + poly(y, 3), modelling, test, vfit_uk)$var1.pred
}
#Find R^2 value to assess fit
1-sum((ok-log(polts$NO2))^2)/sum((log(polts$NO2) - mean(log(polts$NO2)))^2) #0.841058
1-sum((uk-log(polts$NO2))^2)/sum((log(polts$NO2) - mean(log(polts$NO2)))^2) #-0.7609823

#Use krige.cv function to check results
#Ordinary Kriging
vgm_okcv <- variogram(log(NO2)~1, polts.sp) 
vfit_okcv <- fit.variogram(vgm_okcv, vgm(0.08, "Gau", 7500, 0.01))
okcv <- krige.cv(log(NO2)~1, polts.sp, vfit_okcv, nfold=10)

1-sum(okcv$residual^2)/sum((log(polts$NO2) - mean(log(polts$NO2)))^2) #0.8354685

#Universal Kriging
vgm_ukcv <- variogram(log(NO2)~1, polts.sp) 
vfit_ukcv <- fit.variogram(vgm_ukcv, vgm(0.08, "Gau", 7500, 0.01))
ukcv <- krige.cv(log(NO2)~1, polts.sp, vfit_ukcv, nfold=10)

1-sum(ukcv$residual^2)/sum((log(polts$NO2) - mean(log(polts$NO2)))^2) #0.843031

#Plot residuals and QQ plot to check assumptions of CV residuals
ggplot(polts)+
  geom_point(aes(x, y, colour=okcv$residual))+
  labs(colour="resid")+
  scale_colour_viridis_c()+
  coord_equal()+
  ggtitle("Model residuals")-> cv1

ggplot()+
  geom_qq(aes(sample=okcv$residual))+
  geom_qq_line(aes(sample=okcv$residual))+
  ggtitle("QQ-plot")-> cv2

cv1+cv2
  
#Make Predictions
#Log NO2 predictions
ok_log_pred <- krige(log(NO2)~1, polts.sp, grid.sp, vfit_okcv)
#Converting to NO2 predictions
ok_pred$var1.pred <- exp(ok_log_pred$var1.pred)

#Plot NO2 predictions
ggplot(xygrid)+
  geom_raster(aes(xseq,yseq, fill=ok_pred$var1.pred))+
  scale_fill_viridis_c(option="C")+
  coord_equal() +
  labs(fill="predictions", x="Easting", y="Northing")+
  ggtitle("Glasgow NO2 Predictions")

########### Part 2 ##################
#Predictions are here: ok_pred$var1.pred

#Read in shapefile
shape <- readOGR("Shapefile.shp")

coord <- coordinates(shape)
coord <- as.data.frame(coord)
names(coord) <- c("x", "y")
coordinates(coord) <- c("x","y") 

#Make predictions with new coordinates
#Log NO2 predictions
log_predictions <- krige(log(NO2)~1, polts.sp, coord, vfit_okcv)
#Converting to NO2 predictions
predictions <- exp(log_predictions$var1.pred)

#add to admissions data then merge both to shape data
adm$NO2 <- predictions
shape@data <- merge(shape@data, adm, by.x="InterZone", by.y="IZ", sort=FALSE)

#Add SIR column to data
shape$SIR <- shape$Respiratory.Prim/shape$Expected

#Plot SIR
shape.sf <- st_as_sf(shape)
ggplot(shape.sf)+
  geom_sf(aes(fill=SIR, colour=SIR))+
  scale_fill_viridis_c(option="C")+
  scale_colour_viridis_c(option="C")+
  theme_void()+
  ggtitle("SIR values of Respiratory Admissions")
  
#Moran's I test for spatial depepndence
moran.test(shape$SIR, nb2listw(poly2nb(shape)), alternative="two.sided")

#Explore relationships between admissions and covariates
ggpairs(shape@data[,c(12:17, 19)])

#Fit a poisson regression model 
#model code
preg_code <- nimbleCode({
  for(i in 1:N){
    Y[i] ~ dpois(mu[i])
    log(mu[i]) <- log(E[i]) + beta0 + beta1*X1[i] + beta2*X2[i] + beta3*X3[i]+ beta4*X4[i]+ beta5*X5[i]+ beta6*X6[i]
  }
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  beta2 ~ dnorm(0,0.01)
  beta3 ~ dnorm(0,0.01)
  beta4 ~ dnorm(0,0.01)
  beta5 ~ dnorm(0,0.01)
  beta6 ~ dnorm(0,0.01)
})

Data <- list(Y =shape$Respiratory.Prim,
             E =shape$Expected,
             X1=shape$employment-mean(shape$employment),
             X2=shape$crime-mean(shape$crime),
             X3=shape$housing-mean(shape$housing),
             X4=shape$access-mean(shape$access),
             X5=shape$House_Price-mean(shape$House_Price),
             X6=shape$NO2-mean(shape$NO2))

Constants <- list(N=nrow(shape))

Inits <- list(list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0),
              list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0),
              list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0))

#Run the model
preg <- nimbleMCMC(data=Data,
                   constants=Constants,
                   code=preg_code,
                   monitors=c(paste0("beta",0:6), "mu"),
                   inits=Inits,
                   nchains=3,
                   niter=10000,
                   nburnin=5000,
                   summary=TRUE,
                   samplesAsCodaMCMC = TRUE)

#subset to remove mu's
for_diag <- lapply(preg$samples, function(x) x[,grepl("beta", names(x[1,]))])

#gelman diagnostic
gelman.diag(for_diag)

#check traceplots
samples <- as.data.frame(Reduce("rbind", preg$samples))

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta0))+
  labs(x="Index") -> p0

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta1))+
  labs(x="Index") -> p1

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta2))+
  labs(x="Index") -> p2

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta3))+
  labs(x="Index") -> p3

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta4))+
  labs(x="Index") -> p4

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta5))+
  labs(x="Index") -> p5

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta6))+
  labs(x="Index") -> p6

p0/p1/p2/p3/p4/p5/p6

#Check poisson model assumptions
mu_preg <- preg$summary$all.chains[grepl("mu", rownames(preg$summary$all.chains)),1]
res_preg <- (Data$Y - mu_preg)/sqrt(mu_preg)

#mean=variance
var(res_preg)

ggplot()+
  geom_point(aes(mu_preg, res_preg))+
  xlab("fitted vallues")+
  ylab("residuals")+
  ggtitle("Poisson Regression: Residual v's fitted values plot")

#independence assumption
moran.test(res_preg, nb2listw(poly2nb(shape)), alternative="two.sided")

#Plot residuals against covariates to see if any missed patterns
ggplot()+
  geom_point(aes(shape$employment, res_preg))+
  xlab("employment")+
  ylab("residuals")+
  ggtitle("Residual plot") -> a1

ggplot()+
  geom_point(aes(shape$crime, res_preg))+
  xlab("crime\n (1 outlier removed)")+
  ylab("residuals")+
  scale_x_continuous(limits = c(0, 1750))+
  ggtitle("Residual plot") -> a2

ggplot()+
  geom_point(aes(shape$housing, res_preg))+
  xlab("housing")+
  ylab("residuals")+
  ggtitle("Residual plot") -> a3

ggplot()+
  geom_point(aes(shape$access, res_preg))+
  xlab("access")+
  ylab("residuals")+
  ggtitle("Residual plot") -> a4

ggplot()+
  geom_point(aes(shape$House_Price, res_preg))+
  xlab("House price")+
  ylab("residuals")+
  ggtitle("Residual plot") -> a5

ggplot()+
  geom_point(aes(shape$NO2, res_preg))+
  xlab("NO2")+
  ylab("residuals")+
  ggtitle("Residual plot") -> a6

(a1+a2+a3)/(a4+a5+a6)

##Finding DIC and pD for Poisson regression
mu <- apply(samples[,grepl("mu", names(samples))], 2, median)
deviance <- sum(log(dpois(shape$Respiratory.Prim, lambda=mu)))
mu_samp <- samples[,grepl("mu", names(samples))]
ave_dev <- mean(
  sapply(1:nrow(samples),
         function(x){sum(log(dpois(shape$Respiratory.Prim, 
                                   lambda=as.numeric(mu_samp[x,]))))}))

pD <- 2*(deviance-ave_dev)
DIC <- -2*deviance+2*pD
pD;DIC

####  Poisson CAR model #######
W <- nb2mat(poly2nb(shape), style="B")
Adj <- lapply(1:nrow(W), function(x) which(W[x,]==1))
Adj <- Reduce("c", Adj)
Num.Adj <- apply(W, 1, sum)
L <- length(Adj)

pCAR_code <- nimbleCode({
  for(i in 1:N){
    Y[i] ~ dpois(mu[i])
    log(mu[i]) <- log(E[i]) + beta0 + beta1*X1[i] + beta2*X2[i] + beta3*X3[i] + beta4*X4[i] + beta5*X5[i] + beta6*X6[i] + phi[i]
  }
  phi[1:N] ~ dcar_normal(Adj[1:L], weights[1:L], Num.Adj[1:N], tau, zero_mean=1)
  tau ~ dgamma(0.01, 0.01)
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  beta2 ~ dnorm(0,0.01)
  beta3 ~ dnorm(0,0.01)
  beta4 ~ dnorm(0,0.01)
  beta5 ~ dnorm(0,0.01)
  beta6 ~ dnorm(0,0.01)
})

#same Data as before can be used

Constants <- list(N=nrow(shape),
                  L=L, 
                  Adj=Adj, 
                  weights=rep(1, L),
                  Num.Adj=Num.Adj)

Inits <- list(list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0, tau=1, phi=rep(1, nrow(shape))),
              list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0, tau=1, phi=rep(1, nrow(shape))),
              list(beta0=0,beta1=0, beta2=0, beta3=0, beta4=0, beta5=0, beta6=0, tau=1, phi=rep(1, nrow(shape))))

#Run CAR model
pCAR <- nimbleMCMC(data=Data,
                   constants=Constants,
                   code=pCAR_code,
                   monitors=c(paste0("beta", 0:6), "phi", "mu"),
                   inits=Inits,
                   nchains=3,
                   niter=10000, 
                   nburnin=5000,
                   summary=TRUE,
                   samplesAsCodaMCMC = TRUE)

#Check convergence
#subset to remove mu's
for_diag <- lapply(pCAR$samples, function(x) x[,grepl("mu", names(x[1,]))])

#gelman diagnostic
gelman.diag(for_diag)

#check traceplots
samples <- as.data.frame(Reduce("rbind", pCAR$samples))

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta0))+
  labs(x="Index") -> n0

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta1))+
  labs(x="Index") -> n1

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta2))+
  labs(x="Index") -> n2

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta3))+
  labs(x="Index") -> n3

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta4))+
  labs(x="Index") -> n4

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta5))+
  labs(x="Index") -> n5

ggplot(samples)+
  geom_line(aes(1:nrow(samples), beta6))+
  labs(x="Index") -> n6

n0/n1/n2/n3/n4/n5/n6

#Check poisson CAR model assumptions
mu_pCAR <- pCAR$summary$all.chains[grepl("mu", rownames(pCAR$summary$all.chains)),1]
res_pCAR <- (Data$Y - mu_pCAR)/sqrt(mu_pCAR)

#mean=variance
var(res_pCAR)

ggplot()+
  geom_point(aes(mu_pCAR, res_pCAR))+
  xlab("fitted values")+
  ylab("residuals")+
  ggtitle("Poisson CAR: Residual v's fitted values plot")

#independence assumption
moran.test(res_pCAR, nb2listw(poly2nb(shape)), alternative="two.sided")

##Finding DIC and pD for Poisson CAR
mu <- apply(samples[,grepl("mu", names(samples))], 2, median)
deviance <- sum(log(dpois(shape$Respiratory.Prim, lambda=mu)))
mu_samp <- samples[,grepl("mu", names(samples))]
ave_dev <- mean(
  sapply(1:nrow(samples),
         function(x){sum(log(dpois(shape$Respiratory.Prim, 
                                   lambda=as.numeric(mu_samp[x,]))))}))

pD <- 2*(deviance-ave_dev)
DIC <- -2*deviance+2*pD
pD;DIC

### Going back to Poisson regression model ###
exp(preg$summary$all.chains[grepl("beta", rownames(preg$summary$all.chains)), c(1,4,5)])



