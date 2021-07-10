###################################################
### code chunk number 33: dismap.Rnw:1246-1247
###################################################
library(INLA)


###################################################
### code chunk number 34: dismap.Rnw:1249-1250 
###################################################
inla.version()


###################################################
### code chunk number 36: dismap.Rnw:1256-1260
###################################################
pginla <- inla(Observed ~ offset(log(Expected)) -1 + f(AREAID, model = "iid"),
   family = "poisson",  data = as(nc, "data.frame"),
   control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE)
)


###################################################
### code chunk number 59: dismap.Rnw:1874-1879
###################################################
INLA_BYM <- inla(Observed ~ nwprop + f(FIPS, model="iid") + 
  f(AREAID, model="besag", graph=nb2mat(ncCR85, style="B")) + 
  offset(log(Expected)), 
  family="poisson", data=as(nc, "data.frame"), 
  control.predictor=list(compute=TRUE))
nc$INLA <- INLA_BYM$summary.fitted.values[,1]/nc$Expected


###################################################
### code chunk number 97: dismap.Rnw:2960-2964
###################################################
library(INLA)
hyper1 <- list(prec=list(param=c(.001, .001)))
form <- Observed~1+IDLANLre + f(Year, model="rw1", hyper = list(prec=list(param=c(.001,0.001)))) + f(ID, model="besag", graph=nb2mat(neib), hyper = hyper1)
inlares <- inla(form, family="poisson", data=slot(brainst, "data"), E=Expected, control.predictor=list(compute=TRUE), control.results=list(return.marginals.predictor=TRUE))


###################################################
### code chunk number 104: dismap.Rnw:3128-3138 (eval = FALSE)
###################################################
## #Brain cancer in New Mexico: Spatial effects (no covariates)
## form2 <- Observed~1+f(Year, model="rw1")+f(ID, model="besag", graph=nb2mat(neib))
## 
## inlares2 <- inla(form2, family="poisson", data=slot(brainst, "data"),
##    E=Expected,
## control.predictor=list(compute=TRUE),
## #   quantiles=qnts,
##    control.results=list(return.marginals.predictor=TRUE)
## )
## nmf$CAR2 <- inlares2$summary.random$ID[,2]


