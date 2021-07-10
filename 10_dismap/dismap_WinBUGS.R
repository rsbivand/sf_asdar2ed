###################################################
### code chunk number 23: dismap.Rnw:971-979
###################################################
#Data loaded from a previous Windows session
#This file contains the output of MCMC simulations of the P-G model
#I have used BRugs and OpenBUGS to get the results
#
load("PG.RData")
#con <- url("http://www.bias-project.org.uk/ASDAR/PG.RData", open="rb")
#load(con)
#close(con)


###################################################
### code chunk number 24: dismap.Rnw:994-997
###################################################
library(R2WinBUGS)
N <- length(nc$Observed)
d <- list(N=N, observed=nc$Observed, expected=nc$Expected)


###################################################
### code chunk number 25: dismap.Rnw:999-1003 (eval = FALSE)
###################################################
pgmodelfile <- paste(getwd(), "/PG-model.txt", sep="")
wdir <- paste(getwd(), "/PG", sep="")
if(!file.exists(wdir)){dir.create(wdir)}
## BugsDir <- "/home/rsb/.wine/dosdevices/c:/Program Files/WinBUGS14"


###################################################
### code chunk number 26: dismap.Rnw:1011-1018 (eval = FALSE)
###################################################
MCMCres <- bugs(data = d, inits = list(list(nu = 1, alpha = 1)), 
  working.directory = wdir,
  parameters.to.save = c("theta", "nu", "alpha"),
  n.chains = 1, n.iter = 20000, n.burnin = 10000, n.thin = 10,
  model.file = pgmodelfile,
##  bugs.directory = BugsDir,
##   WINEPATH = "/usr/bin/winepath"
)


###################################################
### code chunk number 27: dismap.Rnw:1051-1053
###################################################
nc$PGmean <- MCMCres$mean$theta
nc$PGmedian <- MCMCres$median$theta


###################################################
### code chunk number 28: dismap.Rnw:1112-1136
###################################################
plot(1,1, type="n", xlim=c(1,100), ylim=c(0,4),
  main= "Credible intervals of the relative risks",
  xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)

for(i in 1:100) {
	if(MCMCres$summary[i,3]>1 ) {
		col <- sig.col #gray(.4)
		lty <- 2
		text(i, MCMCres$summary[i,7]+.31, nc$NAME[i], 
       		srt=90, col=sig.col, cex=.85)
	} else {
		col <- "black"
        lty <- 1
	}
	lines(c(i,i), c(MCMCres$summary[i,3], MCMCres$summary[i,7]), col=col, lty=lty)
	points(i, MCMCres$median$theta[i], pch=18, col=col)
}


###################################################
### code chunk number 29: dismap.Rnw:1158-1172
###################################################

print(spplot(nc, c("SMR","EBPG", "PGmean", "PGmedian"),
  col.regions=cols,  at=brks, axes = TRUE))#, colorkey=colorkey))


###################################################
### code chunk number 43: dismap.Rnw:1485-1492 (eval = FALSE)
###################################################
## idx <- match(attr(ncCR85.nb, "region.id"), nc$"CNTY_ID")
## nc.nb <- ncCR85
## nc.nb <- nc.nb[order(idx)]
## nc.nb <- lapply(nc.nb, function(X, idx){idx[X]}, idx=(idx))
## class(nc.nb) <- "nb"
## nc.nb <- nc.nb[(order(idx))]
## nc.nb <- nb2WB(nc.nb)


###################################################
### code chunk number 44: dismap.Rnw:1516-1517
###################################################
nc.nb <- nb2WB(ncCR85)


###################################################
### code chunk number 46: dismap.Rnw:1530-1541
###################################################
nc$nwprop <- nc$NWBIR74/nc$BIR74

d <- list(N=N, observed=nc$Observed, expected=nc$Expected,
  nonwhite=nc$nwprop,
  adj=nc.nb$adj,  weights=nc.nb$weights, num=nc.nb$num)

dwoutcov <- list(N=N, observed=nc$Observed, expected=nc$Expected,
  adj=nc.nb$adj,  weights=nc.nb$weights, num=nc.nb$num)

inits <- list(u=rep(0,N), v=rep(0,N), alpha=0, beta=0, precu=.001, precv=.001)
#inits$v[d$num==0] <- NA


###################################################
### code chunk number 48: dismap.Rnw:1565-1571 (eval = FALSE)
###################################################
## 
## 
## bymmodelfile <- paste(getwd(), "/BYM-model.txt", sep="")
## wdir <- paste(getwd(), "/BYM", sep="")
## if(!file.exists(wdir)){dir.create(wdir)}
## BugsDir <- "/home/rsb/.wine/dosdevices/c:/Program Files/WinBUGS14"


###################################################
### code chunk number 50: dismap.Rnw:1576-1583 (eval = FALSE)
###################################################
## MCMCres<- bugs(data = d, inits = list(inits),
##   working.directory = wdir,
##   parameters.to.save = c("theta", "alpha", "beta", "u", "v", "sigmau", "sigmav"),
##   n.chains = 1, n.iter = 30000, n.burnin = 20000, n.thin = 10,
##   model.file = bymmodelfile,
##   bugs.directory = BugsDir,
##   WINEPATH = "/usr/bin/winepath")


###################################################
### code chunk number 53: dismap.Rnw:1609-1612
###################################################
nc$BYMmean <- MCMCres$mean$theta
nc$BYMumean <- MCMCres$mean$u
nc$BYMvmean <- MCMCres$mean$v


###################################################
### code chunk number 54: dismap.Rnw:1648-1660
###################################################
library(coda)
ncoutput <- read.coda("BYM/coda1.txt", "BYM/codaIndex.txt")

plot(ncoutput[,c("deviance", "alpha", "beta", "theta[94]")])


###################################################
### code chunk number 55: dismap.Rnw:1680-1681
###################################################
geweke.diag(ncoutput[,c("deviance", "alpha", "beta", "theta[94]")])


###################################################
### code chunk number 56: dismap.Rnw:1694-1705
###################################################
print(spplot(nc, c("SMR", "BYMmean"), at=brks, col.regions=cols,
  axes=TRUE))


###################################################
### code chunk number 57: dismap.Rnw:1716-1742
###################################################
ubrks <- quantile(nc$BYMumean, seq(0,1,1/5))
ubrks[6] <- ubrks[6]*1.01
colorkey$labels <- as.character(trunc(1000*ubrks)/1000)
colorkey$at <- ubrks[1]+ubrks[6]*0:5/5
ubrks <- seq(-0.35, 0.35, 0.05)
print(spplot(nc, c("BYMumean", "BYMvmean"), at= ubrks, axes=TRUE, 
col.regions=c(rev(brewer.pal(7, "Reds")), brewer.pal(7, "Blues"))))


###################################################
### code chunk number 58: dismap.Rnw:1787-1820
###################################################
plot(1,1, type="n", xlim=c(1,100), ylim=c(0,4.5),
  main= "Credible intervals of the relative risks",
  xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)

for(i in 1:100)
{
        if(MCMCres$summary[i,3]>1 )
        {
                col <- sig.col
		lty <- 2
                text(i, MCMCres$summary[i,7]+.31, nc$NAME[i],
                srt=90, col=sig.col, cex=.85)
        }
        else
        {
                col <- "black"
		lty <- 1
        }

        lines(c(i,i), c(MCMCres$summary[i,3], MCMCres$summary[i,7]), col=col, lty=lty)
}

points(1:100, MCMCres$median$theta, pch=18, col=col)



