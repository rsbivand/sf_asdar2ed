###################################################
# hello_mod.R
# packages: sp, maps, maptools, rgdal
# datasets: data1964al.xy


###################################################
### chunk number 1: 
###################################################
rm(list=ls())
if ((site <- Sys.getenv("ASDAR_DOWNLOAD")) != "")
  download.file(paste(site, "data1964al.xy", sep="/"), "data1964al.xy")


###################################################
### chunk number 11: 
###################################################
library(sp)
library(maps)
#library(maptools)
#library(rgdal)
library(sf)
sessionInfo()


###################################################
### chunk number 12: 
###################################################
volc.tab = read.table("data1964al.xy")
volc = SpatialPoints(volc.tab[c(2,1)])
llCRS <- CRS("+proj=longlat +ellps=WGS84")
slot(volc, "proj4string") <- llCRS
prj_new = CRS("+proj=moll +ellps=WGS84")
volc_proj = spTransform(volc, prj_new) # uses sf::st_transform internally
wrld <- map("world", interior=FALSE, xlim=c(-179,179), ylim=c(-89,89),
 plot=FALSE)
wrld_sf = st_as_sf(wrld, fill=FALSE, crs=llCRS)
wrld_sf = st_wrap_dateline(wrld_sf) # avoiding maptools::pruneMap
wrld_sp = as(wrld_sf[!st_is_empty(wrld_sf),], "Spatial")
wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grd <- gridlines(wrld_sp, easts=c(-179,seq(-150,150,50),179.5),
 norths=seq(-75,75,15), ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
at_sp <- gridat(wrld_sp, easts=0, norths=seq(-75,75,15), offset=0.3)
at_proj <- spTransform(at_sp, prj_new)
opar = par(no.readonly = TRUE)
par(mar=c(1,1,1,1)+0.1, xpd=NA)
plot(wrld_proj, col="grey50")
plot(wrld_grd_proj, add=TRUE, lty=3, col="grey50")
text(coordinates(at_proj), pos=at_proj$pos, offset=at_proj$offset,
 labels=parse(text=as.character(at_proj$labels)), cex=0.6)
par(opar)

###################################################
### chunk number 13: 
###################################################
data(volcano)
opar = par(no.readonly = TRUE)
par(mar = rep(1,4))
grys <- grey.colors(8, 0.55, 0.95)

layout(matrix(c(1,2,1,3,1,4),3,2,byrow=TRUE), c(3,1))

image(volcano, axes=FALSE, col=grys, asp=1, main="a")
contour(volcano, add=TRUE)
box()

image(volcano, axes=FALSE, col='white', asp=1, main="b")
#x2 = maptools::ContourLines2SLDF(contourLines(volcano))
x = contourLines(volcano)
xx = lapply(x, function(x) cbind(x$x, x$y)) # avoiding maptools::ContourLines2SLDF
xxx = split(xx, sapply(x, "[[", "level"))
x2a = st_sfc(lapply(xxx, st_multilinestring))
x2 = st_as_sf(x2a, level=names(xxx))
#plot(x2, add=TRUE)
plot(st_geometry(x2), add=TRUE)
box()

image(volcano, axes=FALSE, col='white', asp=1, main="c")
#pl <- list(Polygon(coordinates(x2[x2$level == 140,])))
#plot(SpatialPolygons(list(Polygons(pl, ID="x"))), add = TRUE)
plot(st_geometry(x2[x2$level == "140",]), add=TRUE)
box()

image(volcano, axes=FALSE, col=grys, asp=1, main="d")
x = st_polygonize(x2[x2$level == "160",])
xx = st_collection_extract(x)
plot(st_geometry(xx[1,]), col="orange", add=TRUE)
box()
layout(1)
par(opar)


#x3l1 = coordinates(x2[x2$level == 160,])[[1]][[1]]
#x3l2 = coordinates(x2[x2$level == 160,])[[1]][[2]]
#pl <- list(Polygon(x3l1,hole=F), Polygon(x3l2,hole=TRUE))
#x3 = SpatialPolygons(list(Polygons(pl, ID=c("x"))))

#SP2TRI = function(x, debug = TRUE){
#    p = x@polygons[[1]] # object of class Polygons
#    p1 = p@Polygons[[1]] # outer Polygon
#    p2 = p@Polygons[[2]] # inner Polygon
#    stopifnot(!p1@hole)
#    stopifnot(p2@hole)
#    # find nearest point
#    allcoords = rbind(p1@coords, p2@coords)
#    n1 = nrow(p1@coords)
#    n2 = nrow(p2@coords)
#    dists = as.matrix(dist(allcoords))[((n1+1):(n1+n2)),1:n1]
#    wm = which.min(dists)[1]
#    ind1 = (wm %/% n2) + 1
#    ind2 = wm %% n2
#    if (debug)
#        print(c(ind1,ind2))
#    #plot polygon points:
#    p1c = p1@coords
#    p2c = p2@coords
#    #plot shortest distance:
#    if (debug)
#        lines(rbind(p1c[ind1,], p2c[ind2,]))
#    if (debug)
#        points(rbind(p1c, p2c))
#    p = rbind(p1c[c(ind1:n1,1:ind1),], p2c[c(ind2:n2,1:ind2),], p1c[ind1,])
#    #polygon(p, col = 'red', border = NULL)
#    polygon(p, angle=45, border = NA, density = 12)
#}
#plot(x3, col = 'transparent', add = TRUE)
#SP2TRI(x3, FALSE)
#box()

#par(opar)


