


Eddy_core_analysis <- function(nclist,i){

#mount all libraries   
library(ncdf4) # For reading the netCDF file
library(RColorBrewer) # For color palttes
library(fields) # For plotting
library(maps) # For basemaps
library(animation) # For making gif
library(dplyr) # For using storms dataset
library(ggmap) # For maps like library maps
library(raster)


#read nc files with bands 
fileobj<-nc_open(nclist) # August 2005, Era Interim Daily Data Set
time<-ncvar_get(fileobj,"time")
Latitude<-ncvar_get(fileobj,"latitude")
Longitude<-ncvar_get(fileobj,"longitude")
u<-ncvar_get(fileobj,"u")
v<-ncvar_get(fileobj,"v")
time_units <- ncatt_get(fileobj,"time","units")


# Flipping Latitudes
u_wind<-array(NA,dim(u))
u_wind[,,]<-u[,ncol(u):1,]
u_wind[,,]<-u[,ncol(u):1,]
v_wind<-array(NA,dim(v))
v_wind[,,]<-v[,ncol(v):1,]
v_wind[,,]<-v[,ncol(v):1,]
uvel<- u_wind[,,1]
vvel<- v_wind[,,1]

#getting the differentiation
dx=dy= 0.33
# resolution of data is about 0.33m
#forward difference for computing dudy and dvdx
dudy = (uvel[,2:fileobj$var$v$varsize[2]] - uvel[,1:fileobj$var$v$varsize[2]-1])/dy
dvdx = (vvel[2:fileobj$var$v$varsize[1],] - vvel[1:fileobj$var$v$varsize[1]-1,])/dx
# The dimensions of dudy and dvdx are off
# So I add the last row/column to the end+1 so that dimensions will match
# Boundaries are ignored
y = (array(1, dim = c(fileobj$var$v$varsize[1],1)))
yy = y*dudy[,fileobj$var$v$varsize[2]-1]
x = (array(1, dim = c(1,fileobj$var$v$varsize[2])))
xx = x*(t(dvdx[fileobj$var$v$varsize[1]-1,]))
dudy = cbind(dudy,yy)
dvdx = rbind(dvdx,xx)
# Vorticity is equal to dvdx-dudy
vortex <- dvdx-dudy
#### Okubo Weiss Parameter (1/s^2)
w<- dvdx-dudy
w<- vort
#### Ss = dvdx + dudy, Okubo = Sn^2 + Ss^2 - w^2
Sn<- dudx - dvdy
#forward difference for computing dvdy and dudx
dvdy = (uvel[,2:fileobj$var$v$varsize[2]] - uvel[,1:fileobj$var$v$varsize[2]-1])/dy
dudx = (vvel[2:fileobj$var$v$varsize[1],] - vvel[1:fileobj$var$v$varsize[1]-1,])/dx
y2 = (array(1, dim = c(fileobj$var$v$varsize[1],1)))
yy2 = y2*dvdy[,fileobj$var$v$varsize[2]-1]
x2 = (array(1, dim = c(1,fileobj$var$v$varsize[2])))
xx2 = x2*(t(dudx[fileobj$var$v$varsize[1]-1,]))
dvdy = cbind(dvdy,yy2)
dudx = rbind(dudx,xx2)
Sn = dudx - dvdy
Ss = dvdx + dudy
Okubo <- (Sn^2) + (Ss^2) - (vort^2)

eddy<- list(Okubo,vortex)
names(eddy)<- c("okubo","vortex")
return(eddy)
}














