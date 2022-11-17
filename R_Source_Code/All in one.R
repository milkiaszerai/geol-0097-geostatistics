setwd("D:/Semester-1 Liege Materials/Geostatistics/R software/new practicals")
data=read.csv("meuse_data.csv")
for(j in 1:4)
{plot(data[,1],data[,2],pch=16,cex=2*(data[,j+2]-
min(data[,j+2]))/(max(data[,j+2])-min(data[,j+2])),xlab =
"Easting(m)", ylab ="Northing(m)",main=paste(colnames(data)[j+2],"concentrations
(ppm)",sep=" "),asp=1)}
scatter2D(data[,1],data[,2],colvar=data[,10],col=topo.colors(64),pch =
            
            16,xlab = "Easting(m)", ylab ="Northing(m)",clab=colnames(data)[10],main="Soil Type", asp=1)
par(mfrow=c(2,2)) #2x2 sub-screens
for(j in 1:4)
{hist(data[,j+2],col="blue", xlab="Values",main=colnames(data)[j+2])}
par(mfrow=c(2,2))
for(j in 1:4)
{boxplot(data[,j+2],col="blue", xlab="Values",main=colnames(data)[j+2])}
par(mfrow=c(2,2))
for(j in 1:4){hist(data[,j+13],col="blue", xlab="Values",main=colnames(data)[j+13])
boxplot(data[,j+13],col="blue",ylab="Values",main=colnames(data)[j+13])}
summary(data[,14:17])
for(j in 1:4)
{hist(data[,j+13],col="blue", xlab="Values",main=colnames(data)[j+13])
  boxplot(data[,j+13],col="blue",
          ylab="Values",main=colnames(data)[j+13])}
par(mfrow=c(1,1))
plot(data$logZn ~ data$logCu, xlab=expression(paste("log"[10],"(Cu)",sep="")), ylab=expression(paste("log"[10],"(Zn)",sep="")))

par(mfrow=c(1,1))
plot(data$logZn ~ data$logCu, xlab=expression(paste("log"[10],"(Cu)",sep="")), ylab=expression(paste("log"[10],"(Zn)",sep="")))
ix=which((data$logZn < 2.6) & (data$logCu > 1.6))
ix
data[ix, ]
CorMat<- cor(data[,14:17])
print(CorMat)
?ix
?plotViewport
library(corrplot,quietly=TRUE,verbose=FALSE)
corrplot(CorMat)
pairs(data[,14:17], pch = 15)




distance<-function(x,y)
{
  dist=sqrt(outer(x,x,"-")^2+outer(y,y,"-")^2)
  return(dist)
}
## h-scatterplot Function ##
hscatterplot=function(x,y,grade,hdist_min,hdist_max)
{
  Dist_Mat=distance(x,y)
  Val_Mat=expand.grid(grade,grade)
  scatter_points=cbind(Val_Mat,c(Dist_Mat))
  colnames(scatter_points)=c("Z1","Z2","Lag")
  ix=which(as.numeric((scatter_points$Lag>hdist_min)&(scatter_points$Lag<=hdist_max))==T)
  rval= round(cor(scatter_points[ix,1], scatter_points[ix,2]),3)
  plot(scatter_points[ix,1],scatter_points[ix,2],xlab="z(x)",ylab="z(x+h)",main=substitute(paste("h"%in%"[",hdist_min,",",hdist_max,"] "," (r=",rval,")",sep=" ")))
  abline(lm(scatter_points[ix,1]~scatter_points[ix,2]))
}
## Calculation of h-scatterplot for bins (0,80], (80,120], (120,250], 

par(mfrow=c(1,1))
hscatterplot(data$x,data$y,data$logZn,0,80)
hscatterplot(data$x,data$y,data$logZn,80,120)
hscatterplot(data$x,data$y,data$logZn,120,250)
hscatterplot(data$x,data$y,data$logZn,250,500)



# variograms 


library(RGeostats)

db.data = db.create(x1=data[,1],x2=data[,2],z1=data[,17],flag.grid=FALSE)
db.data

# Diagonal Geographic Domain
D=sqrt((max(data[,1])-min(data[,1]))^2 + (max(data[,2])-min(data[,2]))^2) 

nlag=10

Vario_E1 = vario.calc(db.data, nlag=10, lag=D/(2*nlag))

print(Vario_E1)

vario.plot(Vario_E1,npairdw = 1,npairpt=1,varline =TRUE,title="Omnidirectional Experimental
Variogram",col="blue",lty=2,xlab="Distance(m)",ylab="Variogram")

Vario_M1 = model.auto(Vario_E1, struct = melem.name(c(1,2,3,4)),draw=FALSE)

print(Vario_M1)

vario.plot(Vario_E1,npairdw=1,npairpt=0,varline =FALSE,title="Isotropic Model",
reset=TRUE,col="blue",lty=2,xlab="Distance(m)",ylab="Variogram")

vario.plot(Vario_E1,npairdw=1,npairpt=1,varline =FALSE,title="Isotropic Model",
reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")

model.plot(Vario_M1 ,vario=Vario_E1,add=T,col="red")


#change number of lags

nlag=20

Vario_E2=vario.calc(db.data,nlag=nlag,lag=D/(2*nlag))

Vario_E2

vario.plot(Vario_E2,npairdw = 2,npairpt=1,varline =TRUE,title="Omni-di
rectional Experimental Variogram",col="blue",lty=2,xlab="Distance(m)",
ylab="Variogram")

Vario_M2=model.auto(Vario_E2,struct = melem.name(c(1,3)),draw=FALSE)
Vario_M2
vario.plot(Vario_E2,npairdw=1,npairpt=1,varline =FALSE,title="Isotropic Model",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M2 ,vario=Vario_E2,add=T,col="red")


# Directional Variograms

ndir=4 # Number of Directions
ang0=0 # 1st Direction
dirvect=((seq(1,ndir)-1) * 180 / ndir+ ang0) # Directions
nlag=10 # Number of lags
Vario_E4=vario.calc(db.data,dirvect=dirvect,lag=D/(2*nlag),nlag=nlag)
Vario_E4


ndir=4 # Number of Directions
ang0=0 # 1st Direction
dirvect=((seq(1,ndir)-1) * 180 / ndir+ ang0) # Directions
nlag=10 # Number of lags

Vario_E4=vario.calc(db.data,dirvect=dirvect,lag=D/(2*nlag),nlag=nlag)

Vario_E4
vario.plot(Vario_E4,npairdw = 0,npairpt=0,varline =TRUE,title="Directi
onal Experimental Variograms",lty=2,xlab="Distance(m)",ylab="Variogram")

legend("bottomright",legend=c("Direction 0?","Direction 45?","Direction 90?","Direction 135?"),lty=c(2,2,2,2),col=c(1,2,3,4))

Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TRUE, 
auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)

print(Vario_M4)

vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometric Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0?","Direction 45?","Direction 90?","Direction 135?"),lty=c(1,1,1,1),col=c(1,2,3,4))



nlag=10 # Number of lags
Vario_E4=vario.calc(db.data,dirvect=dirvect,lag=D/(2*nlag),nlag=nlag)
Vario_E4
vario.plot(Vario_E4,npairdw = 0,npairpt=0,varline =TRUE,title="Directi
onal Experimental Variograms",lty=2,xlab="Distance(m)",ylab="Variogram
")
legend("bottomright",legend=c("Direction 0?","Direction 45?","Directio
n 90?","Direction 135?"),lty=c(2,2,2,2),col=c(1,2,3,4))
Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TRUE,
auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4
vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometri
c Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variog
ram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0?","Direction 45?","Directio
n 90?","Direction 135?"),lty=c(1,1,1,1),col=c(1,2,3,4))


Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TRUE,auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4
vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometri
c Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variog
ram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0?","Direction 45?","Directio
n 90?","Direction 135?"),lty=c(1,1,1,1),col=c(1,2,3,4))











## Kriging

grid=read.csv("meuse_grid.csv")
db.target=db.create(x1=grid$x,x2=grid$y,flag.grid=FALSE)

neigh1=neigh.create(ndim = 2, type = 0) # Unique Neighborhood kriging
Res1=kriging(db.data,db.target,Vario_M1,mean=NA,neigh1)#Kriging Result
s
Res1
mean(Res1[,4])
sd(Res1[,4])
max(Res1[,4])
min(Res1[,4])

Install.packages ("fields")

quilt.plot(Res1[,2], Res1[,3], Res1[,4],nx =110, ny=140,col=tim.colors(256),xlab = "Easting(m)", ylab = "Northing(m)",main="Ordinary Kriging
Estimates",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)

quilt.plot(Res1[,2], Res1[,3], Res1[,5],nx =110, ny=140,col=tim.colors(256),xlab = "Easting(m)", ylab = "Northing(m)",main="Ordinary Kriging
Standard Deviations",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)


# Cross Validation ##
CV1=xvalid(db.data, model = Vario_M1, neigh = neigh1)
CV_Stats_1=matrix(NA,2,2)
rownames(CV_Stats_1)=c("Errors","Standardized Errors")
colnames(CV_Stats_1)=c("Mean","Variance")
CV_Stats_1[1,1]=mean(CV1[,5])
CV_Stats_1[1,2]=var(CV1[,5])
CV_Stats_1[2,1]=mean(CV1[,6])
CV_Stats_1[2,2]=var(CV1[,6])
CV_Stats_1
plot(CV1[,4]-CV1[,5],CV1[,4],xlab="Estimated Values",ylab="True Values
",pch="+")
hist(CV1[,6],col="blue",xlab="Standardized Estimation Errors",main="")


setwd("D:\Semester-1 Liege Materials\Geostatistics\R software\new practicals\RGeostats_Simulations")
