setwd("D:/Semester-1 Liege Materials/Geostatistics/R software/new practicals")
data=read.csv("meuse_data.csv")
for(j in 1:4)
{plot(data[,1],data[,2],pch=16,cex=2*(data[,j+2]-
min(data[,j+2]))/(max(data[,j+2])-min(data[,j+2])),xlab =
"Easting(m)", ylab ="Northing(m)",main=paste(colnames(data)[j+2],"concentrations
(ppm)",sep=" "),asp=1)}
scatter2D(data[,1],data[,2],colvar=data[,10],col=topo.colors(64),pch =
            16,xlab = "Easting(m)", ylab =
            

                        
            "Northing(m)",clab=colnames(data)[10],main="Soil Type", asp=1)
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

library(RGeostats)
db.data=db.create(x1=data[,1],x2=data[,2],z1=data[,17],flag.grid=FALSE)
db.data

D=sqrt((max(data[,1])-min(data[,1]))^2 + (max(data[,2])-min(data[,2]))^2) # Diagonal Geographic Domain
nlag=10 # number of lags
Vario_E1=vario.calc(db.data,nlag=nlag,lag=D/(2*nlag))
Vario_E1
vario.plot(Vario_E1,npairdw = 2,npairpt=0,varline =TRUE,title="Omnidirectional Experimental
Variogram",col="blue",lty=2,xlab="Distance(m)",ylab="Variogram")
Vario_M1=model.auto(Vario_E1,struct = melem.name(c(1,2,3)),draw=FALSE)
Vario_M1
vario.plot(Vario_E1,npairdw=1,npairpt=0,varline =FALSE,title="Isotropi
c Model",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogr
am")

vario.plot(Vario_E1,npairdw=1,npairpt=0,varline =FALSE,title="Isotropic Model",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M1 ,vario=Vario_E1,add=T,col="red")


nlag=20
Vario_E2=vario.calc(db.data,nlag=nlag,lag=D/(2*nlag))
Vario_E2
vario.plot(Vario_E2,npairdw = 2,npairpt=0,varline =TRUE,title="Omni-di
rectional Experimental Variogram",col="blue",lty=2,xlab="Distance(m)",
           ylab="Variogram")
Vario_M2=model.auto(Vario_E2,struct = melem.name(c(1,2,3)),draw=FALSE)
Vario_M2
vario.plot(Vario_E2,npairdw=1,npairpt=0,varline =FALSE,title="Isotropic Model",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M2 ,vario=Vario_E2,add=T,col="red")


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
onal Experimental Variograms",lty=2,xlab="Distance(m)",ylab="Variogram
")

legend("bottomright",legend=c("Direction 0°","Direction 45°","Direction 90°","Direction 135°"),lty=c(2,2,2,2),col=c(1,2,3,4))

Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TR
                    UE,auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4
vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometric Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0°","Direction 45°","Directio
n 90°","Direction 135°"),lty=c(1,1,1,1),col=c(1,2,3,4))




nlag=10 # Number of lags
Vario_E4=vario.calc(db.data,dirvect=dirvect,lag=D/(2*nlag),nlag=nlag)
Vario_E4
vario.plot(Vario_E4,npairdw = 0,npairpt=0,varline =TRUE,title="Directi
onal Experimental Variograms",lty=2,xlab="Distance(m)",ylab="Variogram
")
legend("bottomright",legend=c("Direction 0°","Direction 45°","Directio
n 90°","Direction 135°"),lty=c(2,2,2,2),col=c(1,2,3,4))
Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TR
                    UE,auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4
vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometri
c Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variog
ram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0°","Direction 45°","Directio
n 90°","Direction 135°"),lty=c(1,1,1,1),col=c(1,2,3,4))


Vario_M4=model.auto(Vario_E4,struct = melem.name(c(1,2)),auth.aniso=TRUE,auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4
vario.plot(Vario_E4,npairdw=1,npairpt=0,varline =FALSE,title="Geometri
c Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variog
ram")
model.plot(Vario_M4 ,vario=Vario_E4,add=T)
legend("bottomright",legend=c("Direction 0°","Direction 45°","Directio
n 90°","Direction 135°"),lty=c(1,1,1,1),col=c(1,2,3,4))




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
