
print("Hello")


# import the dataset
data=read.csv("hunter_data.csv")
data


# Load all necessary packages
library(lattice)
library(misc3d)
library(plot3D)
library(corrplot)
library(Rcpp)
library(RGeostats)
library(fields)


# Visualization of K concentrations 
library(plot3D,quietly=TRUE,verbose=FALSE)
scatter2D(data[,1], data[,2],colvar=data[,3],pch = 16,xlab = 
"Easting(m)", ylab = "Northing(m)",clab="K",main=" Potassium 
concentrations(ppm)", asp=1)


# creates a standard data base (DB). NOTHING NEW!!!
db.data=db.create(x1=data[,1],x2=data[,2],z1=data[,3],flag.grid=FALSE)
db.data


#Transform data values into Gaussian values using functions anam.fit
data.anam = anam.fit(db.data,name="z1")
print(data.anam)

db.data_g = anam.z2y(data.anam,db.data)
print(db.data_g)


# Plot the histogram of the raw data and the Gaussian transformed one

par(mfrow=c(1,2))

hist(db.data_g[,4],col="blue",main="Histogram Raw Data",
xlab="Raw Values")

hist(db.data_g[,5],col="blue",main="Histogram Transformed Data", 
xlab="Transformed Values")



# Compute and display the omni-directional experimental variogram of transformed data
D=sqrt((max(db.data_g[,2])-min(db.data_g[,2]))^2 + (max(db.data_g[,3])
-min(db.data_g[,3]))^2) 

nlag=40

Vario_E_G=vario.calc(db.data_g,nlag=nlag,lag=D/(2*nlag))
Vario_E_G

# Fit variogram model on the omni-directional experimental variogram of transformed data.

Vario_M_G=model.auto(Vario_E_G,struct = melem.name(c(1,2,3)),draw=FALSE)
Vario_M_G

vario.plot(Vario_E_G,npairdw=1,npairpt=0,varline =FALSE,
title="Isotropic Model- Nugget effect + Exponention (20 Lags)",reset=TRUE,col="blue",lty=2, 
xlab="Distance(m)",ylab="Variogram")

model.plot(Vario_M_G ,vario=Vario_E_G,add=T,col="red")
legend("bottomright",legend=c("Empirical Variogram","Fitted Variogram"),lty=c(2,1),cex=1, col=c("blue","red"))




## Conditional Simulations

#Creating the grid DB on which the kriging will be performed

#grid=read.csv("meuse_grid.csv")
#db.target=db.create(x1=grid$x,x2=grid$y,flag.grid=FALSE)

# However, We dont have grid in .CSV 
# Use function db.grid.init instead!

db.target=db.grid.init(db.data_g,nodes=c(300,250))
db.target


# Creating the Neighborhood (simple kriging)
# Note Geostatistical simulation = repeated simulation
## For personal project, I recommend you moving neighbourhood

neigh=neigh.create(ndim = 2, type = 0)
Nsimu=20 # Setting Number of Simulations


CondSimu_G=simtub(db.data_g,db.target,Vario_M_G,neigh, 
nbsimu=Nsimu,nbtuba=1000) # dont change nbtuba=1000

CondSimu=anam.y2z(data.anam,CondSimu_G,names=c("Simu.Gaussian.z1.S*"))

# Note anam.y2z versus anam.z2y



## Display Conditional Simulations ##
library(fields)

# change i to vary number of simulations to be displayed

for(i in 1:20)
{
quilt.plot(CondSimu[,2], CondSimu[,3],CondSimu[,i+Nsimu+3],
zlim=c(min(CondSimu[,(Nsimu+3):(2*Nsimu+3)]),
max(CondSimu[,(Nsimu+3):(2*Nsimu+3)])), nx =300, ny=250,
xlab = "Easting(m)", ylab = "Northing(m)",asp=1, 
main=i,legend.args=list(text="K ppm"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1)
}



# Perform statistics on the conditional simulations
CondSimu_Mean=db.compare(CondSimu,names="Raw.Simu.Gaussian.z1.S*",
fun="mean")

CondSimu_Max=db.compare(CondSimu,names="Raw.Simu.Gaussian.z1.S*",
fun="maxi")

CondSimu_Min=db.compare(CondSimu,names="Raw.Simu.Gaussian.z1.S*",
fun="mini")

CondSimu_Std=db.compare(CondSimu,names="Raw.Simu.Gaussian.z1.S*",
fun="stdv")

CondSimu_Prob=matrix(NA,ncol=Nsimu,nrow=nrow(CondSimu[]))

for (j in 1: Nsimu)
{
  CondSimu_Prob[,j]=as.numeric(CondSimu[,j+Nsimu+3]>40)
}



# Display Conditional Mean ##
quilt.plot(CondSimu_Mean[,2], CondSimu_Mean[,3], CondSimu_Mean[,ncol(CondSimu_Mean[])],
zlim=c(min(CondSimu[,(Nsimu+3):(2*Nsimu+3)]),max(CondSimu[,(Nsimu+3):(2*Nsimu+3)])), 
nx =300, ny=250,xlab = "Easting(m)", ylab = "Northing(m)",asp=1, 
main="Conditional Mean",legend.args=list(text="K"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1)


# Display Maximum simulated values at each grid node ##
quilt.plot(CondSimu_Max[,2], CondSimu_Max[,3], CondSimu_Max[,ncol(CondSimu_Max[])],
zlim=c(min(CondSimu[,(Nsimu+3):(2*Nsimu+3)]),max(CondSimu[,(Nsimu+3):(2*Nsimu+3)])), 
nx =300, ny=250,xlab = "Easting(m)", ylab = "Northing(m)",asp=1, 
main="Maximum simulated values at each grid node",legend.args=list(text="K"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1)


# Display Minimum simulated values at each grid node ##
quilt.plot(CondSimu_Min[,2], CondSimu_Min[,3], CondSimu_Min[,ncol(CondSimu_Min[])],
zlim=c(min(CondSimu[,(Nsimu+3):(2*Nsimu+3)]),max(CondSimu[,(Nsimu+3):(2*Nsimu+3)])), 
nx =300, ny=250,xlab = "Easting(m)", ylab = "Northing(m)",asp=1, 
main="Minimum simulated values at each grid node",legend.args=list(text="K"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1)



## Conditional Standard Deviation ##
quilt.plot(CondSimu_Std[,2], CondSimu_Std[,3], CondSimu_Std[,ncol(CondSimu_Std[])], 
nx =300, ny=250,xlab = "Easting(m)", ylab = "Northing(m)",asp=1, 
main="Conditional Stdv.",legend.args=list(text="K"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1)


## Conditional Probability of k>40 ppm ##
quilt.plot(CondSimu[,2], CondSimu[,3], apply(CondSimu_Prob, 1, mean), 
nx =300, ny=250,xlab = "Easting(m)", ylab = "Northing(m)",asp=1,
main="Conditional Probability [K>40ppm]",legend.args=list(text="K"))
points(db.data_g[,2],db.data_g[,3],col=1,pch=".",cex=1







