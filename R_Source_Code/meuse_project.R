# Import the important libraries/packages

library(lattice)
library(misc3d)
library(plot3D)      #scatter plots
library(corrplot)    #correlation matrix
library(Rcpp)
library(RGeostats)
library(fields)

search()
#Working Directory
getwd()

# Import Data
data_1=read.csv("meuse_data.csv")
class(data_1)

head(data)
tail(data)

str (data)


# Stat

summary(data[,3:6])

##Visualization of samples location
plot()

plot(data[,1],data[,2],pch=15,cex=0.5, xlab = "Easting(m)", ylab = "Northing(m)",main="Locations",asp=1)




##Visualization of heavy metal concentrations##

par(mfrow=c(1,1))
for(j in 1:4)
{plot(data[,1],data[,2],pch=16,cex=2*(data[,j+2]-min(data[,j+2]))/(max
(data[,j+2])-min(data[,j+2])),xlab = "Easting(m)", ylab = "Northing(m)",
main=paste(colnames(data)[j+2],"concentrations (ppm)",sep= "-"),asp=1)}


## Other way to visualize the Data ##
library(plot3D,quietly=TRUE,verbose=FALSE)
scatter2D(data[,1], data[,2],colvar=data[,6],pch = 16,xlab = "Easting(
m)", ylab = "Northing(m)",clab="zinc",main="zinc concentrations(ppm)",asp=1)


## Visualization of categorical Variables (e.g., Soil Type) ##
scatter2D(data[,1],data[,2],colvar=data[,10],col=c(11,12,13),pch = 16,cex= 0.8, xlab = "Easting(m)", 
ylab = "Northing(m)",clab=colnames(data)[10],main="Soil Type", asp=1)
legend("bottomright",legend=c("Soil 1","Soil 2", "Soil 3"),pch =c(16,16,16),col=c(11,12,13))


##Compute the main descriptive univariate statistics#
summary(data[,3:6])


##Display histograms and boxplots of all four heavy metals concentrations
hist(data$cadmium,col="blue", xlab="Values",main="cadmium concentrations")
hist(data$copper,col="blue", xlab="Values",main="copper concentrations")
hist(data$lead,col="blue", xlab="Values",main="lead concentrations")
hist(data$zinc,col="blue", xlab="Values",main="zinc concentrations")

#Using loop (2x2 sub-screens)
par(mfrow=c(1,1)) 
for(j in 1:4)
  {hist(data[,j+2],col="blue", xlab="Values",main=colnames(data)[j+2])}




#transform the screen back to normal
par(mfrow=c(1,1))
boxplot(data$cadmium,col="blue", xlab="Values",main="cadmium concentrations")
boxplot(data$copper,col="blue", xlab="Values",main="copper concentrations")
boxplot(data$lead,col="blue", xlab="Values",main="lead concentrations")
boxplot(data$zinc,col="blue", xlab="Values",main="zinc concentrations")



#using loop:
par(mfrow=c(2,2))



for(j in 1:4){
boxplot(data[,j+2],col="blue", xlab="Values",main=colnames(data)[j+2])}


v = c(1,2,3,4,8)

for (j in 1:5) {
  print(v[j])
}


# Log-transform the highly-skewed distribution of heavy metal concentration.
data$logCd=log10(data$cadmium)
data$logCu=log10(data$copper)
data$logPb=log10(data$lead)
data$logZn=log10(data$zinc)


str(data)

#Compute the main descriptive univariate statistics of log transformed data
summary(data[,14:17])

#Display histograms and boxplots of log transformed data
par(mfrow=c(2,2))
for(j in 1:4){
  hist(data[,j+13],col="blue", xlab="Values",main=colnames(data)[j+13])}

for(j in 1:4){
  hist(data[,j+13],col="blue", xlab="Values",main=colnames(data)[j+13])
  
  boxplot(data[,j+13],col="blue", ylab="Values",main=colnames(data)[j+13])}



par(mfrow=c(1,2))
for(j in 1:4){
  hist(data[,j+2],col="blue", xlab="Values",main=colnames(data)[j+2])
  
  hist(data[,j+13],col="red", xlab=" Log Values",main=colnames(data)[j+13])}



## BIVARIATE STATISTICS

#Plot scatter plots
plot(data$cadmium ~ data$zinc, xlab= "cd", ylab="zinc")

# Different one (conventional method)
plot(data[,15],data[,17],pch=10,cex=0.5, xlab = "log Cu", ylab = "Log Zn",main="Locations",asp=1)

# Observe the samples that do not fit the general pattern 
ix=which((data$logZn < 2.6) || (data$logCu > 1.6))
print (ix)
ix
data[ix,]


#Compute the Pearson correlation matrix
CorMatrix = cor(data[,3:6])
print(CorMatrix) 

CorMatlog<- cor(data[,14:17])
print(CorMat) 

#Compute for log-transformed variables


corrplot(CorMatrix)

# Compute the scatter plot matrix
pairs(data[,3:6], pch = 19)



#Statistics vs category

#Compute summary statistics categorical variables
aggregate(data[,3:6], by=list(data$soil), FUN=median, na.rm=FALSE)

#Display the histogram and boxplot by categorical variables
library(lattice,quietly=TRUE,verbose=FALSE)

histogram(~ data$logZn|as.factor(data$soil),col="blue",
          xlab=expression(paste("log"[10],"(Zn)",sep="")))
boxplot(data$zinc~data$ffreq,col="blue",xlab="Flood Frequency Class", 
        ylab=expression(paste("(Zn)")))


# Structural Analysis

# abline () = add Straight line to a an existing Plot
# lm() = fits Linear Models (e.g. linear  regression)

# #What is the distribution/spatial continuity of Zn values through x-directions?
plot(data$zinc ~ data$x, xlab="Easting", ylab="Zn concentration")
abline(lm(data$zinc ~ data$x), col="red")

# #What is the distribution/spatial continuity of Zn values through y-directions?
plot(data$zinc ~ data$y, xlab="Northing", ylab="Zn concentration")
abline(lm(data$zinc ~ data$y), col="red")

# #What is the distribution/spatial continuity of Zn values through z-directions?
plot(data$zinc ~ data$elev, xlab="Elevation", ylab="Zn concentration")
abline(lm(data$zinc ~ data$elev), col="red")



## Distance Function ##
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
  plot(scatter_points[ix,1],scatter_points[ix,2],xlab="z(x)",ylab="z(x+h)",
  main=substitute(paste("h"%in%"[",hdist_min,",",hdist_max,"] "," (r=",rval,")",sep=" ")))
  abline(lm(scatter_points[ix,1]~scatter_points[ix,2]))
}


## Calculation of h-scatterplot for bins (0,80], (80,120], (120,250], (250,500], and (500,1000] ##
par(mfrow=c(2,3))
hscatterplot(data$x,data$y,data$zinc,0,80)
hscatterplot(data$x,data$y,data$zinc,80,120)
hscatterplot(data$x,data$y,data$zinc,120,250)
hscatterplot(data$x,data$y,data$zinc,250,500)
hscatterplot(data$x,data$y,data$zinc,500,1000)









# variograms
db.data=db.create(x1=data[,1],x2=data[,2],z1=data[,17],flag.grid=FALSE)
db.data

#Graphic representation
db.plot(db.data,xlab = "Easting(m)", ylab = "Northing(m)",title="LogZn concentration")
plot(db.data,xlab = "Easting(m)", ylab = "Northing(m)",title="Log   Zn concentration")

## Compute and display the omni-directional EXPERIMENTAL variogram

# Diagonal Geographic Domain
D=sqrt((max(data[,1])-min(data[,1]))^2 + (max(data[,2])-min(data[,2]))^2)

# number of lags
nlag=10

# calculate variogram
Vario_E1=vario.calc(db.data,nlag=nlag,lag=D/(2*nlag))

Vario_E1

# PLot variogram
vario.plot(Vario_E1,npairdw = 2,npairpt=1,varline =F,
title="Omni-directional Experimental Variogram",col="blue",lty=2,xlab="Distance(m)",ylab="Variogram")

#Fit automatically a variogram model on the omni-directional experimental variogram.

Vario_M1=model.auto(Vario_E1,struct = melem.name(c(1,3)),draw=FALSE)
Vario_M1
vario.plot(Vario_E1,npairdw=1,npairpt=1,varline =1,
title="10 lags (nugget effect + sph)",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M1 ,vario=Vario_E1,add=T,col="red")


#Change the number of lags and observe what happens to the variogram
nlag=20 # number of lags

# calculate variogram
Vario_E2=vario.calc(db.data,nlag=nlag,lag=D/(2*nlag))

Vario_E2

# PLot variogram
vario.plot(Vario_E2,npairdw = 2,npairpt=1,varline =T,
title="Omni-directional Experimental Variogram",col="blue",lty=2,xlab="Distance(m)",ylab="Variogram")

#Fit automatically a variogram model on the omni-directional experimental variogram.

Vario_M2=model.auto(Vario_E2,struct = melem.name(c(1,2)),draw=FALSE)
Vario_M2
vario.plot(Vario_E2,npairdw=1,npairpt=1,varline =1,
title="20 lags (nugget effect + exponential)",reset=TRUE,col="blue",lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_M2 ,vario=Vario_E2,add=T,col="green")



# Compute the experimental directional variogram in four directions: North-South Direction (90°), East-West Direction (0°), North-West South-East Direction (45°), and North-East SouthWest Direction (135°).
ndir=4 # Number of Directions
ang0=0 # 1st Direction
dirvect=((seq(1,ndir)-1) * 180 / ndir+ ang0) # Directions
nlag=10 # Number of lags
Vario_D1=vario.calc(db.data,dirvect=dirvect,lag=D/(2*nlag),nlag=nlag)
Vario_D1

vario.plot(Vario_D1,npairdw = 2,npairpt=0,varline=TRUE,
title="Directional Experimental Variograms",lty=2,
xlab="Distance(m)",ylab="Variogram")

legend("bottomright",legend=c("Direction 0°","Direction 45°","Direction 90°","Direction 135°"),lty=c(2,2,2,2),col=c(1,2,3,4))


# Fit automatically a geometric anisotropic variogram model on directional experimental variograms

Vario_M4=model.auto(Vario_D1,struct = melem.name(c(1,2)),auth.aniso=TRUE,auth.rotation=TRUE,auth.locksame = TRUE,draw=FALSE)
Vario_M4

vario.plot(Vario_D1,npairdw=1,npairpt=0,varline =T,title="Geometric Anisotropic Model",reset=TRUE,lty=2, xlab="Distance(m)",ylab="Variogram")
model.plot(Vario_D1 ,vario=Vario_,add=T)
legend("bottomright",legend=c("Direction 0°","Direction 45°","Direction 90°","Direction 135°"),lty=c(1,1,1,1),col=c(1,2,3,4))


#Compute and display the variographic map

Vario_Map=vmap.calc(db.data,nx=20,ny=20)
Vario_Map

plot(Vario_Map,title="Variographic Map")


# Kriging

#Create a grid on which kriging will be performed

grid=read.csv("meuse_grid.csv")

plot(grid[,1],grid[,2],pch=10,cex=0.00001, xlab = "Easting(m)", ylab = "Northing(m)",main="Grid",asp=1)

db.target=db.create(x1=grid$x,x2=grid$y,flag.grid=FALSE)

# Perform the unique neighbourhood kriging (i.e. simple kriging)
# based on 20 lags experimental variogram fitted to isotropic model 
# nugget effect + exponential model

#create neighbourhood
neigh1=neigh.create(ndim = 2, type = 0) # 0 = Unique Neighborhood kriging

##Kriging Results
Res1=kriging(db.data,db.target,Vario_M4,mean=NA,neigh1)
Res1

#statistics on the estimated values
mean(Res1[,4])
sd(Res1[,4])
max(Res1[,4])
min(Res1[,4])

## Display Kriging Estimates #
quilt.plot(Res1[,2], Res1[,3], Res1[,4],nx =10, ny=10,col=tim.colors(256),
xlab = "Easting(m)", ylab = "Northing(m)",main="simple Kriging Estimates",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)


# Display Kriging Standard Deviations
quilt.plot(Res1[,2], Res1[,3], Res1[,5],nx =100, ny=120,col=tim.colors(256),
xlab = "Easting(m)", ylab = "Northing(m)",main="simple Kriging Standard Deviations",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)

# Perform cross validation

#create X-validation database
CV1=xvalid(db.data, model = Vario_M4, neigh = neigh1)
CV1

# X-validation matrix
CV_Stats_1=matrix(NA,2,2)
rownames(CV_Stats_1)=c("Errors","Standardized Errors")
colnames(CV_Stats_1)=c("Mean","Variance")
CV_Stats_1[1,1]=mean(CV1[,5])
CV_Stats_1[1,2]=var(CV1[,5])
CV_Stats_1[2,1]=mean(CV1[,6])
CV_Stats_1[2,2]=var(CV1[,6])
CV_Stats_1

# Plot of estmimated erros versus true values
plot(CV1[,4]-CV1[,5],CV1[,4],xlab="Estimated Values",
ylab="True Values",pch="+")

# Add a bisector line (a=slope, b=intercept)
abline(a = 1, b = 0.7, col="red")        

#X-validation histogram
hist(CV1[,6],col="blue",xlab="Standardized Estimation Errors",main="")
hist(CV1[,5],col="blue",xlab="Estimation Errors",main="")



# Kriging with Moving Neighborhood (Ordinary Kriging)
neigh2=neigh.create(ndim = 2, type = 2, nsect = 4, nsmax = 6, nmini = 10, nmaxi =25, radius = D/2)
# Kriging Results
Res2=kriging(db.data,db.target,Vario_M4,mean=NA,neigh2) 
Res2


# Display Kriging Estimates #
quilt.plot(Res2[,2], Res2[,3], Res2[,4],nx =110, ny=140,col=tim.colors
(256),xlab = "Easting(m)", ylab = "Northing(m)",main="Ordinary Kriging 
Estimates",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)

# Display Kriging Standard Deviations#
quilt.plot(Res2[,2], Res2[,3], Res2[,5],nx =110, ny=140,col=tim.colors
(256),xlab = "Easting(m)", ylab = "Northing(m)",main="Ordinary Kriging 
Standard Deviations",asp=1)
points(data[,1],data[,2],col=1,pch=".",cex=1)


# Cross-Validation ##
CV2=xvalid(db.data, model = Vario_M1, neigh = neigh2)
CV_Stats_2=matrix(NA,2,2)
rownames(CV_Stats_2)=c("Errors","Standardized Errors")
colnames(CV_Stats_2)=c("Mean","Variance")
CV_Stats_2[1,1]=mean(CV2[,5])
CV_Stats_2[1,2]=var(CV2[,5])
CV_Stats_2[2,1]=mean(CV2[,6])
CV_Stats_2[2,2]=var(CV2[,6])
CV_Stats_2
hist(CV2[,6],col="blue",xlab="Standardized Estimation Errors",main="")
plot(CV2[,4]-CV2[,5],CV2[,4],xlab="Estimated Values",ylab="True Values
",pch="+")


*** THE END ***
