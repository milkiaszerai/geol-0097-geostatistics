
library(lattice)
library(misc3d)
library(plot3D)
library(corrplot)
library(Rcpp)
library(RGeostats)
library(fields)


# args function
args(plot)

args (db.create)

#Working Directory
getwd()

# Import Data
data=read.csv("Iron Deposit Geochemical Data.csv")
str(data)

#Visualization of samples location
plot(data[,5],data[,6],pch=16,cex=0.5, xlab = "Easting(m)", ylab = "Northing(m)",main="Sample Locations",asp=1)


#Visualization of elements concentrations
for(j in 1:8)
{plot(data[,5],data[,6],pch=16,cex=1*(data[,j+8]-min(data[,j+8]))/(max(data[,j+8])-min(data[,j+8])),
xlab = "Easting(m)", ylab = "Northing(m)",
main=paste(colnames(data)[j+8]," concentrations",sep= ""),asp=1)}


#Other way to visualize elements concentrations 
library(plot3D,quietly=TRUE,verbose=FALSE)

for (j in 1:8)
{scatter2D(data[,5], data[,6],colvar=data[,j+8],pch = 16,cex=0.8,
xlab = "Easting(m)", ylab = "Northing(m)",clab=paste(colnames(data[j+8])),
main=paste(colnames(data)[j+8],"Concentrations",sep= " "),asp=1)}


#Compute the main descriptive statistics
summary(data[,7])
summary(data[,9:16])


#Display histograms and boxplots of all four heavy metals concentrations
for(j in 1:8)
{hist(data[,j+8],col="blue", xlab="concentrations",main=colnames(data)[j+8])}

for(j in 1:8)
{boxplot(data[,j+8],col="blue", xlab="concentrations",main=colnames(data)[j+8])}

# Need for log-transformation ????????



#Visualization of categorical Variables (geothite & hematite)
scatter2D(data[,5],data[,6],colvar=data[,8],col=c(1,2),pch = 16,cex=0.8,  
xlab = "Easting(m)", ylab = "Northing(m)",clab=colnames(data)[8],main="Iron Lithology Types", asp=1)
legend("bottomright",legend=c("1. Geothite","2. Hematite"),pch =c(16,16),col=c(1,2))


# Compute summary statistics (Mean) of all elements concentrations in relation to the lithotypes:
  
aggregate(data[,9:16], by=list(data$Lithotype), FUN=mean, na.rm=TRUE)

#Fe
library(lattice,quietly=TRUE,verbose=FALSE)
histogram(~ data$Fe|as.factor(data$Lithotype),col="blue",xlab=expression("Fe %"))
boxplot(data$Fe~data$Lithotype,col="blue",xlab="Lithotype",ylab="Fe %")
#SiO2
histogram(~ data$SiO2|as.factor(data$Lithotype),col="blue",xlab=expression("SiO2 %"))
boxplot(data$SiO2~data$Lithotype,col="blue",xlab="Lithotype",ylab="SiO2 %")
#Al2O3
histogram(~ data$Al2O3|as.factor(data$Lithotype),col="blue",xlab=expression("Al2O3%"))
boxplot(data$Al2O3~data$Lithotype,col="blue",xlab="Lithotype",ylab="Al2O3 %",sep="")
#P
histogram(~ data$P|as.factor(data$Lithotype),col="blue",xlab=expression("P%",sep=""))
boxplot(data$P~data$Lithotype,col="blue",xlab="Lithotype",ylab="P%",sep="")
#S
histogram(~ data$S|as.factor(data$Lithotype),col="blue",xlab=expression("S %"))
boxplot(data$S~data$Lithotype,col="blue",xlab="Lithotype",ylab="S %")
#Mn
histogram(~ data$Mn|as.factor(data$Lithotype),col="blue",xlab=expression("Mn %"))
boxplot(data$Mn~data$Lithotype,col="blue",xlab="Lithotype",ylab="Mn %")


# BIVARIATE STATISTICS

# Observe the relation between two variables by scatter plots
# We have 8 variables and thus 64 scatter plots can be plotted
plot(data$Fe ~ data$P, xlab=expression(paste("P%",sep="")), ylab=expression(paste("Fe%",sep="")))
plot(data$Fe ~ data$SiO2, xlab=expression(paste("SiO2%",sep="")), ylab=expression(paste("Fe%",sep="")))
plot(data$Fe ~ data$Al2O3, xlab=expression(paste("Al2O3%",sep="")), ylab=expression(paste("Fe%",sep="")))
plot(data$Fe ~ data$S, xlab=expression(paste("S%",sep="")), ylab=expression(paste("Fe%",sep="")))
plot(data$SiO2 ~ data$Al2O3, xlab=expression(paste("Al2O3%",sep="")), ylab=expression(paste("SiO2",sep="")))


# Compute the Pearson correlation matrix
CorMat<- cor(data[,9:16])
print(CorMat)

# Plot he Pearson correlation matrix
library(corrplot,quietly=TRUE,verbose=FALSE)
corrplot(CorMat)

# Display the scatter plot matrix for pairs of all variables
pairs(data[,9:16], pch = 19)




