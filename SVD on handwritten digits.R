# Question 1
#Download zip.test and zip.train data from R package
library(ElemStatLearn)

#Obtain the rows of zip.test and zip.train which has first column=3
datatest <- zip.test[which(zip.test[,1]==3),]
datatrain <- zip.train[which(zip.train[,1]==3),]
combineddata <- rbind(datatest,datatrain)


#Dimensions of combined data
dim(combineddata)

#Obtain the whole combined data with first column removed
wholedata <- combineddata[, 2:257]

#x is number of images of '3'
x <- dim(wholedata)[1]
x


#Compute the column means
xmean <- colMeans(wholedata)

#center the matrix
X_c = scale(wholedata, center= TRUE, scale = FALSE)


#Perform SVD on matrix A
A=1/sqrt(x) *X_c

#Compute SVD
svdx = svd(A)
lambda1 = svdx$d[1]
lambda2 = svdx$d[2]
vector1 = svdx$v[,1]
vector2 = svdx$v[,2]

#
plot_image <- function(x){
  creatematrix = matrix(c(3,x),1,257)
  image(zip2image(creatematrix,1), col=gray(256:0/256), xlab="", ylab="" )
}

par(mfrow = c(1,3))
plot_image(xmean)
title("Mean")
plot_image(vector1)
title("1st Eigenvector")
plot_image(vector2)
title("2nd Eigenvector")


# 2 component model
f_lambda <- xmean + lambda1*vector1 + lambda2*vector2
par(mfrow = c(1,2))
plot_image(xmean)
title("Mean")
plot_image(f_lambda)
title("2 Component Model")

# Question 2
#load and read data
setwd("~/Desktop/Assignment")
data<-read.table("principalcurve-data.txt",header=TRUE)
attach(data)
x<-scale(cbind(x1,x2))


#scatterplot
alim<-extendrange(x,f=0.1)
alim_<-range(x)
plot(x[,1],x[,2],bty="n",xlab=expression(x[1]),ylab=expression(x[2]),
     xlim=alim,ylim=alim)
legend("topleft",legend=c("Initialize"), bty="n")


##plot first principal component line
plot(x[,1],x[,2],bty="n",xlab=expression(x[1]),ylab=expression(x[2]),
     xlim=alim,ylim=alim)
legend("topleft",legend=c("Principal Component Curve"), bty="n")
svdx<-svd(x)
clip(alim_[1],alim_[2],alim_[1],alim_[2])
with(svdx,abline(a=0,b=v[2,1]/v[1,1]))

##plot projections of each point onto line
z1<-with(svdx,x%*%v[,1]%*%t(v[,1]))
segments(x0=x[,1],y0=x[,2],x1=z1[,1],y1=z1[,2])


#compute initial lambda (arc-lengths associated with
#orthogonal projections of data onto curve)
lam<- with(svdx,as.numeric(u[,1]*d[1]))

##compute scatterplot smoother in either dimension
##increase 'df' to make the curve more flexible 
par(mfrow=c(2,2))
for (iterations in 1:10) {
  #Step (a) of algorithm
  fit1<-smooth.spline(x=lam,y=x[,1], df=4)
  fit2<-smooth.spline(x=lam,y=x[,2], df=4)
  
##plot data and the principal curve for a sequence of lambdas	
plot(x[,1],x[,2],bty="n",xlab=expression(x[1]),ylab=expression(x[2]),
     xlim=alim,ylim=alim)
legend("topleft", legend = c("step (a)"), bty = "n")
seq_lam<-seq(min(lam),max(lam,length.out=100))
lines(predict(fit1,seq_lam)$y,predict(fit2,seq_lam)$y)
  
##show points along curve corresponding to orginal lambdas
z1<-cbind(predict(fit1,lam)$y,predict(fit2,lam)$y)
segments(x0=x[,1],y0=x[,2],x1=z1[,1],y1=z1[,2])

##step (b) of iterative algorithm	
##recompute lambdas		
euc_dist <- function(l,x,f1,f2)
  sum((c(predict(f1,l)$y,predict(f2,l)$y)-x)^2)
lam <- apply(x,1,function(x0) optimize(euc_dist,interval=extendrange(lam,f=0.50),
                                       x=x0,f1=fit1,f2=fit2)$minimum)

##show projections associated with recomputed lambdas
plot(x[,1],x[,2],bty="n",xlab=expression(x[1]),ylab=expression(x[2]),
     xlim=alim,ylim=alim)
legend("topleft",legend=c("Step (b)"),bty="n")
seq_lam<-seq(min(lam),max(lam),length.out=100)
lines(predict(fit1,seq_lam)$y,predict(fit2,seq_lam)$y)
z1<-cbind(predict(fit1,lam)$y,predict(fit2,lam)$y)
segments(x0=x[,1],y0=x[,2],x1=z1[,1],y1=z1[,2])

}


##final curve
plot(x[,1],x[,2],bty="n",xlab=expression(x[1]),ylab=expression(x[2]),
     xlim=alim, ylim=alim)
legend("topleft",legend=c("Step (b)"),bty="n")
seq_lam<-seq(min(lam),max(lam),length.out=100)
lines(predict(fit1,seq_lam)$y,predict(fit2,seq_lam)$y)
z1<-cbind(predict(fit1,lam)$y,predict(fit2,lam)$y)
segments(x0=x[,1],y0=x[,2],x1=z1[,1],y1=z1[,2])

