##########################################################
## Analysis of Variance
##########################################################

# Manual ANOVA 

## Input data manually
yA = c(14.29,19.10,19.09,16.25,15.09,16.61,19.63)
yB = c(20.06,20.64,18.00,19.56,19.47,19.07,18.38)
yC = c(20.04,26.23,22.74,24.04,23.37,25.02,23.27)
y = c(yA,yB,yC)
C = matrix(c(0,1,-1,0,0,1,0,-1),nrow=2,byrow=T)
  
## set parameters
k = 3
n = length(yA)
df1 = k-1
df2 = k*(n-1)

## Generate the matrix X
x0 = rep(1,k*n)
xA = c(rep(1,n),rep(0,n),rep(0,n))
xB = c(rep(0,n),rep(1,n),rep(0,n))
xC = c(rep(0,n),rep(0,n),rep(1,n))
X = cbind(x0,xA,xB,xC)

## set up for relevant matrix
XtX = t(X)%*%X
Xty = t(X)%*%y

## create generalized inverse for X'X
XtXi = matrix(rep(0,(k+1)*(k+1)),nrow=k+1)
for(i in 1:k){
  XtXi[i+1,i+1] = 1/n
}

## estimation
beta = XtXi%*%Xty
SSE = t(y)%*%y - t(beta)%*%Xty
s2 = SSE/df2

## hypothesis testing for beta
Fnum = t(C%*%beta)%*%solve(C%*%XtXi%*%t(C))%*%(C%*%beta)
Fdenom = SSE
F.all = (Fnum/df1) / (Fdenom/df2)
pval = 1 - pf(F.all,df1, df2)

## hypothesis testing for beta1
c = t(c(0,1,-1,0))
k = 1
df1 = k
Fnum = t(c%*%beta)%*%solve(c%*%XtXi%*%t(c))%*%(c%*%beta)
Fdenom = SSE
F.1 = (Fnum/df1) / (Fdenom/df2)
pval = 1 - pf(F.1,df1, df2)

#############################

## loading the dataset
calcium = read.csv("calcium.csv")

## exploratory analysis
library(psych)
boxplot(value~method, data=calcium) #data grouped by 'method'
describeBy(calcium$value, calcium$method, mat=T) # summary statistics

## preprocessing
class(calcium$method)
calcium$method = factor(calcium$method) # convert 'method' to factor  

## one-way ANOVA
calcium.aov = aov(value~method, data=calcium)
summary(calcium.aov)

## testing contrasts (method 1 as base)
summary.lm(calcium.aov)

## testing contrasts (method 2 as base)
calcium$method = relevel(calcium$method, ref=2) #rearrange factors
calcium.aov2 = aov(value~method, data=calcium)
summary(calcium.aov2)
summary.lm(calcium.aov2) # testing contrasts

## Comparison Method: Bonferroni approach
pairwise.t.test(calcium$value, calcium$method, p.adj='b')
## Comparison Method: Tukey Method
TukeyHSD(calcium.aov)