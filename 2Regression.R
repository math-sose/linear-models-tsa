##########################################################
## Multiple Linear Regression
##########################################################

# Manual Regression 

## load necessary libraries
library(Matrix)
## Input data manually
y = c(2,3,2,7,6,8,10,7,8,12,11,14)
x0 = rep(1,12)
x1 = c(0,2,2,2,4,4,4,6,6,6,8,8)
x2 = c(2,6,7,5,9,8,7,10,11,9,15,13)
X = cbind(x0,x1, x2)
c = matrix(c(0,1,0,0,0,1),nrow=2,byrow=T)

## parameters
n = nrow(X)
p = ncol(X) - 1
k = rankMatrix(c)[1]
df1 = k
df2 = n - p - 1

## set up for relevant matrix
XtX = t(X)%*%X
Xty = t(X)%*%y
XtXi = solve(XtX)
H = diag(x0) - X%*%XtXi%*%t(X)

## estimation
beta = XtXi%*%Xty
e = H%*%y

## hypothesis testing for beta
Fnum = t(c%*%beta)%*%solve(c%*%XtXi%*%t(c))%*%(c%*%beta)
Fdenom = t(y)%*%H%*%y
F.all = (Fnum/df1) / (Fdenom/df2)
pval = 1 - pf(F.all,df1, df2)

## R2
R2 = Fnum/(Fnum+Fdenom)

## hypothesis testing for beta1
c = t(c(0,1,0))
k = 1
df1 = k
Fnum = t(c%*%beta)%*%solve(c%*%XtXi%*%t(c))%*%(c%*%beta)
Fdenom = t(y)%*%H%*%y
F.1 = (Fnum/df1) / (Fdenom/df2)
t.1 = sqrt(F.1)
pval = 2*(1 - pt(t.1,df2))

## R2
R2 = Fnum/(Fnum+Fdenom)

#############################

## loading the dataset
library(mlbench)
data(BostonHousing)
?BostonHousing #opens the R Documentation page of the dataset
names(BostonHousing) #column names of the dataset
head(BostonHousing) #prints first few rows the dataset

## exploratory analysis
summary(BostonHousing) #five-number summary for each column
plot(BostonHousing) #scatterplot for all possible variable pairs

housing = BostonHousing[-4] #remove the binary variable 'chas'

library(corrplot)
housing.corr = cor(housing) #correlation matrix
corrplot(housing.corr, method="color", type="upper") #plot correlation

## initial model
housing.full = lm(medv~., data=housing)
housing.full.res = summary(housing.full)
names(housing.full)  #available results obtained from regression
names(housing.full.res)  #available results obtained from regression
housing.full.res

## regression results
housing.full$coefficients #regression coefficients
housing.full.res$fstatistic #F-test for linear hypothesis
housing.full.res$coefficients #generalized linear hypotheses
housing.full.res$adj.r.squared # adjR2

## reduced models
housing.m11 = lm(medv~.-age, data=housing) #model w/o age
housing.m10 = lm(medv~.-age-indus, data=housing) #model w/o age, indus
housing.m11.res = summary(housing.m11)
housing.m10.res = summary(housing.m10)
housing.m11.res
housing.m10.res

## comparing the models
housing.full.res$adj.r.squared
housing.m11.res$adj.r.squared
housing.m10.res$adj.r.squared
anova(housing.full, housing.m11) #full vs reduced model (no age)
anova(housing.full, housing.m10) #full vs reduced model (no age, indus)

## variable selection
library(leaps)
nvar = ncol(housing) - 1
varsel = regsubsets(medv~., data=housing, nvmax=nvar)
varsel.res = summary(varsel)
names(varsel.res) #available results from selection

# compile results
varsel.metric = cbind(1:nvar, varsel.res$adjr2, varsel.res$cp,
                      varsel.res$bic)
colnames(varsel.metric) = c("No. of Variables", "Adjusted R2",
                            "Mallow's Cp", "BIC")
varsel.metric

# plot of each metric used
plot(varsel.res$adjr2) #adjR2
plot(varsel.res$cp) #Mallow's Cp
plot(varsel.res$bic) #BIC

# list of suggested variables for the results of each metric used
varsel.res$which[which.max(varsel.res$adjr2),] #adjR2
varsel.res$which[which.min(varsel.res$bic),] #BIC

## analyzing residuals
library(nortest)
plot(housing.m10$residuals) #check if there are patterns
qqnorm(housing.m10$residuals) #Q-Q plot of data
qqline(housing.m10$residuals) #Q-Q line
ad.test(housing.m10$residuals) #Anderson-Darling
shapiro.test(housing.m10$residuals) #Shapiro-Wilk

## multicollinearity
library(car)
vif(housing.m10)

# if variables w/ vif>5 are excluded
housing.m8 = lm(medv~.-age-indus-rad-tax,data=housing)
vif(housing.m8) #vif of the new model
summary(housing.m8)$adj.r.squared #adjR2 of new model