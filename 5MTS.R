# Time-series library
library(astsa)
library(MTS)

# cardiovascular mortality (cmort), temperature (tempr), and particulate levels (part)
# from the LA pollution study over the 10 year period 1970-1979.
x = cbind(cmort, tempr, part)

# Build VAR(1) model using MTS library
m.var1 = VARMA(x, p=1, q=0, include.mean=F)

# coefficients Phi (for AR) and Theta (for MA) are stored in Phi and Theta, respectively
# covariance matrix Sigma is in Sigma
m.var1$Phi
m.var1$Theta
m.var1$Sigma

# forecast values
m.var1.p = VARMApred(m.var1, h=5)