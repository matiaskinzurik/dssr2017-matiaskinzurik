# Setting our working directory 
setwd("~/Desktop/Program/")

# Read a file
# read.csv(file = "Bioscreen_Raw.csv", header = TRUE)
read.csv(file = "Bioscreen_Raw.csv", header = TRUE)
dat = read.csv(file = "Bioscreen_Raw.csv", header = TRUE)
head(dat)
class(dat)
dim(dat)

# define variables
Time = dat[, 1:1]
WT = dat[, 2:4]
Strain1 = dat[, 5:7]
Strain2 = dat[, 8:10]
Strain3 = dat[, 11:13]
Strain4 = dat[, 14:16]
Strain5 = dat[, 17:19]
Strain6 = dat[, 20:22]

# Checking variable definitions
WT
Strain1
Strain2
Strain3
Strain4
Strain5
Strain6
Time

# Calculate Averages for the three replicates
AvgWT = rowMeans(WT)
AvgStrain1 = rowMeans(Strain1)
AvgStrain2 = rowMeans(Strain2)
AvgStrain3 = rowMeans(Strain3)
AvgStrain4 = rowMeans(Strain4)
AvgStrain5 = rowMeans(Strain5)
AvgStrain6 = rowMeans(Strain6)

# Checking Avg variable definitions
AvgWT
AvgStrain1
AvgStrain2
AvgStrain3
AvgStrain4
AvgStrain5
AvgStrain6

# Calculate Standard Deviation for the three replicates
sdWT = apply(WT,1,sd)
sdStrain1 = apply(Strain1,1,sd)
sdStrain2 = apply(Strain2,1,sd)
sdStrain3 = apply(Strain3,1,sd)
sdStrain4 = apply(Strain4,1,sd)
sdStrain5 = apply(Strain5,1,sd)
sdStrain6 = apply(Strain6,1,sd)

# Checking sd variable definitions
sdWT
sdStrain1
sdStrain2
sdStrain3
sdStrain4
sdStrain5
sdStrain6

# Plot and determine marker size and type
plot(AvgWT, pch= 6, cex = .4)
plot(AvgStrain1, pch= 0, cex = .4)
plot(AvgStrain2, pch= 1, cex = .4)
plot(AvgStrain3, pch= 8, cex = .4)
plot(AvgStrain4, pch= 2, cex = .4)
plot(AvgStrain5, pch= 5, cex = .4)
plot(AvgStrain6, pch= 4, cex = .4)

# Plot them all at the same time

plot(AvgStrain6, pch= 4, cex = .4)
points(AvgStrain2, pch= 1, cex = .4)
points(AvgStrain3, pch= 8, cex = .4)
points(AvgStrain4, pch= 2, cex = .4)
points(AvgStrain5, pch= 5, cex = .4)
points(AvgStrain1, pch= 0, cex = .4)
points(AvgWT, pch= 6, cex = .4)


# Show Sigmoideal regression equation on plot
# function needed for visualization purposes
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

x = 1:172

# fitting non linear code 
# a, b and c are parameters that you put in not knowing what they are - guesses
# y = OD
fitWT <- nls(AvgWT~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain1 <- nls(AvgStrain1~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain2 <- nls(AvgStrain2~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain3 <- nls(AvgStrain3~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain4 <- nls(AvgStrain4~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain5 <- nls(AvgStrain5~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))
fitStrain6 <- nls(AvgStrain6~a/(1 + exp(-b * (x-c))),start=list(a=1,b=.5,c=25))

# visualization code
# get the coefficients using the coef function
# can later type in paramsXXX and you get the actual values of a, b and c in formula
paramsWT = coef(fitWT)
paramsStrain1 = coef(fitStrain1)
paramsStrain2 = coef(fitStrain2)
paramsStrain3 = coef(fitStrain3)
paramsStrain4 = coef(fitStrain4)
paramsStrain5 = coef(fitStrain5)
paramsStrain6 = coef(fitStrain6)

# calculate sigmoideal regression curve values
sigWT = sigmoid(paramsWT,x)
sigStrain1 = sigmoid(paramsStrain1,x)
sigStrain2 = sigmoid(paramsStrain2,x)
sigStrain3 = sigmoid(paramsStrain3,x)
sigStrain4 = sigmoid(paramsStrain4,x)
sigStrain5 = sigmoid(paramsStrain5,x)
sigStrain6 = sigmoid(paramsStrain6,x)

# plot everything
plot(sigStrain6,type="l", col = "red")
points(sigStrain5,type="l", col = "blue")
points(sigStrain4,type="l", col = "brown")
points(sigStrain3,type="l", col = "green")
points(sigStrain2,type="l", col = "orange")
points(sigStrain1,type="l", col = "grey")
points(sigWT,type="l", col = "purple")
points(AvgWT, pch= 6, cex = .4)
points(AvgStrain6, pch= 4, cex = .4)
points(AvgStrain2, pch= 1, cex = .4)
points(AvgStrain3, pch= 8, cex = .4)
points(AvgStrain4, pch= 2, cex = .4)
points(AvgStrain5, pch= 5, cex = .4)
points(AvgStrain1, pch= 0, cex = .4)

# localize and include text with formula on plot
text(150,y = .4 , labels = paramsWT)
text(150,y = 1.2 , labels = paramsStrain1)
text(150,y = 1.2 , labels = paramsStrain2)
text(150,y = 1.2 , labels = paramsStrain3)
text(150,y = 1.2 , labels = paramsStrain4)
text(150,y = 1.5 , labels = paramsStrain5)
text(150,y = 1.5 , labels = paramsStrain6)
