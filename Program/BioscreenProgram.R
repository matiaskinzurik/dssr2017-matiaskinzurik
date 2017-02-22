# Setting our working directory 
setwd("~/Desktop/Program/")

library("tools")
library(ggplot2)
library(grid)
library(gridExtra)

Open_File = function(filename) {
  
  if(toupper(file_ext(filename)) != "CSV") 
    stop("File is not a CSV. Please load CSV file")
    dat = read.csv(file = filename, header = TRUE)
  
  return(dat)
}

# Read a file

dat = Open_File("Bioscreen_Raw.csv")

# Define variables
Time = dat[, 1:1]
WT = dat[, 2:4]
Strain1 = dat[, 5:7]
Strain2 = dat[, 8:10]
Strain3 = dat[, 11:13]
Strain4 = dat[, 14:16]
Strain5 = dat[, 17:19]
Strain6 = dat[, 20:22]

# Calculate Averages for the three replicates
AvgWT = rowMeans(WT)
AvgStrain1 = rowMeans(Strain1)
AvgStrain2 = rowMeans(Strain2)
AvgStrain3 = rowMeans(Strain3)
AvgStrain4 = rowMeans(Strain4)
AvgStrain5 = rowMeans(Strain5)
AvgStrain6 = rowMeans(Strain6)

# Calculate Sigmoideal regression equation of curves
# function needed for visualization purposes
sigmoid = function(params, Time) {
  params[1] / (1 + exp(-params[2] * (Time - params[3])))
}

x = 1:172

# Fitting non linear code 
# a, b and c are parameters that you put in not knowing what they are = guesses
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
paramsWT = round(coef(fitWT), 2)
paramsStrain1 = round(coef(fitStrain1), 2)
paramsStrain2 = round(coef(fitStrain2), 2)
paramsStrain3 = round(coef(fitStrain3), 2)
paramsStrain4 = round(coef(fitStrain4), 2)
paramsStrain5 = round(coef(fitStrain5), 2)
paramsStrain6 = round(coef(fitStrain6), 2)

# calculate sigmoideal regression curve values
sigWT = sigmoid(paramsWT,x)
sigStrain1 = sigmoid(paramsStrain1,x)
sigStrain2 = sigmoid(paramsStrain2,x)
sigStrain3 = sigmoid(paramsStrain3,x)
sigStrain4 = sigmoid(paramsStrain4,x)
sigStrain5 = sigmoid(paramsStrain5,x)
sigStrain6 = sigmoid(paramsStrain6,x)

# Plot everything

Allplots = function(){
  plot(sigStrain6,type="l", col = "red", main = "OD over time", xlab = "Time", ylab = "OD");
  points(sigStrain5,type="l", col = "blue");
  points(sigStrain4,type="l", col = "brown");
  points(sigStrain3,type="l", col = "green");
  points(sigStrain2,type="l", col = "orange");
  points(sigStrain1,type="l", col = "grey");
  points(sigWT,type="l", col = "purple");
  points(AvgWT, pch= 6, cex = .4);
  points(AvgStrain6, pch= 4, cex = .4);
  points(AvgStrain2, pch= 1, cex = .4);
  points(AvgStrain3, pch= 8, cex = .4);
  points(AvgStrain4, pch= 2, cex = .4);
  points(AvgStrain5, pch= 5, cex = .4);
  points(AvgStrain1, pch= 0, cex = .4)
}

Allplots()

# Writting down sigmoideal formula and adding text on plot
FormulaStrain1 = paste0("y = ",paramsStrain1[1], length = 2, "/(1+e(-",paramsStrain1[2],"*(x-",paramsStrain1[3],")))")
FormulaStrain2 = paste0("y = ",paramsStrain2[1],"/(1+e(-",paramsStrain2[2],"*(x-",paramsStrain2[3],")))")
FormulaStrain3 = paste0("y = ",paramsStrain3[1],"/(1+e(-",paramsStrain3[2],"*(x-",paramsStrain3[3],")))")
FormulaStrain4 = paste0("y = ",paramsStrain4[1],"/(1+e(-",paramsStrain4[2],"*(x-",paramsStrain4[3],")))")
FormulaStrain5 = paste0("y = ",paramsStrain5[1],"/(1+e(-",paramsStrain5[2],"*(x-",paramsStrain5[3],")))")
FormulaStrain6 = paste0("y = ",paramsStrain6[1],"/(1+e(-",paramsStrain6[2],"*(x-",paramsStrain6[3],")))")
FormulaWT = paste0("y = ",paramsWT[1],"/(1+e(-",paramsWT[2],"*(x-",paramsWT[3],")))")

# Create Table with data from sigmoid equation calculation

EqTable = matrix(c(FormulaWT, FormulaStrain1, FormulaStrain2, FormulaStrain3, FormulaStrain4, FormulaStrain5, FormulaStrain6),ncol=1,byrow=TRUE)
colnames(EqTable) = c("Curve Equations")
rownames(EqTable) = c("WT", "Strain 1", "Strain 2", "Strain 3", "Strain 4", "Strain 5", "Strain 6")

EqTable = as.table(EqTable)

EqTable

# Place multiple graphs per page

par(mfrow=c(1,2))
Allplots()
plot.new()
plot.window(xlim=c(0,100), ylim=c(0,100))
text(20,35, paste(EqTable, collapse='\n'), adj = c(0,0))
text(-4,35, paste(rownames(EqTable), collapse='\n'), adj = c(0,0))
text(28,58, paste(colnames(EqTable), collapse='\n'), adj = c(0,0))
par(mfrow=c(1,1))

# Statistical analysis
# Calculate Standard Deviation for the three replicates
sdWT = apply(WT,1,sd)
sdStrain1 = apply(Strain1,1,sd)
sdStrain2 = apply(Strain2,1,sd)
sdStrain3 = apply(Strain3,1,sd)
sdStrain4 = apply(Strain4,1,sd)
sdStrain5 = apply(Strain5,1,sd)
sdStrain6 = apply(Strain6,1,sd)
