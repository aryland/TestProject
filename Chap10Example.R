# Chapter 10 Principal Components
# Data from An Introduction to Applied Multivariate Analysis (2008)
# Tenko Raykov & George A. Marcoulides
# Routledge

######################
#  Install Packages
######################

install.packages(psych)
install.packages("rela")
install.packages(MASS)
install.packages("parallel")
library(psych);library(rela);library(MASS);library(parallel)

#  Return decimal values

options(scipen=999)  # report decimal values 

#################################################################################
# Obtain data from website (http://www.psypress.com/books/details/9780805863758/)
# Download Zip file and extract data files to computer directory
#################################################################################

setwd("C:/")
pca = read.delim(file = "C:/ch7ex1.dat",header=TRUE,sep="\t")
colnames(pca) = c("IRLETT", "FIGREL", "IRSYMB","CULTFR","RAVEN")
pca = as.matrix(pca)      # specify as matrix for use in other packages
head(pca,10)
tail(pca,10)

##########################################################################
# correlation matrix with p values and confidence intervals
##########################################################################

pcacor = cor(pca)
pcacor

pcacov = cov(pca)
pcacov

# Convert covariance matrix to correlation matrix

convert = cov2cor(pcacov)
convert

###########################################
# output p values of correlations in matrix
corr.p(cor(pca),161,alpha=.05,adjust="none")

##########################################################
# Bartlett sphericity test and KMO test of sampling adquacy
# rela package with paf() using correlation matrix
###########################################################

paf.pca = paf(pca,eigcrit=1,convcrit=.001)
summary(paf.pca)

# Test significance of the Bartlett test using correlation matrix

cortest.bartlett(pcacor, n = 161)

# Determinant of correlation matrix

det(pcacor)

#############################################
Principal Component Analysis in psych package
#############################################

pcmodel1 = principal(pcacor,rotate="none")  # default with 1 component and no scores
pcmodel1


pcmodel2 = principal(pcacor,nfactors=5,rotate="none")  # default with 1 component and no scores
pcmodel2

# Cronbach Alpha Reliability

alpha(pcacor)

####################
Scree plots
###################

# Uses values from principal components model

plot(pcmodel2$values, type = "b", xlim=c(1,10),main = "Scree Plot",xlab="Number of Factors",ylab="Eigenvalues")

# Uses raw data set

fa.parallel(pca, n.obs=161,fm="pa",fa="pc")


#  Plot component model 

fa.diagram(pcmodel2)

###########################
# compute scale score
###########################

pca2 = data.frame(pca)
attach(pca2)
pcscores = .87*IRLETT +.86*FIGREL +.92*IRSYMB + .88*CULTFR + .81*RAVEN
pcscores = sort(pcscores,decreasing=FALSE)
pcscores

#  Use high and low scores in formula
# s = (100)/(329.275 -(-66.213)) = 100 / 395.488 = .25285
# m = (0) - (66.213 * .25285) = 16.742

# compute scaled scores

pcscaled = 16.742 + (.25285 * pcscores)
round(pcscaled,2)

# Graph component scores and scaled scores

par(mfrow=c(1,2))
hist(pcscores)
hist(pcscaled)






