###########################################################################
# File name: imputation.R
# Author: Osman Mahic
# Date: 27-01-2024
# Description: R code for performing multi-level multiple imputation
###########################################################################

#Load packages
library(mice)
library(miceadds)
library(lme4) # for longitudinal imputation

# Prepare data, set numeric/factor
sapply(Final_data5, class)
Final_data5$ntx <- as.numeric(Final_data5$ntx)

# Make imputation model
ini <- mice(Final_data5, maxit = 0) # empty imputation model
ini
meth <- ini$meth # Extract imputation methods
meth
pred <- ini$predictorMatrix # Extract predictor matrix

# Specify predictor matrix
meth[c("studynr")] <- "" # don't impute these variables
meth[c("age")] <- ""
meth[c("sex")] <- ""
pred[, "studynr"] <- -2 # for class/cluster variable, 0 if non-longitudinal
pred[, "mtpnt"] <- 0 # time, 2 for random effects, or leave 0

# Specify methods
meth[c(3:11, 13, 15, 18, 19, 20)] <- "2l.pmm" # Specify methods, here longitudinal ‘2L’
meth[c(12)] <- "2lonly.pmm" # Specify methods, ‘only’=baseline covariates only
meth[c(14, 21, 22)] <- "2lonly.pmm" # Specify methods

# Execute imputation, m datasets and iterations 20
imputed <- mice(Final_data5, meth = meth, pred = pred, print = FALSE, maxit = 20, m = 10)

# Check if it worked
imputedData1 <- complete(imputed, 1)
sum(is.na(imputedData1))
densityplot(imputed) # diagnostics

# Save imputed data sets
imp_long_final <- complete(imputed, action = 'long', include = TRUE)

# Note: After imputation (i.e., regardless of additional data wrangling) ensure that the imputed datasets are in the mids object format before pooling the estimates.
