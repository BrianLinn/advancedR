# Advanced R module 4

# Ridge regression
# install.packages("ElemStatLearn")  # Prostate cancer data
# install.packages("glmnet")         # Ridge and LASSO regression
# install.packages("caret")          # parameter tuning

library(ElemStatLearn)
library(car)
library(corrplot)
library(leaps)
library(glmnet)
library(caret)

# Check the prostate cancer data at 
# https://rdrr.io/cran/ElemStatLearn/man/prostate.html

# Load the data
data("prostate")

# Quick overview
View(prostate)
plot(prostate)
plot(prostate$gleason)
table(prostate$gleason)
# gleason values 8 and 9 could be exceptions
# Check how the gleason influences the target variable lpsa (level of prostate-specific antigen)
boxplot(prostate$lpsa~prostate$gleason)
# Can recode together with 7
prostate$gleason <- ifelse(prostate$gleason == 6, 0, 1)
table(prostate$gleason)

# Check the collinearity
corm <- cor(prostate)
corm
corrplot.mixed(corm)
# Some high correlations

# Define train and test sets (last variable in the dataset)
prostateTrain <- subset(prostate, train == TRUE)[, 1:9]
prostateTest <- subset(prostate, train == FALSE)[, 1:9]

# Best subsets
bestsubFit <- regsubsets(lpsa~., data=prostateTrain)
bsf.summ <- summary(bestsubFit)
which.min(bsf.summ$bic)
# Model with 3 features has the lowest
# Bayesian Information Criterion BIC value
plot(bsf.summ$bic, type = "l", xlab = "Features N", ylab = "BIC")
plot(bestsubFit, scale = "bic", main = "Best Subset")
# Features to use: lcavol, lweight, gleason

# Use OLS
olsFit <- lm(lpsa~lcavol+lweight+gleason, data = prostateTrain)
# Plot predicted vs actual
plot(olsFit$fitted.values, prostateTrain$lpsa, xlab = "Predicted", ylab = "Actual")
# Check with the test set
olsFitPred <- predict(olsFit, newdata = prostateTest)
plot(olsFitPred, prostateTest$lpsa, xlab = "Predicted", ylab = "Actual")
# Calculate the mean squared error
residOls <- prostateTest$lpsa - olsFitPred
mean(residOls^2)
# 0.5084126


# Ridge regression
# glmnet function needs matrix inputs
X <- as.matrix(prostateTrain[,1:8])
y <- prostateTrain[,9]
ridgeFit <- glmnet(X, y, family = "gaussian", alpha = 0)
print(ridgeFit)
# coefficient values vs L1-norm
# top x axis - number of coefficients (constant 8)
plot(ridgeFit, label = TRUE)
# plot changes of coefficients with λ
plot(ridgeFit, xvar = "lambda", label = TRUE)

# Show coefficient at specific λ
ridgeFit.coef <- coef(ridgeFit, s=0.1, exact = TRUE)
ridgeFit.coef
# Some are close to zero

# Plot deviance vs coefficients
plot(ridgeFit, xvar="dev", label = TRUE)

# Use λ = 0.1 for prediction
X.R <- as.matrix(prostateTest[,1:8])
y.R <- prostateTest[,9]

ridgeFitPred <- predict(ridgeFit, newx = X.R, type = "response", s = 0.1)
residRidge <- prostateTest$lpsa - ridgeFitPred
mean(residRidge^2)
# 0.4783559 - better thank OLS


# Basic matrix operations
# Vectors
rep(1,10)                 # replicate a number n times
seq(3,7)                  # Sequence of integers between 3 and 7
3:7                       # Equivalent sequence
seq(5,17,by=3)            # Every 3rd integer between 5 and 17
# Vector variables
x <- c(2,0,0,4)           # A vector with elements 2,0,0,4
assign("y", c(1,9,9,9))   # Another way to assign a vector
c(5,4,3,2) -> z           # Another way to assign a vector      
q = c(1,2,3,4)            # Another way to assign a vector
# Vector operations
x + y                     # Sums elements of two vectors
x * 4                     # Multiplies elements
sqrt(x)                   # Function applies to each element
# Vector elements
x <- c(2,0,0,4)
x[1]               # Select the first element
x[-1]              # Exclude the first element
x[1] <- 3; x       # Assign a value to the first element
x[-1] = 5; x       # Assign a value to all other elements
y <- c(1,9,9,9)
y < 8              # Compares each element, returns result as vector
y[4] = 1
y < 8
y[y<8] = 2; y      # Edits elements marked as TRUE in index vector

# Matrices, Arrays, factors, lists, data frames
# Array
x = c(1,2,3,4,5,6); x         # A simple vector
Y = array(x, dim=c(2,3)); Y   # A matrix from the vector - fill by columns
Z = matrix(x,2,3,byrow=F); Z  # A matrix from the vector - fill by columns
U = matrix(x,2,3,byrow=T); U  # A matrix from the vector - fill by rows

# Matrix operations
U <- matrix(c(1,2,3,4), 2, 2, byrow=T)
V <- matrix(c(5,6,7,8), 2, 2, byrow=T)
A <- matrix(c(1,2,3,4,5,6), 2, 3, byrow=T)
B <- matrix(c(1,2,3,4,5,6), 3, 2, byrow=T);

U; V
A; B

U + V; V + U        # Addition
V - U; U - V        # Subtraction
U %*% V; V %*% U    # Multiplication

U %*% A             # Conformable
U %*% B             # Non-comformable

cbind(U, V)         # Combine by columns
rbind(V, U)         # Combine by rows

cbind(U, A)         # Combine by columns
cbind(U, B)         # Combine by columns - error, number of rows must match
rbind(U, A)         # Combine by rows - error, number of columns must match
rbind(U, B)         # Combine by rows

# Determinants
U; det(U)    # -2
V; det(V)    # -2
A; det(A)    # Error
B; det(B)    # Error

# 3x3 example
C <- matrix(c(1, -2, 2, 2, 0, 1, 1, 1, -2), 3, 3, byrow=T)
C
det(C)       # -7

# Eigenvectors
D <- matrix(c(1, 4, 9, 1), 2, 2, byrow = T)
D
det(D)
eigen(D)

E <- matrix(c(2, 1, 1, 1, 2, 1, 1, 1, 2), 3, 3, byrow=T)
E
det(E)
eigen(E)


# Principal component analysis (PCA)
# Reading a data frame from a CSV file
TM = read.table("C:\\AdvancedR\\TM.csv",
                sep = ",", header = TRUE,
                stringsAsFactors = TRUE)

# Extracting numerical data only
TMPCAEFA <- TM[, c("TotalChildren", "NumberChildrenAtHome",
                   "HouseOwnerFlag", "NumberCarsOwned",
                   "BikeBuyer", "YearlyIncome", "Age")]

# PCA from the base installation
pcaBasic <- princomp(TMPCAEFA, cor = TRUE)
summary(pcaBasic)
plot(pcaBasic, main = "PCA Basic", col = "blue")


# Package psych functions used for PCA and EFA
# install.packages("psych")
library(psych)

# PCA unrotated
pcaTM_unrotated <- principal(TMPCAEFA, nfactors = 2, rotate = "none")
pcaTM_unrotated

# PCA varimax rotation
pcaTM_varimax <- principal(TMPCAEFA, nfactors = 2, rotate = "varimax")
pcaTM_varimax

# Biplots
biplot.psych(pcaTM_unrotated, cex = c(0.1, 2), main = "PCA Unrotated")
biplot.psych(pcaTM_varimax, cex = c(0.1, 2), main = "PCA Varimax")


# EFA unrotated
efaTM_unrotated <- fa(TMPCAEFA, nfactors = 2, rotate = "none")
efaTM_unrotated

# EFA varimax
efaTM_varimax <- fa(TMPCAEFA, nfactors = 2, rotate = "varimax")
efaTM_varimax

# EFA promax
efaTM_promax <- fa(TMPCAEFA, nfactors = 2, rotate = "promax")
efaTM_promax

# Plots
factor.plot(efaTM_unrotated,
            labels = rownames(efaTM_unrotated$loadings),
            title = "EFA Unrotated")
factor.plot(efaTM_varimax,
            labels = rownames(efaTM_varimax$loadings),
            title = "EFA Varimax")
factor.plot(efaTM_promax,
            labels = rownames(efaTM_promax$loadings),
            title = "EFA Promax")
fa.diagram(efaTM_unrotated, simple = FALSE,
           main = "EFA Unrotated")
fa.diagram(efaTM_varimax, simple = FALSE,
           main = "EFA Varimax")
fa.diagram(efaTM_promax, simple = FALSE,
           main = "EFA Promax")

