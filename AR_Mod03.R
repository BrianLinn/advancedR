# Advanced R module 3

# Re-read the TM dataset 
TM = read.table("C:\\AdvancedR\\TM.csv",
                sep = ",", header = TRUE,
                stringsAsFactors = TRUE)
attach(TM)
View(TM)

# Education is ordered
Education = factor(Education, order = TRUE,
                   levels = c("Partial High School",
                              "High School", "Partial College",
                              "Bachelors", "Graduate Degree"))
plot(Education, main = 'Education',
     xlab = 'Education', ylab = 'Number of Cases',
     col = "purple")

# Crosstabulation with table() and xtabs()
table(Education, Gender, BikeBuyer)
table(NumberCarsOwned, BikeBuyer)
xtabs(~Education + Gender + BikeBuyer)
xtabs(~NumberCarsOwned + BikeBuyer)

# Storing tables in objects
tEduGen <- xtabs(~Education + Gender)
tNcaBik <- xtabs(~NumberCarsOwned + BikeBuyer)

# Test of independece
chisq.test(tEduGen)
chisq.test(tNcaBik)

summary(tEduGen)
summary(tNcaBik)

# Installing and loading the vcd package
# install.packages("vcd")
library(vcd)

# Measures of association
assocstats(tEduGen)
assocstats(tNcaBik)

# Visualizing the crosstabulation
# Showing expected and observed frequencies
strucplot(tNcaBik, residuals = NULL, shade = TRUE,
          gp = gpar(fill = c("yellow", "blue")),
          type = "expected", main = "Expected")
strucplot(tNcaBik, residuals = NULL, shade = TRUE,
          gp = gpar(fill = c("yellow", "blue")),
          type = "observed", main = "Observed")


# Mutual information
# install.packages("DescTools");
library("DescTools")
MutInf(NumberCarsOwned, Gender);
MutInf(NumberCarsOwned, HouseOwnerFlag);
MutInf(NumberCarsOwned, Age);
MutInf(NumberCarsOwned, CommuteDistance);
MutInf(NumberCarsOwned, YearlyIncome);


# Covariance and correlations

# Pearson
x <- TM[, c("YearlyIncome", "Age", "NumberCarsOwned")]
cov(x)
cor(x)

# Spearman
y <- TM[, c("TotalChildren", "NumberChildrenAtHome", "HouseOwnerFlag", "BikeBuyer")]
cor(y)
cor(y, method = "spearman")

# Two matrices correlations
cor(y, x)

# Visualizing the correlations
# install.packages("corrgram")
library(corrgram)
corrgram(y, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.shade, text.panel = panel.txt,
         cor.method = "spearman", main = "Corrgram")


# Continuous and discrete variables

# T-test
t.test(YearlyIncome ~ Gender)
t.test(YearlyIncome ~ HouseOwnerFlag)
# Error - t-test supports only two groups
t.test(YearlyIncome ~ Education)

# Visualizing the associations
boxplot(YearlyIncome ~ Gender,
        main = "Yearly Income in Groups",
        ylab = "Yearly Income",
        xlab = "Gender")
boxplot(YearlyIncome ~ HouseOwnerFlag,
        main = "Yearly Income in Groups",
        notch = TRUE,
        varwidth = TRUE,
        col = "orange",
        ylab = "Yearly Income",
        xlab = "House Owner Flag")


# One-way ANOVA
aggregate(YearlyIncome, by = list(Education), FUN = mean)
aggregate(YearlyIncome, by = list(Education), FUN = sd)
AssocTest <- aov(YearlyIncome ~ Education)
summary(AssocTest)

# Education is ordered
Education = factor(Education, order = TRUE,
                   levels = c("Partial High School",
                              "High School", "Partial College",
                              "Bachelors", "Graduate Degree"))
# Visualizing ANOVA
boxplot(YearlyIncome ~ Education,
        main = "Yearly Income in Groups",
        notch = TRUE,
        varwidth = TRUE,
        col = "orange",
        ylab = "Yearly Income",
        xlab = "Education")

# Load gplots - another way to plot group means
library(gplots)
plotmeans(YearlyIncome ~ Education,
          bars = TRUE, p = 0.99, barwidth = 3,
          col = "red", lwd = 3,
          main = "Yearly Income in Groups",
          ylab = "Yearly Income",
          xlab = "Education")


# ggplot
# install.packages("ggplot2")
library("ggplot2")

# Plots with count (number) Education by Region
ggplot(TM, aes(Region, fill = Education)) +
  geom_bar(position = "stack")

ggplot(TM, aes(Region, fill = Education)) +
  geom_bar(position = "fill")

ggplot(TM, aes(Region, fill = Education)) +
  geom_bar(position = "dodge")


# A smaller data frame for the purpuse of graph
TMLM <- TM[1:100, c("YearlyIncome", "Age")]

# Plot the data points
plot(TMLM$Age, TMLM$YearlyIncome,
     cex = 2, col = "orange", lwd = 2)
# Plots with ggplot
# Basic
ggplot(data = TMLM, aes(x = Age, y = YearlyIncome)) +
  geom_point()

# Plot with a Lowess line
plot(TMLM$Age, TMLM$YearlyIncome,
     cex = 2, col = "orange", lwd = 2)
lines(lowess(TMLM$Age, TMLM$YearlyIncome),
      col = "blue", lwd = 2)

# With ggplot - linear + loess
ggplot(data = TMLM, aes(x = Age, y = YearlyIncome)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(color = "blue")

# Education is ordered
TM$Education = factor(TM$Education, order = TRUE,
                      levels = c("Partial High School",
                              "High School", "Partial College",
                              "Bachelors", "Graduate Degree"))
# Boxplot
boxplot(TM$YearlyIncome ~ TM$Education,
        main = "Yearly Income in Groups",
        notch = TRUE,
        varwidth = TRUE,
        col = "orange",
        ylab = "Yearly Income",
        xlab = "Education")

# Boxplot with ggplot
ggplot(TM, aes(x = Education, y = YearlyIncome)) +
  geom_boxplot(fill = "orange",
               color = "blue", notch = FALSE)

# Boxplot and violin plot with ggplot
ggplot(TM, aes(x = Education, y = YearlyIncome)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(fill = "orange",
               width = 0.2)

# Density plot
ggplot(TM, aes(x = YearlyIncome, fill = Education)) +
  geom_density(alpha = 0.3)

# Trellis charts
ggplot(TM, aes(x = NumberCarsOwned, fill = Region)) +
  geom_bar(stat = "bin") +
  facet_grid(MaritalStatus ~ BikeBuyer) +
  theme(text = element_text(size = 30))

ggplot(TM, aes(x = NumberCarsOwned, fill = Region)) +
  geom_bar(stat = "bin") +
  facet_grid(MaritalStatus ~ BikeBuyer) +
  theme(text = element_text(size = 30))

ggplot(TM, aes(x = Education, y = BikeBuyer, fill = Region)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ Region) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45))

# Jointplot
# install.packages("ggExtra")
library(ggExtra)

plot1 <- ggplot(TMLM, aes(x=Age,y=YearlyIncome)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(color = "blue")

# default: type="density"
ggMarginal(plot1, type="histogram", bins=20)
ggMarginal(plot1, type="density")

# Detach TM
detach(TM)

# Bayesian inference
# Package e1071 
# install.packages("e1071", dependencies = TRUE)
library(e1071)

str(TM)

# Build the Naive Bayes model
TMNB <- naiveBayes(TM[,2:11], TM[,12])

# Apriori probabilities for the target variable (BikeBuyer)
TMNB$apriori
# Apriori probabilities for the input variables in classes of the target variable
TMNB$tables

# Predicitions
predict(TMNB, TM, type = "raw")

# Data frame with predictions for all rows
df_PR <- as.data.frame(predict(
  TMNB, TM, type = "raw"))

# Combine original data with predictions
df_TM_PR <- cbind(TM, df_PR)
View(df_TM_PR)

# Linear regression
# A smaller data frame 
TMLM <- TM[1:100, c("YearlyIncome", "Age")]
# Adding the smaller data frame to the search path
attach(TMLM)

# Plot the data points
plot(Age, YearlyIncome,
     cex = 2, col = "orange", lwd = 2)

# Simple linear regression model
LinReg1 <- lm(YearlyIncome ~ Age)
summary(LinReg1)

# Polynomial  regression
LinReg2 <- lm(YearlyIncome ~ Age + I(Age ^ 2))
summary(LinReg2)

LinReg3 <- lm(YearlyIncome ~ Age + I(Age ^ 2) + I(Age ^ 3))
summary(LinReg3)

# Visualization
plot(Age, YearlyIncome,
     cex = 2, col = "orange", lwd = 2)
abline(LinReg1,
       col = "red", lwd = 2)
lines(lowess(Age, YearlyIncome),
      col = "blue", lwd = 2)

# Plot the first model to check the validity of the assumptions
plot(LinReg1)
# Residuals vs Fitted plot has U shape - potential linearity problem
# Normal Q-Q (quantile-quantile) exposes potential outliers
# Scale-Location shows U shape, potential heteroscedasticity
# Residuals vs Leverage - Cook's distance > 1 might show outliers problem

# package car qqPlot also shows confidence interval
# install.packages("car")
library(car)
qqPlot(LinReg1)

# can see some problematic outliers

# Check polynomial regressions
qqPlot(LinReg2)
qqPlot(LinReg3)
# The LinReg3 model looks the best

# Removing the smaller data frame from the search path
detach(TMLM)

