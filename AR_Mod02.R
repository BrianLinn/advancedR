# Advanced R module 2

# Anscombe data set
data("anscombe")
View(anscombe)
attach(anscombe)

# Correlations
cor(x1, y1)
cor(x2, y2)
cor(x3, y3)
cor(x4, y4)
# All 0.8165214

# View the data

# Defining a 2x2 graph
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

plot(x1, y1, main = '1')
plot(x2, y2, main = '2')
plot(x3, y3, main = '3')
plot(x4, y4, main = '4')

# Clean up
par(oldpar)
detach(anscombe)

# Load RODBC library (install only if needed)
# install.packages("RODBC")
library(RODBC)

# Connecting and reading the data
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TM <- as.data.frame(sqlQuery(con,
                             "SELECT EnglishEducation AS Education,
                             Age
                             FROM dbo.vTargetMail;"),
                    stringsAsFactors = TRUE)
close(con)

# Plot the Education
plot(TM$Education, main = 'Education',
     xlab = 'Education', ylab = 'Number of Cases',
     col = "light blue")

# Education is ordered
TM$Education = factor(TM$Education, order = TRUE,
                      levels = c("Partial High School",
                                 "High School", "Partial College",
                                 "Bachelors", "Graduate Degree"))
# Plot it again
plot(TM$Education, main = 'Education',
     xlab = 'Education', ylab = 'Number of Cases',
     col = "dark green")


# Get dummies in R
# install.packages("dummies")
library(dummies)

# Create the dummies
TM1 <- cbind(TM, dummy(TM$Education, sep = "_"))
# Last 10 rows
tail(TM1, 10)

# Descriptive statistics
attach(TM);
# A quick summary for the whole dataset
summary(TM);

# A quick summary for Age
summary(Age);
# Details for Age
mean(Age);
median(Age);
min(Age);
max(Age);
range(Age);
quantile(Age, 1/4);
quantile(Age, 3/4);
IQR(Age);
var(Age);
sd(Age);

# Custom function for skewness and kurtosis
skewkurt <- function(p){
  avg <- mean(p)
  cnt <- length(p)
  stdev <- sd(p)
  skew <- sum((p-avg)^3/stdev^3)/cnt
  kurt <- sum((p-avg)^4/stdev^4)/cnt-3
  return(c(skewness=skew, kurtosis=kurt))
};
skewkurt(Age);

# Basic plot
plot(Age)
# Histogram
hist(Age)
hist(Age, 
     main="Histogram for Age", 
     xlab="Age", 
     border="blue", 
     col="yellow",
     xlim=c(0, 100),
     las=1, 
     breaks=15)

# Clean up
detach(TM)


# Handling NULLs
library(RODBC)
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
NULLTest <-
  sqlQuery(con,
           "SELECT c1, c2, c3
          FROM dbo.NULLTest;")
close(con)
NULLTest

# Working with NULLs
na.omit(NULLTest)
is.na(NULLTest)

# Aggregate functions
mean(NULLTest$c2)
mean(NULLTest$c2, na.rm=TRUE)


# Data manipulation with dplyr
# Connecting and reading the data
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TM <- as.data.frame(sqlQuery(con,
                             "SELECT c.CustomerKey,
                             g.EnglishCountryRegionName AS Country,
                             c.EnglishEducation AS Education,
                             c.YearlyIncome AS Income,
                             c.NumberCarsOwned AS Cars,
                             C.MaritalStatus,
                             c.NumberChildrenAtHome
                             FROM dbo.DimCustomer AS c
                             INNER JOIN dbo.DimGeography AS g
                             ON c.GeographyKey = g.GeographyKey;"),
                    stringsAsFactors = TRUE)
close(con)

# a quick demo barplot
# $ Notation
mg <- table(TM$Country, TM$Cars);
mg
barplot(mg,
        main = 'Cars in countries',
        xlab = 'Cars owned', ylab = 'Country',
        col = c("blue", "yellow", "red", "green", "orange", "black"),
        beside = TRUE,
        legend = TRUE)


# Package dplyr
# install.packages("dplyr")
library(dplyr)

# Quick overview
glimpse(TM)

# Projection
head(TM)
head(select(TM, Income:MaritalStatus))
head(select(TM, - Income))
head(select(TM, starts_with("C")))

# Filter
# All data frame has 18484 cases
count(TM)
# 2906 cases with more than 2 cars
count(filter(TM, Cars > 2))
# Check
table(TM$Cars)

# Sort
head(arrange(TM, desc(CustomerKey)))

# Pipe operator
TM %>%
  select(starts_with("C")) %>%
  filter(Cars > 2) %>%
  count

# Projection, filter, sort
TM %>%
  select(starts_with("C")) %>%
  filter(Cars > 2) %>%
  arrange(desc(CustomerKey)) %>%
  head

# Adding a column
TM %>%
  filter(Cars > 0) %>%
  mutate(PartnerExists = as.integer(ifelse(MaritalStatus == 'S', 0, 1))) %>%
  mutate(HouseHoldNumber = 1 + PartnerExists + NumberChildrenAtHome) %>%
  select(CustomerKey, Country, HouseHoldNumber, Cars) %>%
  arrange(desc(CustomerKey)) %>%
  head

# Aggregating
TM %>%
  filter(Cars > 0) %>%
  mutate(PartnerExists = as.integer(ifelse(MaritalStatus == 'S', 0, 1))) %>%
  mutate(HouseHoldNumber = 1 + PartnerExists + NumberChildrenAtHome) %>%
  select(CustomerKey, Country, HouseHoldNumber, Cars) %>%
  summarise(avgCars = mean(Cars),
            avgHouseHoldNumber = mean(HouseHoldNumber))

# Grouping and aggregating
TM %>%
  filter(Cars > 0) %>%
  mutate(PartnerExists = as.integer(ifelse(MaritalStatus == 'S', 0, 1))) %>%
  mutate(HouseHoldNumber = 1 + PartnerExists + NumberChildrenAtHome) %>%
  select(CustomerKey, Country, HouseHoldNumber, Cars) %>%
  group_by(Country) %>%
  summarise(avgCars = mean(Cars),
            avgHouseHoldNumber = mean(HouseHoldNumber)) %>%
  arrange(desc(Country))

# Storing in a df
TM1 =
  TM %>%
  filter(Cars > 0) %>%
  mutate(PartnerExists = as.integer(ifelse(MaritalStatus == 'S', 0, 1))) %>%
  mutate(HouseHoldNumber = 1 + PartnerExists + NumberChildrenAtHome) %>%
  select(CustomerKey, Country, HouseHoldNumber, Cars) %>%
  group_by(Country) %>%
  summarise(avgCars = mean(Cars),
            avgHouseHoldNumber = mean(HouseHoldNumber)) %>%
  arrange(desc(Country))
TM1

# Basic scatterplot
plot(TM1$avgCars ~ TM1$avgHouseHoldNumber, cex = 2, lwd = 2)


# Package Car scatterplot
# install.packages("car")
library(car)
scatterplot(avgCars ~ avgHouseHoldNumber | Country,
            data = TM1,
            xlab = "HouseHoldNumber Avg", ylab = "Cars Avg",
            main = "Enhanced Scatter Plot",
            cex = 3.5, lwd = 15,
            cex.lab = 1.3,
            xlim = c(2.2, 3.6), ylim = c(1.7, 2.2),
            col = c('red', 'blue', 'green', 'black', 'orange', 'magenta'),
            boxplot = 'xy')


# Aggregations in groups
aggregate(TM$Income, by = list(TM$Country), FUN = sum)

# More grouping
aggregate(TM$Income,
          by = list(TM$Country, TM$Education),
          FUN = mean)

# Aggregations in groups with a custom function
aggregate(TM$Income, by = list(TM$Country), FUN = skewkurt)

# Filtering aggregations
TMAGG <- aggregate(list(Income = TM$Income),
                   by = list(Country = TM$Country), FUN = mean)
TMAGG[TMAGG$Income > 60000,]

# Plot the income mean over countries
barplot(TMAGG$Income,
        legend = TMAGG$Country,
        col = c('blue', 'yellow', 'red', 'green', 'magenta', 'black'))


# Pivoting and Transposing Data

# Connecting and reading the data
library(RODBC)
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
SGY <- as.data.frame(sqlQuery(con,
                              "SELECT Country, State, CountryState, CYear, Sales
                              FROM dbo.SalesGeoYear;"),
                     stringsAsFactors = TRUE)
close(con)
View(SGY)


# Transposing data
# t() function
t(SGY)

# Using row.names
SNA <- SGY
row.names(SNA) <-
  paste(SNA$CountryState, SNA$CYear, sep = ', ')
SNA <- SNA[c("Country", "State", "Sales")]
t(SNA)

# Pivoting
# Counts
table(SGY$Country, SGY$CYear)
# Sums
xtabs(Sales ~ Country + CYear, data = SGY)
xtabs(Sales ~ CountryState + CYear, data = SGY)
# Different aggregate functions
tapply(SGY$Sales, list(SGY$Country, SGY$CYear), FUN = length)
tapply(SGY$Sales, list(SGY$Country, SGY$CYear), FUN = sum)

# RevoScaleR
library(RevoScaleR)
# Crosstabulation
rxCube(formula = Sales ~
         Country:F(CYear),
       data = SGY, means = FALSE)
# Another way
# Crosstabulation object
cTabs <- rxCrossTabs(formula = Sales ~
                       Country:F(CYear),
                     data = SGY)
# Check the results
print(cTabs, output = "counts")
print(cTabs, output = "sums")
print(cTabs, output = "means")

# Histogram
rxHistogram(formula = ~ Country|F(CYear),
            data = SGY)


# Package reshape
# install.packages("reshape")
library(reshape)
cast(formula = Country ~ CYear,
     value = "Sales",
     data = SGY,
     fun.aggregate = sum)

# Package tidyr - transpose
# install.packages("tidyr")
library(tidyr)
spread(data = SGY[c("CountryState", "CYear", "Sales")], key = CYear, value = Sales)

# Package tidyr + dplyr - pivot
# install.packages("dplyr")
library(dplyr)
SGY[c("Country", "CYear", "Sales")] %>%
  group_by(Country, CYear) %>%
  summarise(Sales = sum(Sales)) %>%
  spread(key = CYear, value = Sales)


# Unpivoting Data
# Connecting and reading the data
library(RODBC)
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
SGY <- as.data.frame(sqlQuery(con,
                              "SELECT Country, [2010], [2011], [2012], [2013], [2014]
                              FROM dbo.SalesPivoted;"),
                     stringsAsFactors = TRUE)
close(con)
View(SGY)

# Simple stack()
stack(SGY[-1])
# Adding countries
rbind(SGY[1], SGY[1], SGY[1], SGY[1], SGY[1])
cbind(rbind(SGY[1], SGY[1], SGY[1], SGY[1], SGY[1]), stack(SGY[-1]))

# Library reshape melt()
library(reshape)
melt(SGY, id = c("Country"))


# Library tidyr gather()
library(dplyr)
library(tidyr)
SUO <-
gather(SGY, key = CYear, value = Sales, - Country) %>%
  arrange(Country, CYear)
SUO

# Donut chart
library(plotly)
plot_ly(data = SUO, labels = ~SUO$Country, values = ~SUO$Sales) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Sales by Country", showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Binning
# Connecting and reading the data
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TMAge <- as.data.frame(sqlQuery(con,
                                "SELECT CustomerKey, Age
                                FROM dbo.TMAge;"),
                       stringsAsFactors = TRUE)
close(con)

# Equal width binning
TMAge["AgeEWB"] = cut(TMAge$Age, 5)
table(TMAge$AgeEWB)

# Equal height binning - step by step
# Lower limit for the number of cases in a bin
length(TMAge$Age) %/% 5
# Create the vector of the bins with number of cases
rep(length(TMAge$Age) %/% 5, 5)
# How many bins need a case more
length(TMAge$Age) %% 5
# Array to add cases to the first 4 bins
ifelse(1:5 <= length(TMAge$Age) %% 5, 1, 0)

# Equal height binning - a function
EHBinning <- function(data, nofbins) {
  bincases <- rep(length(data) %/% nofbins, nofbins)
  bincases <- bincases + ifelse(1:nofbins <= length(data) %% nofbins, 1, 0)
  bin <- rep(1:nofbins, bincases)
  bin <- bin[rank(data, ties.method = "last")]
  return(factor(bin, levels = 1:nofbins, ordered = TRUE))
}
TMAge["AgeEHB"] = EHBinning(TMAge$Age, 5)
table(TMAge$AgeEHB)

# Custom binning
TMAge["AgeCUB"] = cut(TMAge$Age, c(17, 23, 30, 40, 55, 90))
table(TMAge$AgeCUB)


# Entropy
# Connecting and reading the data
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TM <- as.data.frame(sqlQuery(con,
                             "SELECT CustomerKey, NumberCarsOwned, BikeBuyer, GenMar
                             FROM dbo.TM;"),
                    stringsAsFactors = TRUE)
close(con)

# Check the data frame
str(TM)

# Histogram from RevoScaleR
# Entropy (install only if needed) from DescTools
# install.packages("DescTools");
library("RevoScaleR")
library("DescTools")

# NumberCarsOwned 
table(TM$NumberCarsOwned)
rxHistogram(formula = ~NumberCarsOwned,
            data = TM)
paste('mean:', mean(TM$NumberCarsOwned))
paste('sd  :', sd(TM$NumberCarsOwned))
paste('CV  :', sd(TM$NumberCarsOwned) / mean(TM$NumberCarsOwned))


# BikeBuyer 
table(TM$BikeBuyer)
rxHistogram(formula = ~BikeBuyer,
            data = TM)
paste('mean:', mean(TM$BikeBuyer))
paste('sd  :', sd(TM$BikeBuyer))
paste('CV  :', sd(TM$BikeBuyer) / mean(TM$BikeBuyer))

# GenMar
table(TM$GenMar)
rxHistogram(formula = ~GenMar,
            data = TM)


# Entropy
NCOT = table(TM$NumberCarsOwned)
print(c(Entropy(NCOT), log2(5), Entropy(NCOT) / log2(5)))
BBT = table(TM$BikeBuyer)
print(c(Entropy(BBT), log2(2), Entropy(BBT) / log2(2)))
GenMarT = table(TM$GenMar)
print(c(Entropy(GenMarT), log2(4), Entropy(GenMarT) / log2(4)))



# Bonus code
# Drawing different distributions

# Normal distribution
n <- 10000;
x <- rnorm(n);
hist(x,
     xlim = c(min(x), max(x)), probability = T, nclass = 27,
     col = 'light yellow', xlab = ' ', ylab = ' ', axes = F,
     main = 'Normal Distribution');
lines(density(x), col = 'dark blue', lwd = 3);

# Positively skewed distribution
n <- 10000;
x <- rnbinom(n, 10, .5);
hist(x,
     xlim = c(min(x), max(x)), probability = T, nclass = max(x) - min(x) + 1,
     col = 'light yellow', xlab = ' ', ylab = ' ', axes = F,
     main = 'Positively Skewed Distribution');
lines(density(x, bw = 1), col = 'dark blue', lwd = 3);

# Peaked distribution
# install.packages("SuppDists");
library(SuppDists);
parms <- JohnsonFit(c(0, 1, 0, 5), moment = "use");
x <- rJohnson(1000, parms);
hist(x, main = 'Tailed Distribution',
     xlim = c(min(x), max(x)), probability = T, nclass = 40,
     col = 'light yellow', xlab = ' ', ylab = ' ', axes = F);
lines(density(x, bw = 0.3), col = 'dark blue', lwd = 3);

