# Advanced R module 1

# Basic expressions
1 + 2
2 + 5 * 4
3 ^ 4
sqrt(81)
pi

# Check the built-in constants
?? "constants"

# Sequences
rep(1, 5)
4:8
seq(4, 8)
seq(4, 20, by = 3)

# Variables
x <- 2
y <- 3
z <- 4
x + y * z

# Names are case-sensitive
X + Y + Z

# Can use period
This.Year <- 2018
This.Year

# Vectors
x <- c(2, 0, 0, 4); x
assign("y", c(1, 9, 9, 9)); y
c(5, 4, 3, 2) -> z; z
q = c(1, 2, 3, 4); q

# Check the installed packages
installed.packages()
# Library location
.libPaths()
library()

# Reading from SQL Server
# Install RODBC library
# install.packages("RODBC")
# Load RODBC library
library(RODBC)
# Getting help about RODBC
help(package = "RODBC")

# Ad-hoc connection
con <- odbcDriverConnect('driver={SQL Server};server=SQL2017EIM;
                         database=AdventureWorksDW2017;uid=RUser;pwd=Pa$$w0rd')

# DSN connection
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
# Read SQL Server data
sqlQuery(con,
         "SELECT CustomerKey,
         EnglishEducation AS Education,
         Age, NumberCarsOwned, BikeBuyer
         FROM dbo.vTargetMail;")
close(con)

# Matrix
x = c(1, 2, 3, 4, 5, 6); x
Y = array(x, dim = c(2, 3)); Y
Z = matrix(x, 2, 3, byrow = F); Z
U = matrix(x, 2, 3, byrow = T); U

# Using explicit names
rnames = c("rrr1", "rrr2")
cnames = c("ccc1", "ccc2", "ccc3")
V = matrix(x, 2, 3, byrow = T,
           dimnames = list(rnames, cnames))
V

# Array
rnames = c("rrr1", "rrr2")
cnames = c("ccc1", "ccc2", "ccc3")
pnames = c("ppp1", "ppp2", "ppp3")
Y = array(1:18, dim = c(2, 3, 3),
          dimnames = list(rnames, cnames, pnames))
Y

# Factor
x = c("good", "moderate", "good", "bad", "bad", "good"); x
y = factor(x); y
z = factor(x, order = TRUE); z
w = factor(x, order = TRUE,
           levels = c("bad", "moderate", "good")); w

# List
L = list(name1 = "ABC", name2 = "DEF",
         no.children = 2, children.ages = c(3, 6))
L
L[[1]]
L[[4]]
L[[4]][2]

# Data frame
CategoryId = c(1, 2, 3, 4)
CategoryName = c("Bikes", "Components", "Clothing", "Accessories")
ProductCategories = data.frame(CategoryId, CategoryName)
ProductCategories

# Reading in a data frame from SQL Server
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TM <-
  sqlQuery(con,
           "SELECT CustomerKey,
            EnglishEducation AS Education,
            Age, NumberCarsOwned, BikeBuyer
          FROM dbo.vTargetMail;")
close(con)
head(TM, 5)
tail(TM, 5)
TM[1:5, 1:5]
TM[1:5, c("CustomerKey", "BikeBuyer")]
TM[TM$CustomerKey >= 29479, c("CustomerKey", "BikeBuyer")]

# Check the complete data frame
View(TM)

# Crosstabulation of BikeBuyer and NumberCarsOwned
table(TM$NumberCarsOwned, TM$BikeBuyer)

# Data table
# install.packages("data.table")
library(data.table)

dtTM <- data.table(TM)
class(dtTM)
# See explicit row numbers
dtTM

# View works
View(dtTM)

# Aggregation
dtTM[, mean(Age)]
# The following line does not work
TM[, mean(Age)]

# More aggregations
dtTM[, .N]   # counts
dtTM[, .(Count=.N), by = NumberCarsOwned]
dtTM[, .(AverageAge=mean(Age)), by = NumberCarsOwned]

# Filter
dtTM[NumberCarsOwned > 1, .(AverageAge=mean(Age)),
     by = NumberCarsOwned]

# Order
head(dtTM[order(-CustomerKey)], 5)

