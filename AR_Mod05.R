# Advanced R module 5

# Hierarchical clustering
library(RODBC)
con <- odbcConnect("AWDW", uid="RUser", pwd="Pa$$w0rd")
# Re-read the data, every 400th case only
TM <- as.data.frame(sqlQuery(con, 
                             "SELECT TotalChildren, NumberChildrenAtHome,
                                     HouseOwnerFlag, NumberCarsOwned,
                                     BikeBuyer
                                FROM dbo.vTargetMail
                                WHERE CustomerKey % 400 = 0"), stringsAsFactors = FALSE)

# Close connection
close (con)

# Check the number of clusters proposed
# install.packages("NbClust")
library(NbClust)

# Store the parameters
oldpar <- par(no.readonly = TRUE)

numCl <- NbClust(TM, distance = "euclidean",
                 min.nc = 2, max.nc = 10,
                 method = "ward.D2", index = "all")
numCl$Best.nc
numCl$Best.partition

# Restore the parameters
par(oldpar)

# create a distance matrix from the data
ds <- dist(TM, method = "euclidean") 

# Hierarchical clustering model
TMCL <- hclust(ds, method="ward.D2", members = NULL) 
# Display the dendrogram
plot(TMCL) 

# Cut tree into 3 clusters
groups <- cutree(TMCL, k=3) 

# Draw dendogram with red borders around the 6 clusters 
rect.hclust(TMCL, k=3, border="red") 


# K-Means Clustering
# Read SQL Server data
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
TM <-
  sqlQuery(con,
           "SELECT CustomerKey, CommuteDistance,
           TotalChildren, NumberChildrenAtHome, 
           Gender, HouseOwnerFlag,
           NumberCarsOwned, MaritalStatus,
           Age, BikeBuyer, Region,
           YearlyIncome AS Income,
           EnglishEducation AS Education,
           EnglishOccupation AS Occupation
           FROM dbo.vTargetMail")
close(con)
# View(TM)

# Order Education
TM$Education = factor(TM$Education, order = TRUE,
                      levels = c("Partial High School",
                                 "High School", "Partial College",
                                 "Bachelors", "Graduate Degree"))
# Create integer Education
TM$EducationInt = as.integer(TM$Education)
# View(TM)

# Do the scalable Kmeans
library(RevoScaleR)
ThreeClust <- rxKmeans(formula = ~NumberCarsOwned + Income + Age +
                         NumberChildrenAtHome + BikeBuyer + EducationInt,
                       data = TM, numClusters = 3)
summary(ThreeClust)


# Add cluster membership to the original data frame and rename the variable
TMClust <- cbind(TM, ThreeClust$cluster)
names(TMClust)[16] <- "ClusterID"
View(TMClust)

# Attach the new data frame
attach(TMClust);
# Saving parameters
oldpar <- par(no.readonly = TRUE);
# Defining a 2x2 graph
par(mfrow = c(2, 2));
# Income and clusters
boxplot(Income ~ ClusterID,
        main = "Yearly Income in Clusters",
        notch = TRUE,
        varwidth = TRUE,
        col = "orange",
        ylab = "Yearly Income",
        xlab = "Cluster Id")
# BikeBuyer and clusters
nc <- table(BikeBuyer, ClusterID)
barplot(nc,
        main = 'Bike buyer and cluster ID',
        xlab = 'Cluster Id', ylab = 'BikeBuyer',
        legend = rownames(nc),
        col = c("blue", "yellow"),
        beside = TRUE)
# Education and clusters
nc <- table(Education, ClusterID)
barplot(nc,
        main = 'Education and cluster ID',
        xlab = 'Cluster Id', ylab = 'Total Children',
        col = c("black", "blue", "green", "red", "yellow"),
        beside = TRUE)
legend("topright", rownames(nc), cex = 0.6,
       fill = c("black", "blue", "green", "red", "yellow"))
# Age and clusters
boxplot(Age ~ ClusterID,
        main = "Age in Clusters",
        notch = TRUE,
        varwidth = TRUE,
        col = "Green",
        ylab = "Yearly Income",
        xlab = "Cluster Id")
# Clean up
par(oldpar)
detach(TMClust)


# Association rules
# Install arules library only if needed
# install.packages("arules")
library(arules)

# Read the data
library(RODBC)
con <- odbcConnect("AWDW", uid = "RUser", pwd = "Pa$$w0rd")
df_AR <- as.data.frame(sqlQuery(con,
                                "SELECT OrderNumber, Model
                                FROM dbo.vAssocSeqLineItems
                                ORDER BY OrderNumber, Model;"
), stringsAsFactors = FALSE)
close(con)
View(df_AR)

# Defining transactions 
trans <- as(split(df_AR[, "Model"],
                  df_AR[, "OrderNumber"]),
            "transactions")
# Transactions info
trans
inspect(trans[6:8])

# Association rules
AR <- apriori(trans,
              parameter = list
              (minlen = 2,
                supp = 0.03,
                conf = 0.05,
                target = "rules"))
inspect(AR, ruleSep = "---->", itemSep = " + ")

# Install arulesViz library only if needed
# install.packages("arulesViz")
library(arulesViz)
# Rules graph
plot(AR, method = "graph", control = list(type = "items"))

