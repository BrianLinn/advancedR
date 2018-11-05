-- Advanced R module 2

-- Preparing table for NULL handling
USE AdventureWorksDW2017;
GO

-- Handling NULLs
DROP TABLE IF EXISTS dbo.NULLTest;
GO
CREATE TABLE dbo.NULLTest
(
 c1 INT NULL,
 c2 INT NULL,
 c3 INT NULL
);
GO

INSERT INTO dbo.NULLTest VALUES
(1, NULL, 3),
(4, 5, 6),
(NULL, 8, 9),
(10, 11, 12),
(13, NULL, NULL);
GO

-- Data
SELECT *
FROM dbo.NULLTest;
GO


-- Data preparation for pivoting
DROP TABLE IF EXISTS dbo.SalesGeoYear;
GO

SELECT g.EnglishCountryRegionName AS Country,
 g.StateProvinceName AS State,
 g.EnglishCountryRegionName + ' ' + g.StateProvinceName AS CountryState,
 d.CalendarYear AS CYear,
 SUM(s.SalesAmount) AS Sales
INTO dbo.SalesGeoYear
FROM dbo.FactInternetSales s
 INNER JOIN dbo.DimDate d
  ON d.DateKey = s.OrderDateKey
 INNER JOIN dbo.DimCustomer c
  ON c.CustomerKey = s.CustomerKey
 INNER JOIN dbo.DimGeography g
  ON g.GeographyKey = c.GeographyKey
WHERE g.EnglishCountryRegionName IN (N'Australia', N'Canada')
GROUP BY
 g.EnglishCountryRegionName,
 g.StateProvinceName,
 d.CalendarYear;
GO

SELECT TOP 5 *
FROM dbo.SalesGeoYear;
SELECT DISTINCT CYear
FROM dbo.SalesGeoYear;
GO


-- Data preparation for unpivoting
DROP TABLE IF EXISTS dbo.SalesPivoted;
GO

WITH PCTE AS
(
SELECT g.EnglishCountryRegionName AS Country,
 d.CalendarYear AS CYear,
 SUM(s.SalesAmount) AS Sales
FROM dbo.FactInternetSales s
 INNER JOIN dbo.DimDate d
  ON d.DateKey = s.OrderDateKey
 INNER JOIN dbo.DimCustomer c
  ON c.CustomerKey = s.CustomerKey
 INNER JOIN dbo.DimGeography g
  ON g.GeographyKey = c.GeographyKey
WHERE g.EnglishCountryRegionName IN (N'Australia', N'Canada')
GROUP BY
 g.EnglishCountryRegionName,
 g.StateProvinceName,
 d.CalendarYear
)
SELECT Country, [2010], [2011], [2012], [2013], [2014]
INTO dbo.SalesPivoted
FROM PCTE
 PIVOT (SUM(Sales) FOR CYear
    IN ([2010], [2011], [2012], [2013], [2014])) AS P;
GO

-- Check the data
SELECT Country, [2010], [2011], [2012], [2013], [2014]
FROM dbo.SalesPivoted;
GO


-- Preparing demo table for discretization
DROP TABLE IF EXISTS dbo.TMAge;
GO
SELECT CustomerKey, Age-10 AS Age
INTO dbo.TMAge
FROM dbo.vTargetMail;
GO

SELECT *
FROM dbo.TMAge;
GO

-- Preparing demo table for Entropy
DROP TABLE IF EXISTS dbo.TM;
GO
SELECT CustomerKey,
 NumberCarsOwned, BikeBuyer,
 Gender + MaritalStatus AS GenMar
INTO dbo.TM
FROM dbo.vTargetMail;
GO

SELECT *
FROM dbo.TM;
GO
