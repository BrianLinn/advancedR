-- Advanced R module 6

-- Preparing table for NULL handling
USE AdventureWorksDW2017;
GO

DROP TABLE IF EXISTS dbo.TMTest;
DROP TABLE IF EXISTS dbo.TMTrain;
GO

-- Test set
SELECT TOP 30 PERCENT
  CustomerKey, CommuteDistance,
  TotalChildren, NumberChildrenAtHome, 
  Gender, HouseOwnerFlag,
  NumberCarsOwned, MaritalStatus,
  Age, Region,
  YearlyIncome AS Income,
  EnglishEducation AS Education,
  EnglishOccupation AS Occupation,
  BikeBuyer, 2 AS TrainTest
 INTO dbo.TMTest
FROM dbo.vTargetMail
ORDER BY CAST(CRYPT_GEN_RANDOM(4) AS INT);
-- 5546 rows

-- Training set
SELECT 
  CustomerKey, CommuteDistance,
  TotalChildren, NumberChildrenAtHome, 
  Gender, HouseOwnerFlag,
  NumberCarsOwned, MaritalStatus,
  Age, Region,
  YearlyIncome AS Income,
  EnglishEducation AS Education,
  EnglishOccupation AS Occupation,
  BikeBuyer, 1 AS TrainTest
INTO dbo.TMTrain
FROM dbo.vTargetMail AS v
WHERE NOT EXISTS
 (SELECT * FROM dbo.TMTest AS t
  WHERE v.CustomerKey = t.CustomerKey);
GO  
-- 12938 rows
