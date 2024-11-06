
USE MaxMinManufacturingDW
GO

--     TASK 2 (a) create a view
DROP VIEW IF EXISTS vInventoryByType;
GO 

CREATE VIEW vInventoryByType AS
SELECT
	PT.ProductTypeName,
	PST.ProductSubtypeName,
	i.DateOfInventory,
	i.InventoryLevel
FROM
	dbo.InventoryFact i
JOIN 
	dbo.DimProduct p ON i.ProductCode = p.Productcode
JOIN
	dbo.DimProductSubtype pst ON p.ProductSubtypeCode = pst.ProductSubtypeCode
JOIN
	dbo.DimProductType pt ON pst.ProductTypeCode = pt.ProductTypeCode;


-- Displaying the data in the view
SELECT *FROM vInventoryByType


--     TASK 3 (a)  to create stored procedure to use the view
IF OBJECT_ID('dbo.spMaxInventoryByType', 'P') IS NOT NULL
    DROP PROCEDURE [dbo].[spMaxInventoryByType];
GO

CREATE PROCEDURE spMaxInventoryByType
	@Year INT,
	@Month INT
AS
BEGIN
	SELECT
		ProductTypeNAME,
		ProductSubtypeNAME,
		MAX(InventoryLevel) AS MaxInventory
	FROM	
		vInventoryByType
	WHERE 
		YEAR(DateOfInventory) = @Year
		AND MONTH(DateOfInventory) = @Month
	GROUP BY 
		ProductTypeName,
		ProductSubtypeName
	ORDER BY
		ProductTypeName,
		ProductSubtypeName
END


-- Executing the stored procedure 
EXEC spMaxInventoryByType @Year = 2009, @Month = 4


--     TASK 2 (b) create a view
DROP VIEW IF EXISTS vAcceptedByCountry;
GO

CREATE VIEW vAcceptedByCountry AS
SELECT	DimCountry.CountryCode, 
		DimCountry.CountryName, 
		DimPlant.PlantName, 
		CONVERT(VARCHAR(10), ManufacturingFact.DateOfManufacture, 23) AS DateOfManufacture, --change format of date
		ManufacturingFact.AcceptedProducts
FROM DimCountry
JOIN DimPlant ON DimCountry.CountryCode = DimPlant.CountryCode
JOIN DimMachine ON DimPlant.PlantNumber=DimMachine.PlantNumber
JOIN ManufacturingFact ON DimMachine.MachineNumber=ManufacturingFact.MachineNumber
GO

--display the data in the view
SELECT * FROM vAcceptedByCountry


--     TASK 3 (b)  to create stored procedure that will use the view
DROP PROCEDURE IF EXISTS spAcceptedByCountry;
GO

CREATE PROCEDURE spAcceptedByCountry
	@Year INT,
	@Month INT
AS
BEGIN
	SELECT	CountryCode,
			CountryName,
			REPLACE(PlantName, 'Maximum Miniatures - ', '') AS PlantName,
			SUM(AcceptedProducts) AS TotalAcceptedProducts
	FROM vAcceptedByCountry
	WHERE 
		YEAR(DateOfManufacture) = @Year
		AND MONTH(DateOfManufacture) = @Month
	GROUP BY CountryCode, CountryName, PlantName
	ORDER BY CountryCode
END
GO


--EXEC spAcceptedByCountry 
EXEC spAcceptedByCountry @Year = 2009, @Month = 4



--     TASK 2 (c) create a view

--Drop table before creating a new view
DROP VIEW IF EXISTS vRejectedProductsByType;
GO

CREATE VIEW vRejectedProductsByType AS
SELECT 
	PT.ProductTypeName,
	PST.ProductSubtypeName, 
	ROUND((100.0*(MF.RejectedProducts) / (MF.AcceptedProducts + MF.RejectedProducts)), 7) AS PercentRejected,--round it to 7 decimal points
	(MF.AcceptedProducts + MF.RejectedProducts) AS TotalManufactured,
	CONVERT(DATE, MF.DateOfManufacture) AS DateOfManufacture -- convert the date format
FROM DimProductType AS PT
INNER JOIN DimProductSubtype AS PST
	ON PST.ProductTypeCode = PT.ProductTypeCode
INNER JOIN DimProduct AS P
	ON P.ProductSubtypeCode = PST.ProductSubtypeCode
INNER JOIN ManufacturingFact AS  MF
	ON MF.ProductCode = P.ProductCode
GO

--Display all record from the created view
SELECT * FROM vRejectedProductsByType;


--     TASK 3 (c)  to create stored procedure to use the view
--Check if stored procedure spAvgRejected exists, if exist, delete, then create
IF OBJECT_ID('dbo.spAvgRejected', 'P') IS NOT NULL
    DROP PROCEDURE [dbo].[spAvgRejected];
GO

CREATE PROCEDURE spAvgRejected
	@Year INT,
	@Month INT
AS

BEGIN
    IF @Month < 1 OR @Month > 12 OR @Year < 1900 -- only accepts valid Year and Month
    BEGIN
        PRINT 'Invalid Month or Year.' --Error message
        RETURN
    END

	SELECT 
	ProductTypeName,
	ProductSubtypeName,
	AVG(PercentRejected)  AS AvgPercentRejected,
	SUM(TotalManufactured) AS TotManufactured
	FROM vRejectedProductsByType
	WHERE
		YEAR(DateOfManufacture) = @Year AND
		MONTH(DateOfManufacture) = @Month 
	GROUP BY 
	ProductTypeName, 
	ProductSubtypeName
	ORDER BY 
	ProductTypeName
END;

--executing stored procedure spAvgRejected with parameter year and month
EXEC spAvgRejected @Year = 2009, @Month = 4;


