# # Following two packages are recommended to install before ExtractCohort
# install.packages("devtools")
# devtools::install_github("OHDSI/DatabaseConnector")
# devtools::install_github("OHDSI/SqlRender")
#
# # Install ExtractCohort
# devtools::install_github("ABMI/ExtractCohort")
# library(ExtractCohort)

library(roxygen2)
library(devtools)
library(lubridate)


# 1.DB connect
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms= "sql server",
                                                                server='128.1.99.58',
                                                                user='seol',
                                                                password='Asdf1004!!',
                                                                port='1433',
                                                                pathToDriver = '/usr/lib/jvm/java-1.11.0-openjdk-amd64/')
oracleTempSchema <- NULL
cdmDatabaseSchema <- "CDMPv533_ABMI.dbo"
cohortDatabaseSchema <- "cohortDb.dbo"

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

# Temp 1_sample cohort table create
T_createCohort <- FALSE # Create sample cohort table for test
T_genorateCohort <-  FALSE
cohortTable <- "seol_CRC_0614"
targetCohortId <- 1842
DiganosisConceptID <- '197500, 74582'
nowYear <- year(Sys.Date())

# Create cohort table
if(T_createCohort){
  createCohortTable(connection,
                    oracleTempSchema,
                    cohortDatabaseSchema,
                    cohortTable_temp = "temp",
                    DiganosisConceptID)
}

# Generate target cohort
if(T_genorateCohort){
  cohortGeneration(connection,
                   oracleTempSchema,
                   cdmDatabaseSchema,
                   cohortDatabaseSchema,
                   cohortTable,
                   cohortTable_temp = "temp",
                   targetCohortId,
                   DiganosisConceptID)
}


# 2. Run APP
ExtractCohort()
