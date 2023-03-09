# # Following two packages are recommended to install before ExtractCohort
# install.packages("devtools")
# devtools::install_github("OHDSI/DatabaseConnector")
# devtools::install_github("OHDSI/SqlRender")
#
# # Install ExtractCohort
# devtools::install_github("ABMI/ExtractCohort")
# library(ExtractCohort)
# library(roxygen2)
# library(devtools)

library(lubridate)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(plotly)
library(quantmod)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(summaryBox)
library(DT)
library(ggrepel)
library(gridExtra)
library(shinyWidgets)
library(shinyalert)
library(stringr)


# 1.DB connect
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms= "sql server",
                                                                server='128.1.99.58',
                                                                user='seol',
                                                                password='Asdf1004!!',
                                                                port='1433',
                                                                pathToDriver = '/usr/lib/jvm/java-1.11.0-openjdk-amd64')
oracleTempSchema <- NULL
cdmDatabaseSchema <- "SynPUF_CDM.dbo"
cohortDatabaseSchema <- "cohortDb.dbo"

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

# 2.sample cohort table create
createCohort <- F # Create sample cohort table for test
generateCohort <- F
cohortTable <- "seol_synPuf_cohort3"
targetCohortId <- 2087
DiganosisConceptID <- '74582, 197500' # breastcancer ConceptID

# Load Cohort table
Cohort <- loadCohortTable(createCohort,
                          generateCohort,
                          connection,
                          cohortDatabaseSchema,
                          cohortTable,
                          DiganosisConceptID)

# TNM stage code
TNMcode <- loadTNMcode()

# Calculation
totalVisitTable <- visitCountDiag()
Ave_obserperiod <- F_obserperiod()
Ave_visit <- F_visit()
Ave_EmerVisit <- F_EmerVisit()
Ave_HosVisit <- F_HOSvisit()
Ave_HOSperiod <- F_HOSperiod()


# TRACER figure
TRACERflow <- "http://14rg.abmi.kr/files/code/Dashboard/ExtractCohort/breastcancer.html"


# # 3. Run APP
# ExtractCohort()

