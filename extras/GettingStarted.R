# # Following two packages are recommended to install before ExtractCohort
# install.packages("devtools")
# devtools::install_github("OHDSI/DatabaseConnector")
# devtools::install_github("OHDSI/SqlRender")

.libPaths("./renv/sandbox/R-4.1/x86_64-pc-linux-gnu/6212ffa2")

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

# DatabaseConnector::downloadJdbcDrivers(dbms = 'postgresql', pathToDriver = 'pathToDriver')

# 1.DB connect
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms= "postgresql",
                                                                server='server',
                                                                user='user',
                                                                password='password',
                                                                port='port',
                                                                pathToDriver = 'pathToDriver')

oracleTempSchema <- NULL
cdmDatabaseSchema <- "synpuf_cdm.public"
cohortDatabaseSchema <- "synpuf_cdm.cohort"

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

# 2.sample cohort table create
createCohort <- T # Create sample cohort table for test
generateCohort <- T
cohortTable <- "seol_brestcancer_cohort"
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

