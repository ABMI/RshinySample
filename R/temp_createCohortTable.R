#' createCohortTable function
#'
#' This is a temp function which can create a sample Cohort Table
#'
#'
#' @param connection
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param cohortTable
#' @param cohortTable_temp
#' @param DiganosisConceptID
#' @param T_createCohort
#' @param T_genorateCohort
#'
#' @export createCohortTable
createCohortTable <- function(connection,
                              oracleTempSchema,
                              cohortDatabaseSchema,
                              cohortTable_temp = "temp",
                              DiganosisConceptID
                              ){
  # Create Cohort table in your DB
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "ExtractCohort",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable_temp,
                                           DiganosisConceptID = DiganosisConceptID
                                           )
  DatabaseConnector::executeSql(connection,
                                sql,
                                progressBar = TRUE,
                                reportOverallTime = TRUE
                                )
  }


#' @export cohortGeneration
cohortGeneration <- function(connection,
                             oracleTempSchema,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortTable,
                             cohortTable_temp = "temp",
                             targetCohortId,
                             DiganosisConceptID
){
  # Insert multiple target cohort
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GenerationCohort.sql",
                                           packageName = "ExtractCohort",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           vocabulary_database_schema = cdmDatabaseSchema,
                                           target_database_schema = cohortDatabaseSchema,
                                           target_cohort_table = cohortTable_temp,
                                           target_cohort_id = targetCohortId,
                                           cohortTable = cohortTable,
                                           DiganosisConceptID = DiganosisConceptID
                                           )
  DatabaseConnector::executeSql(connection,
                                sql,
                                progressBar = TRUE,
                                reportOverallTime = TRUE
                                )
}


