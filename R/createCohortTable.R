#' createCohortTable function
#'
#' This is a function which can create a sample Cohort Table
#'
#'
#' @param connection
#' @param oracleTempSchema
#' @param cdmDatabaseSchema
#' @param cohortDatabaseSchema
#' @param cohortTable
#' @param cohortTable_temp
#' @param DiganosisConceptID
#' @param targetCohortId
#' @param T_createCohort
#' @param T_generateCohort
#'
#' @export createCohortTable
createCohortTable <- function(connection,
                              oracleTempSchema,
                              cohortDatabaseSchema,
                              cohortTable_temp = "temp"
                              ){
  # Create Cohort table in your DB
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "ExtractCohort",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable_temp
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

#' @export loadCohortTable
loadCohortTable <- function(createCohort,
                            generateCohort,
                            connection,
                            cohortDatabaseSchema,
                            cohortTable,
                            DiganosisConceptID
                            ){
  # Create cohort table
  if(createCohort){
    createCohortTable(connection,
                      oracleTempSchema,
                      cohortDatabaseSchema,
                      cohortTable_temp = "temp")
  }

  # Generate target cohort
  if(generateCohort){
    cohortGeneration(connection,
                     oracleTempSchema,
                     cdmDatabaseSchema,
                     cohortDatabaseSchema,
                     cohortTable,
                     cohortTable_temp = "temp",
                     targetCohortId,
                     DiganosisConceptID)
  }

  sql_loadCohort <- "SELECT * FROM @cohort_database_schema.@cohort_table"
  sql_loadCohort <- SqlRender::render(sql_loadCohort,
                                      cohort_database_schema = cohortDatabaseSchema,
                                      cohort_table = cohortTable)
  loadTable <- as.data.frame(DatabaseConnector::querySql(connection, sql_loadCohort))

  # Change the sex name
  loadTable$GENDER_SOURCE_VALUE <- gsub("F", "Female", loadTable$GENDER_SOURCE_VALUE)
  loadTable$GENDER_SOURCE_VALUE <- gsub("M", "Male", loadTable$GENDER_SOURCE_VALUE)

  # Add the age group column
  loadTable <- loadTable %>%
    mutate(ageGroup = as.numeric((DIAGNOSIS_AGE %/% 10) * 10))

  return(loadTable)
}
