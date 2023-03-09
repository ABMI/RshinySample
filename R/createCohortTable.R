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

  cohortsql <- "select a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date, d.year_of_birth, year(a.cohort_start_date)-d.year_of_birth as diagnosis_age, d.gender_source_value, b.condition_concept_id, c.concept_name, b.condition_start_date, e.death_date, z.value_as_concept_id, z.measurement_source_value
into @target_database_schema.@cohortTable
From @target_database_schema.@target_cohort_table a
left outer join @cdm_database_schema.condition_occurrence b on a.subject_id = b.person_id
left outer join @cdm_database_schema.person d on a.subject_id = d.person_id
left outer join @cdm_database_schema.CONCEPT c on b.condition_concept_id = c.concept_id
left outer join @cdm_database_schema.death e on a.subject_id = e.person_id
left outer join @cdm_database_schema.cancer_measurement z on a.subject_id = z.person_id;

DROP TABLE @target_database_schema.@target_cohort_table;"

  cohortsql <- SqlRender::render(cohortsql,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 target_database_schema = cohortDatabaseSchema,
                                 target_cohort_table = cohortTable_temp,
                                 cohortTable = cohortTable
                                 )
  DatabaseConnector::executeSql(connection,
                                cohortsql,
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

#' @export loadReportTable
loadReportTable <- function(){
  # load note data
  sql_note <- "SELECT distinct subject_id into #uniquePt
              from @cohort_database_schema.@cohort_table

              SELECT b.note_id, b.person_id, b.note_date, b.note_title, b.note_text
              from #uniquePt a
              left join @cdm_database_schema.note b on a.subject_id = b.person_id
              where b.note_title like 'Pathology%'

              drop table #uniquePt;"

  sql_note <- SqlRender::render(sql_note,
                                cohort_database_schema = cohortDatabaseSchema,
                                cdm_database_schema = cdmDatabaseSchema,
                                cohort_table = cohortTable)

  pathologyReport <- as.data.frame(DatabaseConnector::querySql(connection, sql_note))
  pathologyReport$NOTE_TEXT <- gsub('<.{1,10}>|&#x0d;|H&amp;|&gt;|&lt;|&amp;', ' ',
                                    pathologyReport$NOTE_TEXT,
                                    perl = T)

  # select colorectal pathology report
  crcNoteid <- vector()
  for(i in 1:nrow(pathologyReport)){
    if(!is.na(str_extract(pathologyReport$NOTE_TEXT[i],
                          "(:\\s+)(Rectum|rectal|Colon|colon|colitis|Rectosigmoid\\sjuction|rectosigmoid\\sjunctional\\sca|LST)"))){
      crcNoteid[i] <- pathologyReport$NOTE_ID[i]
    }
  }
  crcNoteid <- crcNoteid[!is.na(crcNoteid)]

  crcPtNote <- pathologyReport %>%
    filter(NOTE_ID %in% crcNoteid)

  # split report: only Biopsy report
  crcPtBioNote <- crcPtNote

  for(i in 1:nrow(crcPtBioNote)){
    if(!str_detect(crcPtBioNote$NOTE_TEXT[i],"Gross Result")){
      crcPtBioNote$NOTE_TEXT[i] <- substr(crcPtNote$NOTE_TEXT[i],
                                          str_locate(crcPtNote$NOTE_TEXT[i], "Biopsy Result")[,1],
                                          str_locate(crcPtNote$NOTE_TEXT[i], "$")[,1])
      }else if(!str_detect(crcPtBioNote$NOTE_TEXT[i],"Biopsy Result")){
        crcPtBioNote$NOTE_TEXT[i] <- "no Biopsy Result"
      }else{
      crcPtBioNote$NOTE_TEXT[i] <- substr(crcPtNote$NOTE_TEXT[i],
                                          str_locate(crcPtNote$NOTE_TEXT[i], "Biopsy Result")[,1],
                                          (str_locate(crcPtNote$NOTE_TEXT[i], "Gross Result")[,1])-1)
      }

    if(crcPtBioNote$NOTE_TEXT[i] == ""){
        crcPtBioNote$NOTE_TEXT[i] <- substr(crcPtNote$NOTE_TEXT[i],
                                            str_locate(crcPtNote$NOTE_TEXT[i], "Biopsy Result")[,1],
                                            str_locate(crcPtNote$NOTE_TEXT[i], "$")[,1])
        }
    }

  # note to table process(1)
  crcPtBioNote$NOTE_TEXT <- crcPtBioNote$NOTE_TEXT %>% str_split(":", n = 2)

  F_listToDf <- do.call(rbind.data.frame, crcPtBioNote$NOTE_TEXT)
  names(F_listToDf) = c("RESULT_TYPE","etc")

  crcPtBioNote <- cbind(crcPtBioNote, F_listToDf)
  crcPtBioNote <- subset(crcPtBioNote,
                         select = -c(NOTE_TEXT))

  # trim blank
  crcPtBioNote$etc <- gsub("^\\s+", "", crcPtBioNote$etc)

  ### Biomarker ####
  # MSI
  crcPtBioNote$MSI <- substr(crcPtBioNote$etc,
                             (str_locate(crcPtBioNote$etc,
                                         "(MSI)(.+)(yes|no)(\\s+)")[,2])+1,
                             (str_locate(crcPtBioNote$etc,
                                         "(MSI)(.+)(yes|no)(\\s+)(.+)")[,2])+3)

  crcPtBioNote$MSI <- gsub("(\n|\\s+\n|\r)(.+)", "", crcPtBioNote$MSI)
  crcPtBioNote$MSI <- gsub("MSI-H|MSI-high|MSI-High", "High", crcPtBioNote$MSI)
  crcPtBioNote$MSI <- gsub("MSI-L|MSI-low|MSI-Low", "Low", crcPtBioNote$MSI)
  crcPtBioNote$MSI <- gsub("MSS-high|MSS-High|MSS-low|MSS-Low|MSS", "Stable", crcPtBioNote$MSI)

  # # Alcain blue-PAS
  # crcPtBioNote$ABPAS <- substr(crcPtBioNote$etc,
  #                              (str_locate(crcPtBioNote$etc,
  #                                          "(\r\n|\r\n\\s+)(\\.*?)(:\\sPositive|:\\spositive)")[,1]),
  #                              (str_locate(crcPtBioNote$etc,
  #                                          "(\\.*?)(:\\sPositive|:\\spositive/gm)")[,2]))
  #
  # a <- str_detect(crcPtBioNote$etc,
  #                 regex("(Alcain\\sblue-PAS|Cytokeratin\\s7|CK7|Cytokeratin\\s20|CK20|EGFR|P53|TP53|CDX|CDX2|MLH1|MLH2|MSH2|Synaptophysin|Myeloperoxidase)(\\.*?)(:\\sPositive|:\\spositive)", dotall = T))
  #
  #
  # b <- str_detect(crcPtBioNote$etc,
  #                 "(immunohistochemistry|Immunoexpression\\sof\\stumor|immunohistochemical\\sstaining)")
  #
  # c <- str_detect(crcPtBioNote$etc,
  #                 "(\\.*?)(:\\sPositive|:\\spositive)")
  #
  # table(a)
  # table(b)
  # table(c)
  #
  # ifelse(a==c, "same", "difference")
  #
  # i <- 22
  #
  # crcPtBioNote$etc[i]
  # crcPtBioNote[i,]
  # a[i]
  # b[i]


  # Cytokeratin 7
  # CK7
  # Cytokeratin 20
  # CK20
  # EGFR
  # P53
  # TP53
  # CDX
  # CDX2
  # MLH1
  # MLH2
  # MSH2
  # Synaptophysin


  # split single and more than two or additional note
  s_crcPtBioNote <- subset(crcPtBioNote,
                           !str_detect(crcPtBioNote$etc,
                                       "^[A-Z]\\.\\s+|^[1]\\.\\s+|^Additional"))
  m_crcPtBioNote <- subset(crcPtBioNote,
                           str_detect(crcPtBioNote$etc,
                                      "^[A-Z]\\.\\s+|^[1]\\.\\s+|^Additional"))

  # single
  # remove incorrectly sorted note
  s_crcPtBioNote<- s_crcPtBioNote %>%
    filter(str_detect(s_crcPtBioNote$etc,
                      "^(Rectum|rectal|Colon|colon|colitis|Rectosigmoid\\sjuction|rectosigmoid\\sjunctional\\sca|LST)") == T)

  # note to table process(2-1)
  for(i in 1:nrow(s_crcPtBioNote)){
    s_crcPtBioNote$etc[i] <- s_crcPtBioNote$etc[i] %>%
      str_split(":\r\n\\s+|: \r\n\\s+|: \r \n\\s+", n = 2)}

  s_listToDf <- do.call(rbind.data.frame, s_crcPtBioNote$etc)
  names(s_listToDf) = c("REGION_METHOD","OTHERS")

  s_crcPtBioNote <- cbind(s_crcPtBioNote, s_listToDf)
  s_crcPtBioNote <- subset(s_crcPtBioNote,
                           select = -c(etc))

  for(i in 1:nrow(s_crcPtBioNote)){
    s_crcPtBioNote$REGION[i] <- substr(s_crcPtBioNote$REGION_METHOD[i],
                                       0,
                                       (str_locate(s_crcPtBioNote$REGION_METHOD[i],
                                                   "(,\\s+)([^,]*)(biopsy|resection|polypectomy|closure|regional|TURBT|operation|colostomy\\srepair|hemicoloectomy|colectomy|excision|curettage)")[,1])-1)

    s_crcPtBioNote$METHOD[i] <- substr(s_crcPtBioNote$REGION_METHOD[i],
                                       str_locate(s_crcPtBioNote$REGION_METHOD[i],
                                                  "(,\\s+)([^,]*)(biopsy|resection|polypectomy|closure|regional|TURBT|operation|colostomy\\srepair|hemicoloectomy|colectomy|excision|curettage)")[,1],
                                       str_locate(s_crcPtBioNote$REGION_METHOD[i], "(,\\s+)([^,]*)(biopsy|resection|polypectomy|closure|regional|TURBT|operation|colostomy\\srepair|hemicoloectomy|colectomy|excision|curettage)")[,2])
  }

  # find incorrectly sorted note
  add_mNote <- s_crcPtBioNote[which(is.na(s_crcPtBioNote$REGION)), ]
  s_crcPtBioNote <- s_crcPtBioNote[-which(is.na(s_crcPtBioNote$REGION)), ]

  # Add missing data to m_crcPtBioNote
  add_mNote <- subset(add_mNote,
                      select = -c(REGION_METHOD, OTHERS, REGION, METHOD))
  forMerge <- crcPtBioNote[c("NOTE_ID", "etc")]
  add_mNote <- left_join(add_mNote, forMerge, key = "NOTE_ID")
  m_crcPtBioNote <- rbind(m_crcPtBioNote, add_mNote)

  # extract diagnosis
  s_crcPtBioNote$DIAGNOSIS <- str_extract(s_crcPtBioNote$OTHERS,
                                          "Adenocarcinoma|Adenocarcinomas|adenocarcimnoma|Tubular\\sadenoma|tubular adenoma|Tubulovillous\\sadenoma|Carcinoma|carcinoma|adenoma|Residual\\sadenocarcinoma|Mucinous\\sadenocarcinoma|Intramucosal\\sadenocarcinoma|neuroendocrine\\stumor")
  s_crcPtBioNote$DIAGNOSIS <- gsub("carcinoma|Carcinoma","Carcinoma", s_crcPtBioNote$DIAGNOSIS)
  s_crcPtBioNote$DIAGNOSIS <- gsub("adenoma","Adenoma", s_crcPtBioNote$DIAGNOSIS)
  s_crcPtBioNote$DIAGNOSIS <- gsub("neuroendocrine tumor","Neuroendocrine tumor", s_crcPtBioNote$DIAGNOSIS)
  s_crcPtBioNote$DIAGNOSIS <- gsub("tubular Adenoma|Tubular Adenoma","Tubular adenoma", s_crcPtBioNote$DIAGNOSIS)
  s_crcPtBioNote$DIAGNOSIS <- gsub("Adenocarcinoma|Adenocarcinomas","Adenocarcinoma", s_crcPtBioNote$DIAGNOSIS)

  # trim table(remove unneed column, data)
  s_crcPtBioNote$METHOD <- gsub(",\\s+","", s_crcPtBioNote$METHOD)
  s_crcPtBioNote <- subset(s_crcPtBioNote,
                           select = -c(REGION_METHOD))
  s_crcPtBioNote <- s_crcPtBioNote %>%
    relocate("NOTE_ID","PERSON_ID","NOTE_DATE","NOTE_TITLE","RESULT_TYPE","REGION",
             "METHOD","DIAGNOSIS","MSI","OTHERS")
}
