#' visitCount function
#'
#' This is a function that counts the number of visit per person
#'
#' @param Cohort
#' @param cohortDatabaseSchema
#' @param cdmDatabaseSchema
#' @param cohortTable
#' @param DiganosisConceptID
#' @param connection


#' @export F_obserperiod
F_obserperiod <- function(){
  obserperiod_start <- totalVisitTable %>% distinct(SUBJECT_ID, VISIT_START_DATE, VISIT_END_DATE) %>%
    arrange(SUBJECT_ID, VISIT_START_DATE) %>% select(SUBJECT_ID, VISIT_START_DATE)
  obserperiod_end <- totalVisitTable %>% distinct(SUBJECT_ID, VISIT_START_DATE, VISIT_END_DATE) %>%
    arrange(SUBJECT_ID, desc(VISIT_END_DATE)) %>% select(SUBJECT_ID, VISIT_END_DATE)

  obserTable <- data.frame()
  l <- list()
  for (i in 1:length(unique(obserperiod_end$SUBJECT_ID))){
  obserTable <- obserperiod_end[which(unique(obserperiod_end$SUBJECT_ID)[i] == obserperiod_end$SUBJECT_ID)[1], ]
  assign(paste0('obserTable',i), obserTable)
  df.now <- get(paste0('obserTable', i))
  l[[i]] <- df.now
  rm(list = ls()[grep("obserTable", ls())])
  }
  EndDate <- do.call(rbind,l)

  obserTable <- data.frame()
  l <- list()
  for (i in 1:length(unique(obserperiod_start$SUBJECT_ID))){
    obserTable <- obserperiod_start[which(unique(obserperiod_start$SUBJECT_ID)[i] == obserperiod_start$SUBJECT_ID)[1], ]
    assign(paste0('obserTable',i), obserTable)
    df.now <- get(paste0('obserTable', i))
    l[[i]] <- df.now
    rm(list = ls()[grep("obserTable", ls())])
  }
  StartDate <- do.call(rbind,l)

  periodTable <- left_join(EndDate, StartDate, by = "SUBJECT_ID")
  periodTable <- periodTable %>%
    mutate(period = VISIT_END_DATE - VISIT_START_DATE)

  obserPeriod <- paste0(round(mean(periodTable$period), digits = 2), " (",
                        round(sd(periodTable$period), digits = 2), ")")
  return(obserPeriod)
}

#' #' @export F_Txperiod
#' F_Txperiod <- function(){
#'   #sql
#'   # Procedure_radiation, surgery, chemotherpy
#'   sql_procedure <- "with chemotherapy as (select concept_id from @cdm_database_schema.CONCEPT
#'   where concept_id in (4029715, 4273629, 4157779, 2102679, 4100042)
#' 	union select c.concept_id
#' 	from @cdm_database_schema.CONCEPT c
#' 	join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
#' 	and ca.ancestor_concept_id in (4029715, 4273629, 4157779, 2102679, 4100042)
#' 	and c.invalid_reason is null)
#'
#' 	select distinct a.subject_id, b.procedure_concept_id, b.procedure_date
#' 	from @cohort_database_schema.@cohort_table a
#' 	LEFT OUTER JOIN @cdm_database_schema.procedure_occurrence b on a.subject_id = b.person_id
#' 	and b.procedure_concept_id in (select concept_id from chemotherapy)
#' 	where b.procedure_date >= a.cohort_start_date
#' 	order by subject_id, procedure_date desc"
#'
#'   sql_procedure <- SqlRender::render(sql_procedure,
#'                                      cohort_database_schema = cohortDatabaseSchema,
#'                                      cdm_database_schema = cdmDatabaseSchema,
#'                                      cohort_table = cohortTable)
#'
#'   df_procedure <- DatabaseConnector::querySql(connection, sql_procedure)
#'
#'   # Drug
#'   sql_anticancer <- "with antineoplastic_agent as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (21601387)
#' 	union select c.concept_id
#' 	from @cdm_database_schema.CONCEPT c
#' 	join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
#' 	and ca.ancestor_concept_id in (21601387)
#' 	and c.invalid_reason is null)
#'
#' 	select distinct a.subject_id, e.drug_concept_id, e.drug_exposure_start_date
#' 	from @cohort_database_schema.@cohort_table a
#' 	LEFT OUTER JOIN @cdm_database_schema.drug_exposure e on a.subject_id = e.person_id
#' 	and e.drug_concept_id in (select concept_id from antineoplastic_agent)
#' 	where e.drug_exposure_start_date >= a.cohort_start_date
#' 	order by subject_id, drug_exposure_start_date desc"
#'
#'   sql_anticancer <- SqlRender::render(sql_anticancer,
#'                                       cohort_database_schema = cohortDatabaseSchema,
#'                                       cdm_database_schema = cdmDatabaseSchema,
#'                                       cohort_table = cohortTable)
#'
#'   df_anticancer <- DatabaseConnector::querySql(connection, sql_anticancer)
#'
#'   df_procedure <- rename(df_procedure, "DATE" = "PROCEDURE_DATE")
#'   df_anticancer <- rename(df_anticancer, "DATE" = "DRUG_EXPOSURE_START_DATE")
#'   periodTable <- rbind(df_procedure %>% select(SUBJECT_ID, DATE),
#'                            df_anticancer %>% select(SUBJECT_ID, DATE))
#'   periodTable_end <- periodTable %>% distinct(SUBJECT_ID, DATE) %>% arrange(SUBJECT_ID, desc(DATE))
#'   periodTable_end <- rename(periodTable_end, "EndDate" = "DATE")
#'   periodTable_start <- periodTable %>% distinct(SUBJECT_ID, DATE) %>% arrange(SUBJECT_ID, DATE)
#'   periodTable_start <- rename(periodTable_start, "StartDate" = "DATE")
#'
#'
#'   test <- data.frame()
#'   l <- list()
#'   for (i in 1:length(unique(periodTable_end$SUBJECT_ID))){
#'   test <- periodTable_end[which(unique(periodTable_end$SUBJECT_ID)[i] == periodTable_end$SUBJECT_ID)[1], ]
#'   assign(paste0('test',i), test)
#'   df.now <- get(paste0('test', i))
#'   l[[i]] <- df.now
#'   rm(list = ls()[grep("test", ls())])
#'   }
#'   EndDate <- do.call(rbind,l)
#'
#'
#'   test <- data.frame()
#'   l <- list()
#'   for (i in 1:length(unique(periodTable_start$SUBJECT_ID))){
#'     test <- periodTable_start[which(unique(periodTable_start$SUBJECT_ID)[i] == periodTable_start$SUBJECT_ID)[1], ]
#'     assign(paste0('test',i), test)
#'     df.now <- get(paste0('test', i))
#'     l[[i]] <- df.now
#'     rm(list = ls()[grep("test", ls())])
#'   }
#'   StartDate <- do.call(rbind,l)
#'
#'   periodTable <- left_join(EndDate, StartDate, by = "SUBJECT_ID")
#'   periodTable <- periodTable %>%
#'     mutate(period = EndDate - StartDate)
#'
#'
#'   txPeriod <- paste0(round(mean(periodTable$period), digits = 2), " (",
#'                      round(sd(periodTable$period), digits = 2), ")")
#'   return(txPeriod)
#'
#' }


#' @export visitCountDiag
visitCountDiag <- function(){
  #sql
  sql_AvgVisit <- "with CRC_concept as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (@DiganosisConceptID)
    UNION  select c.concept_id
    from  @cdm_database_schema.CONCEPT c
    join  @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (@DiganosisConceptID)
    and c.invalid_reason is null
    )
    SELECT distinct a.subject_id, m.visit_concept_id, m.visit_start_date, m.visit_end_date
    FROM @cohort_database_schema.@cohort_table a
    LEFT OUTER JOIN @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
    LEFT OUTER JOIN @cdm_database_schema.condition_occurrence b on a.subject_id = b.person_id
    and b.condition_concept_id in (select concept_id from CRC_concept)
    where m.visit_start_date > b.condition_start_date;"


  sql_AvgVisit <- SqlRender::render(sql_AvgVisit,
                                    cohort_database_schema = cohortDatabaseSchema,
                                    cdm_database_schema = cdmDatabaseSchema,
                                    cohort_table = cohortTable,
                                    DiganosisConceptID = DiganosisConceptID)

  totalVisitTable <- DatabaseConnector::querySql(connection, sql_AvgVisit)
}

#' @export F_visit
F_visit <- function(){
  visit <- vector(mode = "list", length = length(unique(totalVisitTable$SUBJECT_ID)))

  for (i in 1:length(unique(totalVisitTable$SUBJECT_ID))){
    visit[i] <- totalVisitTable %>%
      filter(SUBJECT_ID == unique(totalVisitTable$SUBJECT_ID)[i]) %>%
      n_distinct()
  }
  visit <- paste0(round(mean(unlist(visit)), digits = 2), " (",
                  round(sd(unlist(visit)), digits = 2),")")
  return(visit)
}


#' @export F_EmerVisit
F_EmerVisit <- function(){
  filterEmergency <- totalVisitTable %>%
    filter(VISIT_CONCEPT_ID == 9203| VISIT_CONCEPT_ID == 262) %>%
    distinct()

  EmerVisit <- vector(mode = "list", length = length(unique(filterEmergency$SUBJECT_ID)))

  for (i in 1:length(unique(filterEmergency$SUBJECT_ID))){
    EmerVisit[i] <- filterEmergency %>%
      filter(SUBJECT_ID == unique(filterEmergency$SUBJECT_ID)[i]) %>%
      n_distinct()
  }

  EmerVisit <- paste0(round(mean(unlist(EmerVisit)), digits = 2), " (",
                      round(sd(unlist(EmerVisit)), digits = 2), ")")
  return(EmerVisit)
}


#' @export F_HOSvisit
F_HOSvisit <- function(){
  filterHOS <- totalVisitTable %>%
    filter(VISIT_CONCEPT_ID != 9202) %>%
    mutate(Day = difftime(VISIT_END_DATE, VISIT_START_DATE, units = "days")) %>%
    filter(Day != 0) %>%
    distinct()

  HOSvisit <- vector(mode = "list", length = length(unique(filterHOS$SUBJECT_ID)))

  for (i in 1:length(unique(filterHOS$SUBJECT_ID))){
    HOSvisit[i] <- filterHOS %>%
      filter(SUBJECT_ID == unique(filterHOS$SUBJECT_ID)[i]) %>%
      n_distinct()
  }

  HOSvisit <- paste0(round(mean(unlist(HOSvisit)), digits = 2), " (",
                     round(sd(unlist(HOSvisit)), digits = 2), ")")

  return(HOSvisit)
}


#' @export F_HOSperiod
F_HOSperiod <- function(){
  filterHOS <- totalVisitTable %>%
    filter(VISIT_CONCEPT_ID != 9202) %>%
    mutate(Day = difftime(VISIT_END_DATE, VISIT_START_DATE, units = "days")) %>%
    filter(Day != 0) %>%
    distinct()

  Hosperiod <- vector(mode = "list", length = length(unique(filterHOS$SUBJECT_ID)))

  for (i in 1:length(unique(filterHOS$SUBJECT_ID))){
    selectedSubjectTable <- filterHOS %>%
      filter(SUBJECT_ID == unique(filterHOS$SUBJECT_ID)[i])
    Hosperiod[i] <- round(sum(selectedSubjectTable$Day) / nrow(selectedSubjectTable),
                          digits = 2)
  }

  Hosperiod <- paste0(round(mean(unlist(Hosperiod)), digits = 2), " (",
                      round(sd(unlist(Hosperiod)), digits = 2), ")")
  return(Hosperiod)
}
