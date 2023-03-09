#' list of diagnosis
#'
#' @export sortDiagnosis
sortDiagnosis <- function(){
  sortTable <- count(filter(Cohort,
                            CONDITION_CONCEPT_ID != 0),
                     CONDITION_CONCEPT_ID,
                     CONCEPT_NAME,
                     sort = TRUE)
  sortTable$percentage <- round(sortTable$n / length(Cohort$CONDITION_CONCEPT_ID) * 100,
                                digits = 3)
  sortTable <- data.frame(sortTable)

  return(sortTable)
  }
