#' loadTNMcode function
#'
#' This is a function of loaded the TNM code
#'
#' @export loadTNMcode
loadTNMcode <- function(){
  TNMcode <- read.csv(file = '/home/seol/code/Dashboard/ExtractCohort/inst/TNMcode',
                      header = T)
}
