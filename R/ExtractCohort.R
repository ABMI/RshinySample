#' ExtractCohort function
#'
#' This is a main function which can implement ExtractCohort application.
#'
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import RSQLite
#' @import lubridate
#' @import plotly
#' @import quantmod
#' @import data.table
#' @import ggrepel
#' @import gridExtra
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom shinythemes shinytheme
#'
#' @export ExtractCohort
ExtractCohort <- function(){
  filePath <- paste0(.libPaths()[1],"/ExtractCohort")
  shiny::runApp(filePath)
}
