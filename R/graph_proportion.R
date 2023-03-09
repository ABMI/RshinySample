#' Drawing proportion graph
#'
#' @param Cohort
#'
#' @export sexProportionGraph
sexProportionGraph <- function(){
  tempSex <- Cohort %>% distinct(SUBJECT_ID, GENDER_SOURCE_VALUE)
  sexProportion <- plot_ly(tempSex, labels = ~GENDER_SOURCE_VALUE, type = 'pie',
                           textposition = 'inside',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF'),
                           marker = list(colors = c("#E53A40", "#30A9DE")),
                           showlegend = F)
  return(sexProportion)
}

#' @export ageProportionGraph
ageProportionGraph <- function(){
  tempAge <- Cohort %>%
    distinct(SUBJECT_ID, GENDER_SOURCE_VALUE, ageGroup) %>%
    select(GENDER_SOURCE_VALUE, ageGroup) %>%
    group_by(GENDER_SOURCE_VALUE, ageGroup) %>%
    summarise(n=n())
  tempAge <- rename(tempAge, gender = GENDER_SOURCE_VALUE)
  tempAge <- tempAge %>% mutate(percentage = round(n/sum(n) * 100, digits = 2))

  temptext <- Cohort %>% distinct(SUBJECT_ID, ageGroup) %>%
    select(ageGroup) %>%
    group_by(ageGroup) %>%
    summarise(n=n())
  temptext <- temptext %>% mutate(percentage = round(n/sum(n) * 100, digits = 2))
  temptext <- temptext %>% mutate(total = paste0(n, ' (', percentage, '%)'))


  ageProportion <- plot_ly(tempAge, x = ~ageGroup, y = ~n,
                           color = ~gender,
                           text = ~paste0('ageGroup: ', ageGroup, '\n ', n, ' (', percentage, '%)'),
                           textposition = 'inside',
                           hoverinfo = 'text',
                           colors = c("#E53A40", "#30A9DE"),
                           type = 'bar') %>%
    layout(xaxis = list(title = ''),
           yaxis = list(title = 'Count', tickformat = "digits"),
           barmode = 'stack') %>%
    add_annotations(text =  ~temptext$total,
                    textposition = 'outside',
                    x = temptext$ageGroup,
                    y = temptext$n)

  return(ageProportion)
}
#' @export TNMStageConceptID
TNMStageConceptID <- function(){
  TNMstage <- data.frame(totalT = c(45880104, 35919839, 45882493, 35919576, 35919020,
                                    45881604, 45884279, 35919454, 35919771, 35919565,
                                    35919919, 35919186, 45880976, 35919656, 35919756,
                                    45876313, 35919784, 35919265, 45880977, 35919386,
                                    35919074, 35919491, 35919018, 35919829, 35919641,
                                    35919081, 35919139, 45878379),
                         T1 = c(45880104, 35919186, 35919756, 35919386, 35919829, 45878379),
                         T2 = c(45884279, 35919565, 35919656, 35919074),
                         T3 = c(35919839, 35919771, 45876313, 35919784, 45880977,
                                35919018, 35919641, 35919081, 35919139),
                         T4 = c(45882493, 35919576, 45881604, 35919454, 35919919,
                                45880976, 35919265, 35919491),
                         totalN = c(45881615, 35919098, 35919152, 46237062, 35919928,
                                    35919423, 45882499, 45881613, 45882498, 45880982,
                                    45881614, 45881617, 35919474, 35919353, 35919626,
                                    45880111, 35919638, 35919167, 35919637, 35919125,
                                    35919844, 35919690, 35919053),
                         N0 = c(45881617, 35919626, 35919637),
                         N1 = c(45881615, 35919098, 46237062, 35919928, 45881613,
                                45880982, 35919353, 35919638, 35919690, 35919053),
                         N2 = c(35919152, 35919423, 45882498, 45881614, 35919474,
                                45880111, 35919167, 35919125),
                         N3 = c(45882499, 35919844),
                         M0 = c(35919673, 45878650),
                         M1 = c(35919762, 35919321, 35919795, 45876322, 35919223,
                                35919199, 45881618, 45882500, 35919664)
                         )
}

#' @export TGraph
TGraph <- function(){
  # T stage concept ID
  totalT <- c(45880104, 35919839, 45882493, 35919576, 35919020,
              45881604, 45884279, 35919454, 35919771, 35919565,
              35919919, 35919186, 45880976, 35919656, 35919756,
              45876313, 35919784, 35919265, 45880977, 35919386,
              35919074, 35919491, 35919018, 35919829, 35919641,
              35919081, 35919139, 45878379)
  Tis <- 35919020

  T1 <- c(45880104, 35919186, 35919756, 35919386, 35919829, 45878379)

  T2 <- c(45884279, 35919565, 35919656, 35919074)

  T3 <- c(35919839, 35919771, 45876313, 35919784, 45880977,
          35919018, 35919641, 35919081, 35919139)

  T4 <- c(45882493, 35919576, 45881604, 35919454, 35919919,
          45880976, 35919265, 35919491)

  # T stage cohort
  tempTis <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% Tis) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "Tis")

  tempT1 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% T1) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "T1")

  tempT2 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% T2) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "T2")

  tempT3 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% T3) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "T3")

  tempT4 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% T4) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "T4")

  totalTstage <- rbind(tempT1, tempT2, tempT3, tempT4)

  # T stage graph
  ProportionT <- plot_ly(totalTstage, labels = ~TNMstage, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = 'black'),
                 marker = list(line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE) %>%
    layout(title = paste("T stage", "(n =", n_distinct(totalTstage$SUBJECT_ID), ", ",
                         round(n_distinct(totalTstage$SUBJECT_ID)/n_distinct(Cohort$SUBJECT_ID)*100,
                               digits = 2), "%)"),
           colorway = c('#F6C297','#FBD591','#D3D6A7','#3B6263','#E1DE92'))
  return(ProportionT)
}

#' @export NGraph
NGraph <- function(){
  # N stage concept ID
  totalN <- c(45881615, 35919098, 35919152, 46237062, 35919928,
              35919423, 45882499, 45881613, 45882498, 45880982,
              45881614, 45881617, 35919474, 35919353, 35919626,
              45880111, 35919638, 35919167, 35919637, 35919125,
              35919844, 35919690, 35919053)

  N0 <- c(45881617, 35919626, 35919637)

  N1 <- c(45881615, 35919098, 46237062, 35919928, 45881613,
          45880982, 35919353, 35919638, 35919690, 35919053)

  N2 <- c(35919152, 35919423, 45882498, 45881614, 35919474,
          45880111, 35919167, 35919125)

  N3 <- c(45882499, 35919844)

  # N stage cohort
  tempN0 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% N0) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "N0")

  tempN1 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% N1) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "N1")

  tempN2 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% N2) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "N2")

  tempN3 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% N3) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "N3")


  totalNstage <- rbind(tempN0, tempN1, tempN2, tempN3)

  # N stage graph
  ProportionN <- plot_ly(totalNstage, labels = ~TNMstage, type = 'pie',
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = 'black'),
                         marker = list(line = list(color = '#FFFFFF', width = 1)),
                         showlegend = FALSE) %>%
    layout(title = paste("N stage", "(n =", n_distinct(totalNstage$SUBJECT_ID), ", ",
                         round(n_distinct(totalNstage$SUBJECT_ID)/n_distinct(Cohort$SUBJECT_ID)*100,
                               digits = 2), "%)"),
           colorway = c("#D7E1E9","#94C4D2","#0186BA","#00427D"))
  return(ProportionN)
}

#' @export MGraph
MGraph <- function(){
  # M stage concept ID
  M0 <- c(35919673, 45878650)

  M1 <- c(35919762, 35919321, 35919795, 45876322, 35919223,
          35919199, 45881618, 45882500, 35919664)

  # M stage cohort
  tempM0 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% M0) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "M0")

  tempM1 <- Cohort %>%
    filter(VALUE_AS_CONCEPT_ID %in% M1) %>%
    distinct(SUBJECT_ID, MEASUREMENT_SOURCE_VALUE) %>%
    mutate(TNMstage = "M1")

  totalMstage <- rbind(tempM0, tempM1)

  # M stage graph
  ProportionM <- plot_ly(totalMstage, labels = ~TNMstage, type = 'pie',
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = 'black'),
                         marker = list(line = list(color = '#FFFFFF', width = 1)),
                         showlegend = FALSE) %>%
    layout(title = paste("M stage", "(n =", n_distinct(totalMstage$SUBJECT_ID), ", ",
                         round(n_distinct(totalMstage$SUBJECT_ID)/n_distinct(Cohort$SUBJECT_ID)*100,
                               digits = 2), "%)"),
           colorway = c("#C7C6C4","#E67452"))
  return(ProportionM)
}
