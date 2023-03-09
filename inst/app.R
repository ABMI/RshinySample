shinyApp(
    ui <- navbarPage(
        theme = shinythemes::shinytheme("flatly"),
        title = "Visual Dashboard",

        # Database Level
        tabPanel("Cohort level",
                 tabBox(title = tags$b(paste("Cohort (n =",n_distinct(Cohort$SUBJECT_ID), ")")),
                        width = 12,
                        # SUMMARY TAB
                        tabPanel(title = tags$b("SUMMARY"),
                                 mainPanel(width = 12,
                                           # 1st line
                                           # Proportion of Sex and age group
                                           fluidRow(column(6,
                                                           column(4,
                                                                  box(plotlyOutput("Sex"),
                                                                      title = tags$b("sex Proportion"),
                                                                      width = NULL,
                                                                      height = "100%"
                                                                      )
                                                                  ),
                                                           column(8,
                                                                  box(plotlyOutput("Age"),
                                                                      title = tags$b("age Proportion"),
                                                                      width = NULL,
                                                                      height = "100%")
                                                                  )
                                                           ),
                                                    # Proportion of TNM stage
                                                    column(6,
                                                           tags$h1("TNM Proportion"),
                                                           column(4,
                                                                  box(plotlyOutput("Ts"),
                                                                      width = NULL,
                                                                      height = "100%"
                                                                      )
                                                                  ),
                                                           column(4,
                                                                  box(plotlyOutput("Ns"),
                                                                      width = NULL,
                                                                      height = "100%"
                                                                  )
                                                           ),
                                                           column(4,
                                                                  box(plotlyOutput("Ms"),
                                                                      width = NULL,
                                                                      height = "100%"
                                                                  )
                                                           )
                                                           )
                                                    ),
                                           # 2nd line
                                           # Trends of Occurrence and Death
                                           fluidRow(column(6,
                                                           column(6,
                                                                  box(plotlyOutput("Occurrence"),
                                                                      title = tags$b("Trend of Occurrence"),
                                                                      width = "50%",
                                                                      height = "100%")
                                                           ),
                                                           column(6,
                                                                  box(plotlyOutput("Death"),
                                                                      title = tags$b("Trend of Death"),
                                                                      width = "50%",
                                                                      height = "100%")
                                                           )
                                           ),
                                           # list of diagnosis
                                           column(6,
                                                  tabBox(title = tags$b("List of most frequently received diagnosis(n, %)"),
                                                         width = "100%",
                                                         # 1st tabBox total
                                                         tabPanel(title = tags$b("Total"),
                                                                  DT::dataTableOutput("totalDiagnosis")
                                                         ),
                                                         # 2nd tabBox gender
                                                         tabPanel(title = tags$b("Gender"),
                                                                  selectInput(inputId = "chooseSex",
                                                                              label = NULL,
                                                                              choices = list("Female", "Man")
                                                                  ),
                                                                  DT::dataTableOutput("genderDiagnosis")
                                                         ),
                                                         # 3rd tabbox age group
                                                         tabPanel(title = tags$b("Age Group"),
                                                                  selectInput(inputId = "chooseAge",
                                                                              label = NULL,
                                                                              choices = names(table(Cohort$ageGroup))
                                                                  ),
                                                                  DT::dataTableOutput("ageDiagnosis")
                                                         )
                                                  )
                                           )
                                           )
                                 )
                        ),
                        # TREATMENT TAB
                        tabPanel(title = tags$b("TREATMENT"),
                                 # Average of Visit and Duration of Hospitalization
                                 fluidRow(column(2, uiOutput("AvgObserPeriod")),
                                          column(10,
                                                 column(3, uiOutput("AvgVisit")),
                                                 column(3, uiOutput("AveEmerVisit")),
                                                 column(3, uiOutput("AveHosVisit")),
                                                 column(3, uiOutput("AveHOSperiod"))
                                          )
                                 ),
                                 # Flow of Chemotherapy
                                 fluidRow(shinycssloaders::withSpinner(
                                     htmlOutput("Sankey"),
                                     type = 5)
                                 )
                        ),
                        # NOTE TAB
                        tabPanel(title = tags$b("BIOMARKER"),
                                 fluidRow(column(6,
                                                 box(plotlyOutput("MSI_b"),
                                                     title = tags$b(paste0("Microsatellite instability",
                                                                          " (n =",
                                                                          sum(!is.na(BiopsyResult$MSI)),
                                                                          ", ",
                                                                          round(sum(!is.na(BiopsyResult$MSI))/length(unique(Cohort$SUBJECT_ID))*100, digits = 2),
                                                                          "%)")),
                                                     width = "50%",
                                                     height = "100%"
                                                     )
                                                 ),
                                          column(6,
                                                 box(plotlyOutput("cancerType"),
                                                     title = tags$b("Cancer type"),
                                                     width = "50%",
                                                     height = "100%")
                                                 )
                                          ),
                                 fluidRow(column(6,
                                                 box(plotlyOutput("MSI_p"),
                                                     title = tags$b("Microsatellite instability"),
                                                     width = "50%",
                                                     height = "100%")
                                                 )
                                          )
                                 )
                 )
        ),

        # Inidividual Level
        tabPanel("Inidividual level",
                 sidebarPanel(width = 2,
                              h3(strong("Step 1: Search subject")),
                              fluidRow(box(width = 12,
                                           h4(strong("Enter the Subject ID")),
                                           textInput("searchSubject",
                                                     label = NULL,
                                                     width = "100%"),
                                           actionButton("subjectid", "Search")
                                           ),
                                       box(width = 12,
                                           h3(strong("Step 2: Search period")),
                                           h5("You can select the dates from below."),
                                           h5("Jan. 1, 1994 to Jul. 31, 2021.")),

                                       box(width = 12,
                                           h4(strong("1) Lab Date")),
                                           dateInput("Labdate",
                                                     label = "Search Date: yyyy-mm-dd",
                                                     min = "1994-01-01",
                                                     max = "2021-07-31",
                                                     format = "yyyy-mm-dd"
                                                     ),
                                           actionButton("labDateSearch", "Search")
                                           ),
                                       box(width = 12,
                                           h4(strong("2) Drug period")),
                                           dateRangeInput("Drugdate",
                                                          label = "Period: yyyy-mm-dd to yyyy-mm-dd",
                                                          min = "1994-01-01",
                                                          max = "2021-07-31",
                                                          format = "yyyy-mm-dd"
                                                          ),
                                           actionButton("drugPeriodSearch", "Search")
                                           )
                                       )
                              ),

                 mainPanel(width = 10,
                           tabBox(width = "100%",
                                  # Subject Information
                                  tabPanel("Info",
                                           box(title = "Total Medical Schedule",
                                               tags$h5("-Enter the subject id in the left sidebar."),
                                               plotlyOutput("summary"),
                                               width = "100%")
                                           ),

                                  # Subject Lab results
                                  tabPanel("Lab",
                                           fluidRow(column(6,
                                                           box(title = tags$b("Lab Result"),
                                                               tags$h5("-Select the wanted lab date in the left sidebar."),
                                                               width = "100%",
                                                               height = NULL,
                                                               DT::dataTableOutput("Lablist")
                                                               )
                                                           ),
                                                    column(6,
                                                           box(title = tags$b("Trend of Results"),
                                                               tags$h5("-Enter the concept id."),
                                                               sidebarSearchForm(textId = "Labcode",
                                                                                 label = "Search test code",
                                                                                 buttonId = "CodeSearchButton"
                                                                                 ),
                                                               plotlyOutput("Labgraph"),
                                                               width = "100%",
                                                               height = NULL)
                                                           )
                                                    )
                                           ),

                                  # Subject Drug
                                  tabPanel("Drug",
                                           fluidRow(box(title = tags$b("Drug Graph"),
                                                        tags$h5("-This is the overall medication graph for the patient."),
                                                        plotlyOutput("DrugGraph"),
                                                        width = "100%",
                                                        height = NULL
                                                        )
                                                    ),

                                           fluidRow(box(title = tags$b("Drug List"),
                                                        tags$h5("-Select the wanted period in the left sidebar."),
                                                        width = "100%",
                                                        height = NULL,
                                                        DT::dataTableOutput("Druglist")
                                                        )
                                                    )
                                           )
                                  )
                           )
                 ),

        # Filter cohort
        tabPanel("Cohort Generation",
                 sidebarPanel(width = 3,
                              h3(strong("Step 1: Select Options")),
                              fluidRow(box(width = 12,
                                           h4(strong("1) Sex")),
                                           checkboxGroupInput("selSex",
                                                              label = NULL,
                                                              choices = names(table(Cohort$GENDER_SOURCE_VALUE)),
                                                              selected = names(table(Cohort$GENDER_SOURCE_VALUE))
                                                              )
                                           )
                                       ),

                              fluidRow(box(width = 12,
                                           h4(strong("2) Age")),
                                           sliderInput('ageVar','Range of age:',
                                                       min = min(Cohort$DIAGNOSIS_AGE, na.rm = TRUE),
                                                       max = max(Cohort$DIAGNOSIS_AGE, na.rm = TRUE),
                                                       value = c(min(Cohort$DIAGNOSIS_AGE, na.rm = TRUE),
                                                                 max(Cohort$DIAGNOSIS_AGE, na.rm = TRUE)
                                                                 )
                                                       )
                                           )
                                       ),

                              fluidRow(box(width = 12,
                                           h4(strong("3) TNM stage")),
                                           h5("You can select mulitple."),
                                           column(4,
                                                  selectInput("selT",
                                                              label = "select T stage",
                                                              choices = (TNMcode %>% filter(sort == "Tstage"))[1],
                                                              multiple = TRUE,
                                                              selectize = TRUE
                                                              )
                                                  ),
                                           column(4,
                                                  selectInput("selN",
                                                              label = "select N stage",
                                                              choices = (TNMcode %>% filter(sort == "Nstage"))[1],
                                                              multiple = TRUE,
                                                              selectize = TRUE
                                                              )
                                                  ),
                                           column(4,
                                                  selectInput("selM",
                                                              label = "select M stage",
                                                              choices = (TNMcode %>% filter(sort == "Mstage"))[1],
                                                              multiple = TRUE,
                                                              selectize = TRUE
                                                              )
                                                  )
                                           )
                                       ),
                              fluidRow(actionButton("Preview", "View", width = "100%")
                                       ),

                              h3(strong("Step 2: Create a cohort table")),
                              helpText("Enter Table Name (ex. kim_cohort)"),
                              fluidRow(box(width = 9,
                                           textInput("TableName",
                                                     label = NULL,
                                                     width = "100%")
                                           ),
                                       actionButton("ExtractData", "Generation")
                                       )
                              ),


                 mainPanel(width = 9,
                           h3(strong("Pre-View")),
                           fluidRow(
                               shinycssloaders::withSpinner(
                                   dataTableOutput('preview'),
                                   type = 5
                                   )
                               )
                           )
                 )
        )


    # Define server logic required to draw a histogram
    , server <- function(input, output, session) {
        ### reactive
        ## Cohort Level
        # Diagnosis list
        rc_sex <- reactive({
            filterSex <- filter(Cohort,
                                GENDER_SOURCE_VALUE == input$chooseSex)
            sortDiagnosis <- count(filter(filterSex,
                                          CONDITION_CONCEPT_ID != 0),
                                   CONDITION_CONCEPT_ID,
                                   CONCEPT_NAME,
                                   sort = TRUE)
            sortDiagnosis$percentage <- round(sortDiagnosis$n / length(Cohort$CONDITION_CONCEPT_ID) * 100,
                                              digits = 3)
            sortDiagnosis <- data.frame(sortDiagnosis)
        })
        rc_age <- reactive({
            filterAge <- filter(Cohort,
                                ageGroup == input$chooseAge)
            sortAgeDiagnosis <- count(filter(filterAge,
                                             CONDITION_CONCEPT_ID != 0),
                                      CONDITION_CONCEPT_ID,
                                      CONCEPT_NAME,
                                      sort = TRUE)
            sortAgeDiagnosis$percentage <- round(sortAgeDiagnosis$n / length(filterAge$CONDITION_CONCEPT_ID) * 100,
                                                 digits = 3)
            sortAgeDiagnosis <- data.frame(sortAgeDiagnosis)
        })

        ## Individual Level
        # summary
        rc_smPlotly <- reactive({

                subjectId <- as.numeric(input$searchSubject)
                # Make summary data table
                # Visit summary
                sql_visit <- "SELECT distinct a.subject_id, m.visit_concept_id, z.concept_name, m.visit_start_date, m.visit_end_date
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
                left outer join @cdm_database_schema.CONCEPT z on m.visit_concept_id = z.concept_id
                where a.subject_id = @subjectID;"

                sql_visit <- SqlRender::render(sql_visit,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               cohort_table = cohortTable,
                                               subjectID = subjectId)

                df_visit <- as.data.frame(DatabaseConnector::querySql(connection, sql_visit))

                sm_visit <- df_visit %>%
                    select(CONCEPT_NAME, VISIT_START_DATE, VISIT_END_DATE) %>%
                    arrange(VISIT_START_DATE) %>%
                    mutate(type = "Visit")

                for (i in 1:nrow(sm_visit)){
                    sm_visit$tag[i] <- paste(sm_visit$CONCEPT_NAME[i],
                                             paste(sm_visit$VISIT_START_DATE[i],
                                                   sm_visit$VISIT_END_DATE[i],
                                                   sep = " ~ "),
                                             sep = ", ")
                }

                sm_visit <- subset(sm_visit, select = -VISIT_END_DATE)

                setnames(sm_visit,
                         old = c("VISIT_START_DATE"),
                         new = c("Dates")
                )

                # Condition summary
                sql_condition <- "SELECT distinct a.subject_id, d.condition_concept_id, z.concept_name, d.condition_start_date, d.condition_end_date
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.condition_occurrence d on a.subject_id = d.person_id
                left outer join @cdm_database_schema.CONCEPT z on d.condition_concept_id = z.concept_id
                where a.subject_id = @subjectID;"

                sql_condition <- SqlRender::render(sql_condition,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   subjectID = subjectId)

                df_condition <- as.data.frame(DatabaseConnector::querySql(connection, sql_condition))

                sm_condition <- df_condition %>%
                    distinct(CONCEPT_NAME, CONDITION_START_DATE, .keep_all = TRUE) %>%
                    select(CONCEPT_NAME, CONDITION_START_DATE, CONDITION_END_DATE) %>%
                    arrange(CONDITION_START_DATE) %>%
                    mutate(type = "Condition")

                t <- sm_condition %>%
                    distinct(CONDITION_START_DATE)

                for (i in 1:nrow(t)){
                    z <- sm_condition %>%
                        filter(CONDITION_START_DATE == t$CONDITION_START_DATE[i]) %>%
                        select(CONCEPT_NAME)

                    y <- as.list(z$CONCEPT_NAME)

                    if(length(y) <= 5){
                        x <- paste(y[1:length(y)], collapse = ", ")
                    }else{
                        x <- paste(y[1:5], collapse = ", ")
                        x <- paste(x, ", lots of condition list omitted..")
                    }

                    sm_condition$CONCEPT_NAME[i] <-  x
                }


                for (i in 1:nrow(sm_condition)){
                    sm_condition$tag[i] <- paste(paste(sm_condition$CONDITION_START_DATE[i],
                                                       sm_condition$CONDITION_END_DATE[i],
                                                       sep = " ~ "),
                                                 sm_condition$CONCEPT_NAME[i],
                                                 sep = ", ")
                }

                sm_condition <- subset(sm_condition, select = -CONDITION_END_DATE)

                setnames(sm_condition,
                         old = c("CONDITION_START_DATE"),
                         new = c("Dates")
                )

                # measurement summary
                sql_lab <- "SELECT distinct a.subject_id, i.measurement_concept_id, z.concept_name, i.measurement_date
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.measurement i on a.subject_id = i.person_id
    left outer join @cdm_database_schema.CONCEPT z on i.measurement_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

                sql_lab <- SqlRender::render(sql_lab,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_table = cohortTable,
                                             subjectID = subjectId)

                df_measurement <- as.data.frame(DatabaseConnector::querySql(connection, sql_lab))

                sm_measurement <- df_measurement %>%
                    select(CONCEPT_NAME, MEASUREMENT_DATE) %>%
                    arrange(MEASUREMENT_DATE) %>%
                    mutate(type = "Lab")

                t <- sm_measurement %>%
                    distinct(MEASUREMENT_DATE)

                for (i in 1:nrow(t)){
                    z <- sm_measurement %>%
                        filter(MEASUREMENT_DATE == t$MEASUREMENT_DATE[i]) %>%
                        select(CONCEPT_NAME)

                    y <- as.list(z$CONCEPT_NAME)

                    if(length(y) <= 5){
                        x <- paste(y[1:length(y)], collapse = ", ")
                    }else{
                        x <- paste(y[1:5], collapse = ", ")
                        x <- paste(x, ", lots of lab list omitted..")
                    }

                    sm_measurement$CONCEPT_NAME[i] <-  x
                }

                sm_measurement <- sm_measurement %>%
                    distinct(MEASUREMENT_DATE, .keep_all = TRUE)

                sm_measurement$tag <- sm_measurement$CONCEPT_NAME

                setnames(sm_measurement,
                         old = "MEASUREMENT_DATE",
                         new = "Dates"
                )

                # Observation summary
                sql_observation <- "SELECT	distinct a.subject_id, j.observation_concept_id, z.concept_name, j.observation_date
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.observation j on a.subject_id = j.person_id
    left outer join @cdm_database_schema.CONCEPT z on j.observation_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

                sql_observation <- SqlRender::render(sql_observation,
                                                     cohort_database_schema = cohortDatabaseSchema,
                                                     cdm_database_schema = cdmDatabaseSchema,
                                                     cohort_table = cohortTable,
                                                     subjectID = subjectId)

                df_observation <- as.data.frame(DatabaseConnector::querySql(connection, sql_observation))

                sm_observation <- df_observation %>%
                    select(CONCEPT_NAME, OBSERVATION_DATE) %>%
                    arrange(OBSERVATION_DATE) %>%
                    mutate(type = "Observation")

                t <- sm_observation %>%
                    distinct(OBSERVATION_DATE)

                for (i in 1:nrow(t)){
                    z <- sm_observation %>%
                        filter(OBSERVATION_DATE == t$OBSERVATION_DATE[i]) %>%
                        select(CONCEPT_NAME)

                    y <- as.list(z$CONCEPT_NAME)

                    if(length(y) <= 5){
                        x <- paste(y[1:length(y)], collapse = ", ")
                    }else{
                        x <- paste(y[1:5], collapse = ", ")
                        x <- paste(x, ", lots of observation list omitted..")
                        x <- paste(y, collapse = ", ")
                    }

                    sm_observation$CONCEPT_NAME[i] <-  x
                }

                sm_observation <- sm_observation %>%
                    distinct(OBSERVATION_DATE, .keep_all = TRUE)

                sm_observation$tag <- sm_observation$CONCEPT_NAME

                setnames(sm_observation,
                         old = "OBSERVATION_DATE",
                         new = "Dates"
                )

                # TNM summary
                sql_TNM <- "SELECT distinct a.subject_id, c.measurement_date, c.measurement_source_value
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.cancer_measurement c on a.subject_id = c.person_id
    where a.subject_id = @subjectID;"

                sql_TNM <- SqlRender::render(sql_TNM,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_table = cohortTable,
                                             subjectID = subjectId)

                df_TNM <- as.data.frame(DatabaseConnector::querySql(connection, sql_TNM))

                sm_TNM <- df_TNM %>%
                    select(MEASUREMENT_DATE, MEASUREMENT_SOURCE_VALUE) %>%
                    arrange(MEASUREMENT_SOURCE_VALUE) %>%
                    mutate(type = "TNM")

                sm_TNM$tag <- sm_TNM$MEASUREMENT_SOURCE_VALUE

                setnames(sm_TNM,
                         old = c("MEASUREMENT_SOURCE_VALUE",
                                 "MEASUREMENT_DATE"),
                         new = c("CONCEPT_NAME",
                                 "Dates")
                )

                # binding summaries
                row <- data.frame(
                    CONCEPT_NAME = c(NA, NA, NA, NA, NA),
                    Dates = c(NA, NA, NA, NA, NA),
                    type = c("Visit", "Condition", "Lab", "Observation", "TNM"),
                    tag = c(NA, NA, NA, NA, NA),
                    stringsAsFactors = FALSE
                )

                bindsm <- rbind(sm_visit, sm_condition, sm_measurement, sm_observation, sm_TNM, row)



                # Graph
                fig <- plot_ly(bindsm,
                               x = ~Dates,
                               y = ~type,
                               color = ~type,
                               text = ~tag,
                               type = "scatter",
                               mode = "markers",
                               marker = list(size = 20)) %>%
                    layout(xaxis = list(
                        rangeselector = list(
                            buttons = list(
                                list(
                                    count = 3,
                                    label = "3 mo",
                                    step = "month",
                                    stepmode = "backward"),
                                list(
                                    count = 6,
                                    label = "6 mo",
                                    step = "month",
                                    stepmode = "backward"),
                                list(
                                    count = 1,
                                    label = "1 yr",
                                    step = "year",
                                    stepmode = "backward"),
                                list(step = "all"))),
                        rangeslider = list(type = "date")))

            })

        # LAB
        rc_labList <- reactive({
            subjectId <- as.numeric(input$searchSubject)
            wantdate <- as.Date(input$Labdate)

            sql_lab <- "SELECT distinct a.subject_id, i.measurement_concept_id, z.concept_name, i.measurement_date, i.value_as_number, i.value_as_concept_id, i.unit_concept_id, i.range_low, i.range_high, i.unit_source_value, i.value_source_value
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.measurement i on a.subject_id = i.person_id
    left outer join @cdm_database_schema.CONCEPT z on i.measurement_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

            sql_lab <- SqlRender::render(sql_lab,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         cohort_table = cohortTable,
                                         subjectID = subjectId)

            df_measurement <- as.data.frame(DatabaseConnector::querySql(connection, sql_lab))

            FureTestLab <- df_measurement %>%
                filter(MEASUREMENT_DATE == wantdate) %>%
                distinct(MEASUREMENT_CONCEPT_ID,
                         VALUE_AS_NUMBER,
                         VALUE_AS_CONCEPT_ID,
                         .keep_all = TRUE) %>%
                filter(MEASUREMENT_CONCEPT_ID != 0)

            FureTestLab$Range <- paste(paste(FureTestLab$RANGE_LOW,
                                             FureTestLab$RANGE_HIGH,
                                             sep = " ~ "),
                                       FureTestLab$UNIT_SOURCE_VALUE,
                                       sep = " ")
            FureTestLab$VALUE_AS_NUMBER <- ifelse(is.na(FureTestLab$VALUE_AS_NUMBER),
                                                  FureTestLab$VALUE_SOURCE_VALUE,
                                                  FureTestLab$VALUE_AS_NUMBER)

            filteredLabDate <- as.data.frame(FureTestLab %>%
                                                 select(MEASUREMENT_CONCEPT_ID,
                                                        CONCEPT_NAME,
                                                        VALUE_AS_NUMBER,
                                                        Range)
            )
            setnames(filteredLabDate,
                     old = c("MEASUREMENT_CONCEPT_ID", "VALUE_AS_NUMBER"),
                     new = c("CONCEPT_ID", "Result")
            )

            as.data.frame(filteredLabDate)

        })
        rc_labGraph <- reactive({
            subjectId <- as.numeric(input$searchSubject)
            wantLAb <- as.numeric(input$Labcode)

            sql_lab <- "SELECT distinct a.subject_id, i.measurement_concept_id, z.concept_name, i.measurement_date, i.value_as_number, i.value_as_concept_id, i.unit_concept_id, i.range_low, i.range_high, i.unit_source_value, i.value_source_value
    FROM @cohort_database_schema.@cohort_table a
    inner join @cdm_database_schema.measurement i on a.subject_id = i.person_id and a.subject_id = @subjectID
    inner join @cdm_database_schema.CONCEPT z on i.measurement_concept_id = z.concept_id and i.measurement_concept_id = @wantLAb
    where i.measurement_date between '1994-01-01' and '2021-07-31';"

            sql_lab <- SqlRender::render(sql_lab,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         cohort_table = cohortTable,
                                         subjectID = subjectId,
                                         wantLAb = wantLAb)

            df_measurement <- as.data.frame(DatabaseConnector::querySql(connection, sql_lab))

            FureTestLab <- df_measurement %>%
                arrange(MEASUREMENT_DATE) %>%
                distinct(MEASUREMENT_CONCEPT_ID,
                         MEASUREMENT_DATE,
                         VALUE_AS_NUMBER,
                         VALUE_AS_CONCEPT_ID,
                         .keep_all = TRUE)

            FureTestLab$Range <- paste("Normal Range",
                                       paste(paste(FureTestLab$RANGE_LOW,
                                                   FureTestLab$RANGE_HIGH,
                                                   sep = " ~ "),
                                             FureTestLab$UNIT_SOURCE_VALUE,
                                             sep = " ")
            )

            FureTestLab$VALUE_AS_NUMBER <- ifelse(is.na(FureTestLab$VALUE_AS_NUMBER),
                                                  FureTestLab$VALUE_SOURCE_VALUE,
                                                  FureTestLab$VALUE_AS_NUMBER)
            reArrange <- FureTestLab %>%
                distinct(MEASUREMENT_DATE, .keep_all = TRUE) %>%
                arrange(MEASUREMENT_DATE)

            setnames(reArrange,
                     old = c("MEASUREMENT_DATE", "VALUE_AS_NUMBER"),
                     new = c("Date", "Result"))

            GraphLab <- plot_ly(reArrange,
                                x = ~Date,
                                y = ~Result,
                                text = ~Range,
                                color = ~Result,
                                type = "scatter",
                                mode = "lines",
                                marker = list(size = 20)) %>%
                layout(xaxis = list(
                    rangeselector = list(
                        buttons = list(
                            list(
                                count = 3,
                                label = "3 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 6,
                                label = "6 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 1,
                                label = "1 yr",
                                step = "year",
                                stepmode = "backward"),
                            list(step = "all"))),
                    rangeslider = list(type = "date")))

            return(GraphLab)
        })

        # Drug
        rc_druglist <- reactive({
            subjectId <- as.numeric(input$searchSubject)

            sql_drug <- "SELECT distinct a.subject_id, f.drug_concept_id, z.concept_name, f.drug_era_start_date, f.drug_era_end_date
    FROM @cohort_database_schema.@cohort_table a
    inner join @cdm_database_schema.drug_era f on a.subject_id = f.person_id
    inner join @cdm_database_schema.CONCEPT z on f.drug_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

            sql_drug <- SqlRender::render(sql_drug,
                                          cohort_database_schema = cohortDatabaseSchema,
                                          cdm_database_schema = cdmDatabaseSchema,
                                          cohort_table = cohortTable,
                                          subjectID = subjectId)

            df_drug <- as.data.frame(DatabaseConnector::querySql(connection, sql_drug))

            filterSubject <- df_drug %>%
                filter(DRUG_ERA_START_DATE >= input$Drugdate[1] & DRUG_ERA_END_DATE <= input$Drugdate[2]) %>%
                ungroup() %>%
                select(DRUG_CONCEPT_ID,
                       CONCEPT_NAME,
                       DRUG_ERA_START_DATE,
                       DRUG_ERA_END_DATE)

            setnames(filterSubject,
                     old = c("DRUG_CONCEPT_ID",
                             "CONCEPT_NAME",
                             "DRUG_ERA_START_DATE",
                             "DRUG_ERA_END_DATE"),
                     new = c("CONCEPT_ID",
                             "DrugName",
                             "StartDate",
                             "EndDate")
            )
            as.data.frame(filterSubject)

        })
        rc_drugGraph <- reactive({
            subjectId <- as.numeric(input$searchSubject)

            sql_drug <- "SELECT distinct a.subject_id, f.drug_concept_id, z.concept_name, f.drug_era_start_date, f.drug_era_end_date
    FROM @cohort_database_schema.@cohort_table a
    inner join @cdm_database_schema.drug_era f on a.subject_id = f.person_id
    inner join @cdm_database_schema.CONCEPT z on f.drug_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

            sql_drug <- SqlRender::render(sql_drug,
                                          cohort_database_schema = cohortDatabaseSchema,
                                          cdm_database_schema = cdmDatabaseSchema,
                                          cohort_table = cohortTable,
                                          subjectID = subjectId)

            df_drug <- as.data.frame(DatabaseConnector::querySql(connection, sql_drug))

            df_drug$duration <- paste(df_drug$DRUG_ERA_START_DATE,
                                      df_drug$DRUG_ERA_END_DATE,
                                      sep = " ~ ")

            fig_drug <- plot_ly(df_drug,
                                x = ~DRUG_ERA_START_DATE,
                                y = ~CONCEPT_NAME,
                                type = "scatter",
                                color = ~CONCEPT_NAME,
                                text = ~duration,
                                mode = "markers",
                                marker = list(size = 20)
            ) %>%
                layout(xaxis = list(
                    rangeselector = list(
                        buttons = list(
                            list(
                                count = 3,
                                label = "3 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 6,
                                label = "6 mo",
                                step = "month",
                                stepmode = "backward"),
                            list(
                                count = 1,
                                label = "1 yr",
                                step = "year",
                                stepmode = "backward"),
                            list(step = "all")
                        )
                    ),
                    rangeslider = list(type = "date"),
                    title = "Date"
                )
                )
        })

        ## Download
        # subject_id, cohort_start_date, cohort_end_date, GENDER_SOURCE_VALUE
        sel_data <- reactive({
            # select options
            # 1. sex
            filter_sex <- input$selSex
            ifelse(filter_sex == 'Female',
                   filter_sex <- '\'F\'',
                   ifelse(filter_sex == 'Male',
                          filter_sex <- '\'M\'',
                          filter_sex <- '\'F\', \'M\''))

            # 2. age
            age_start <- (input$ageVar)[1]
            age_end <- (input$ageVar)[2]

            # 3. TNM
            ## T stage
            if(is.null(input$selT)){
                filter_T <- 0
            }
            else{
                select_T <- (TNMcode %>% filter(TNMstage %in% input$selT))[2]
                if(str_detect(select_T, 'c')){
                    filter_T <- gsub('(c|\\(|\\)|\")', "", select_T)
                    }else{
                        filter_T <- paste(select_T)
                    }
                }

            ## N stage
            if(is.null(input$selN)){
                filter_N <- 0
            }
            else{
                select_N <- (TNMcode %>% filter(TNMstage %in% input$selN))[2]
                if(str_detect(select_N, 'c')){
                    filter_N <- gsub('c|\\(|\\)|\"', "", select_N)
                    }else{
                        filter_N <- paste(select_N)
                    }
                }

            ## M stage
            if(is.null(input$selM)){
                filter_M <- 0
            }
            else{
                select_M <- (TNMcode %>% filter(TNMstage %in% input$selM))[2]
                if(str_detect(select_M, 'c')){
                    filter_M <- gsub('c|\\(|\\)|\"', "", select_M)
                    }else{
                        filter_M <- paste(select_M)
                    }
                }

            # SQL Render
            if(filter_T != 0 && filter_N != 0 && filter_M != 0){
                sql_subject <-
                "SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                INTO #step1
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where a.cohort_definition_id in (@targetCohortId)
                    and a.gender_source_value in (@sex)
                    and a.diagnosis_age between (@age_start) and (@age_end)
                    and b.value_as_concept_id in (@Tstage)

                SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                INTO #step2
                FROM #step1 a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where b.value_as_concept_id in (@Nstage)

                SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                FROM #step2 a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where b.value_as_concept_id in (@Mstage)

                DROP TABLE #step1, #step2;"

                sql_subject <- SqlRender::render(sql_subject,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_table = cohortTable,
                                                 targetCohortId = targetCohortId,
                                                 sex = filter_sex,
                                                 age_start = age_start,
                                                 age_end = age_end,
                                                 Tstage = filter_T,
                                                 Nstage = filter_N,
                                                 Mstage = filter_M)


            }else if(filter_N == 0 && filter_M ==0){
                sql_subject <-
                "SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where a.cohort_definition_id in (@targetCohortId)
                    and a.gender_source_value in (@sex)
                    and a.diagnosis_age between (@age_start) and (@age_end)
                    and b.value_as_concept_id in (@Tstage)
                    and b.measurement_source_value not like '%N%'
                    and b.measurement_source_value not like '%M%';"

                sql_subject <- SqlRender::render(sql_subject,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_table = cohortTable,
                                                 targetCohortId = targetCohortId,
                                                 sex = filter_sex,
                                                 age_start = age_start,
                                                 age_end = age_end,
                                                 Tstage = filter_T)
            }else if(filter_N == 0){
                sql_subject <-
                    "SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                INTO #step1
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where a.cohort_definition_id in (@targetCohortId)
                    and a.gender_source_value in (@sex)
                    and a.diagnosis_age between (@age_start) and (@age_end)
                    and b.value_as_concept_id in (@Tstage)

                SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                FROM #step1 a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where b.value_as_concept_id in (@Mstage)
                and b.measurement_source_value not like '%N%'

                DROP TABLE #step1;"

                sql_subject <- SqlRender::render(sql_subject,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_table = cohortTable,
                                                 targetCohortId = targetCohortId,
                                                 sex = filter_sex,
                                                 age_start = age_start,
                                                 age_end = age_end,
                                                 Tstage = filter_T,
                                                 Mstage = filter_M)
            }else{
                sql_subject <-
                    "SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                INTO #step1
                FROM @cohort_database_schema.@cohort_table a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where a.cohort_definition_id in (@targetCohortId)
                    and a.gender_source_value in (@sex)
                    and a.diagnosis_age between (@age_start) and (@age_end)
                    and b.value_as_concept_id in (@Tstage)

                SELECT distinct a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date,
                b.measurement_source_value, b.value_source_value
                FROM #step1 a
                left outer join @cdm_database_schema.cancer_measurement b on a.subject_id = b.person_id
                where b.value_as_concept_id in (@Nstage)
                and b.measurement_source_value not like '%M%'

                DROP TABLE #step1;"

                sql_subject <- SqlRender::render(sql_subject,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_table = cohortTable,
                                                 targetCohortId = targetCohortId,
                                                 sex = filter_sex,
                                                 age_start = age_start,
                                                 age_end = age_end,
                                                 Tstage = filter_T,
                                                 Nstage = filter_N)
            }

            df_subject <- as.data.frame(DatabaseConnector::querySql(connection, sql_subject))
            sel_cohort <- df_subject %>%
                select(COHORT_DEFINITION_ID,
                       SUBJECT_ID,
                       COHORT_START_DATE,
                       COHORT_END_DATE,
                       MEASUREMENT_SOURCE_VALUE,
                       VALUE_SOURCE_VALUE) %>%
                distinct(COHORT_DEFINITION_ID,
                         SUBJECT_ID,
                         COHORT_START_DATE,
                         COHORT_END_DATE,
                         MEASUREMENT_SOURCE_VALUE,
                         VALUE_SOURCE_VALUE) %>%
                arrange(SUBJECT_ID)
            sel_cohort <- rename(sel_cohort, TNMstage = MEASUREMENT_SOURCE_VALUE, OrganCode = VALUE_SOURCE_VALUE)
            })

        ### render
        # Database Level
        # Proportion
        output$Sex <- renderPlotly(sexProportionGraph())
        output$Age <- renderPlotly(ageProportionGraph())

        # Trends
        output$Occurrence <- renderPlotly({
            # Deduplication and limit year
            Deduplication <- Cohort %>%
                distinct(SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE, .keep_all = TRUE)

            # Frequency table
            yearTable <- sort(unique(year(Deduplication$COHORT_START_DATE)))
            incidence <- data.frame()

            for (i in 1:length(yearTable)){
                year <- yearTable[i]

                yearcohort <- Deduplication %>%
                    filter(year(COHORT_START_DATE) == yearTable[i])
                cohortN <- nrow(yearcohort)

                totalN <- (nrow(Deduplication)*length(yearTable))

                incidence <- as.data.frame(rbind(incidence, c(year, cohortN, totalN)))

            }
            colnames(incidence) <- c("year", "cohortN", "totalN")


            # Graph
            incidenceRates <- plot_ly(incidence,
                                      x = ~year,
                                      y = ~cohortN,
                                      type = 'scatter',
                                      mode = 'lines',
                                      text = ~paste0('year: ', year, '\n', cohortN),
                                      textposition = 'middle right',
                                      hoverinfo = 'text') %>%
                layout(xaxis = list(title = 'Year'),
                       yaxis = list(title = 'Count'),
                       title = 'Trends in occurrence by year')
        })
        output$Death <- renderPlotly({
            # Deduplication and limit year
            Deduplication <- Cohort %>%
                filter(!is.na(DEATH_DATE)) %>%
                distinct(SUBJECT_ID, .keep_all = TRUE)

            # Frequency table
            yearTable <- sort(unique(year(Deduplication$DEATH_DATE)))
            deathN <- data.frame()

            for (i in 1:length(yearTable)){
                year <- yearTable[i]

                yearcohort <- Deduplication %>%
                    filter(year(DEATH_DATE) == yearTable[i])
                cohortN <- nrow(yearcohort)

                totalN <- (nrow(Deduplication)*length(yearTable))

                deathN <- as.data.frame(rbind(deathN, c(year, cohortN, totalN)))

            }
            colnames(deathN) <- c("year", "cohortN", "totalN")

            # Graph
            death <- plot_ly(deathN,
                             x = ~year,
                             y = ~cohortN,
                             type = 'scatter',
                             mode = 'lines',
                             text = ~paste0('year: ', year, '\n', cohortN),
                             textposition = 'middle right',
                             hoverinfo = 'text') %>%
                layout(xaxis = list(title = 'Year'),
                       yaxis = list(title = 'Count'),
                       title = 'Trends in death by year')
        })

        # TNM stage
        output$Ts <- renderPlotly(TGraph())
        output$Ns <- renderPlotly(NGraph())
        output$Ms <- renderPlotly(MGraph())

        # Average
        output$AvgObserPeriod <- renderUI({
            summaryBox3(title = tags$b("Mean(SD) duration of Observation(days)"),
                        value = as.vector(Ave_obserperiod),
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AvgVisit <- renderUI({
            summaryBox3(title = tags$b("Mean(SD) Visits"),
                        value = as.vector(Ave_visit),
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AveEmerVisit <- renderUI({
            summaryBox3(title = tags$b("Mean(SD) Emergency Visits"),
                        value = as.vector(Ave_EmerVisit),
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AveHosVisit <- renderUI({
            summaryBox3(title = tags$b("Mean(SD) Hospitalization"),
                        value = as.vector(Ave_HosVisit),
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AveHOSperiod <- renderUI({
            summaryBox3(title = tags$b("Mean(SD) duration of Hospitalization(days)"),
                        value = as.vector(Ave_HOSperiod),
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })

        # Flow of Tx
        output$Sankey <- renderUI({
            tags$iframe(seamless = "seamless",
                        src = TRACERflow,
                        width = "100%",
                        height = "800px")

        })

        # Diagnosis list
        output$totalDiagnosis <- DT::renderDataTable(sortDiagnosis())
        output$genderDiagnosis <- DT::renderDataTable(rc_sex())
        output$ageDiagnosis <- DT::renderDataTable(rc_age())

        # Biomarker
        output$MSI_b <- renderPlotly({
            msi <- as.data.frame(table(BiopsyResult$MSI))
            msi_b <- plot_ly(msi,
                             x = ~Var1,
                             y = ~Freq,
                             color = ~Var1,
                             text = ~Freq,
                             type = 'bar',
                             textposition = 'outside',
                             hoverinfo = 'text') %>%
                layout(xaxis = list(title = "",
                                    categoryarray = "Stable", "Low", "High"),
                       yaxis = list(title = ""))
            })
        output$MSI_p <- renderPlotly({
            msi <- as.data.frame(table(BiopsyResult$MSI))
            msi_p <- plot_ly(msi, labels = ~Var1, values = ~Freq, type = 'pie',
                             textposition = 'inside',
                             textinfo = 'label+percent',
                             insidetextfont = list(color = 'black'),
                             marker = list(line = list(color = '#FFFFFF', width = 1)),
                             showlegend = T) %>%
                layout(colorway = c('#D3D6A7','#F6C297','#FBD591'))
        })
        output$cancerType <- renderPlotly({
            cType <- as.data.frame(table(BiopsyResult$DIAGNOSIS))
            G_cType <- plot_ly(cType,
                               x = ~Var1,
                               y = ~Freq,
                               color = ~Var1,
                               text = ~Freq,
                               type = 'bar',
                               textposition = 'outside',
                               hoverinfo = 'text',
                               showlegend = F) %>%
                layout(xaxis = list(title = "",
                                    categoryorder = ""),
                       yaxis = list(title = ""))
        })


        # Inidividual Level
        # Summary
        B_summary <- eventReactive(input$subjectid, {rc_smPlotly()})
        output$summary <- renderPlotly(B_summary())

        # Lab
        B_lablist <- eventReactive(input$labDateSearch, {rc_labList()})
        output$Lablist <- DT::renderDataTable({B_lablist()})
        output$Labgraph <- renderPlotly(rc_labGraph())

        # Drug
        output$DrugGraph <- renderPlotly(rc_drugGraph())
        B_drugperiod <- eventReactive(input$drugPeriodSearch, {rc_druglist()})
        output$Druglist <- DT::renderDataTable({B_drugperiod()})


        ## Extract cohort
        # showTable
        dataView <- eventReactive(input$Preview, {sel_data()})
        output$preview <- renderDataTable({dataView()})

        # Extract table
        observeEvent(input$ExtractData, {
            tableName <- input$TableName
            sel_cohort <- sel_data()
            sel_cohort %>%
                select(COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE) %>%
                distinct(COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE)

            DatabaseConnector::insertTable(connection,
                                           databaseSchema = cohortDatabaseSchema,
                                           tableName = tableName,
                                           data = sel_cohort,
                                           createTable = TRUE,
                                           progressBar = TRUE
                                           )

            shinyalert("Done!", "Check your Database.", type = "success")
        })

    }

)
