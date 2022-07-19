shinyApp(
    ui <- navbarPage(
        theme = shinythemes::shinytheme("flatly"),
        title = "Visual Dashboard",

        # Database Level
        tabPanel("Database level",
                 tabBox(width = 12,
                        # SUMMARY TAB
                        tabPanel(title = tags$b("SUMMARY"),
                                 mainPanel(width = 12,
                                           # 1st line
                                           # Proportion of Sex and age group
                                           fluidRow(column(6,
                                                           column(6,
                                                                  box(plotOutput("Sex"),
                                                                      title = tags$b("sex Proportion"),
                                                                      width = NULL,
                                                                      height = "100%")
                                                           ),
                                                           column(6,
                                                                  box(plotOutput("Age"),
                                                                      title = tags$b("age Proportion"),
                                                                      width = NULL,
                                                                      height = "100%")
                                                           )
                                           ),
                                           # Proportion of TNM stage
                                           column(6,
                                                  box(plotOutput("TNM"),
                                                      title = tags$b("TNM Proportion"),
                                                      width = NULL,
                                                      height = "100%")
                                           )
                                           ),
                                           # 2nd line
                                           # Trends of Occurrence and Death
                                           fluidRow(column(6,
                                                           column(6,
                                                                  box(plotOutput("Occurrence"),
                                                                      title = tags$b("Trand of Occurrence"),
                                                                      width = "50%",
                                                                      height = "100%")
                                                           ),
                                                           column(6,
                                                                  box(plotOutput("Death"),
                                                                      title = tags$b("Trand of Death"),
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
                                                                              choices = list('Female', 'Man')
                                                                  ),
                                                                  DT::dataTableOutput("genderDiagnosis")
                                                         ),
                                                         # 3rd tabbox age group
                                                         tabPanel(title = tags$b("Age Group"),
                                                                  selectInput(inputId = "chooseAge",
                                                                              label = NULL,
                                                                              choices = list('10',
                                                                                             '20',
                                                                                             '30',
                                                                                             '40',
                                                                                             '50',
                                                                                             '60',
                                                                                             '70',
                                                                                             '80',
                                                                                             '90')
                                                                  ),
                                                                  DT::dataTableOutput("ageDiagnosis")
                                                         )
                                                  )
                                           )
                                           )
                                 )
                        ),
                        # TREATMENT TAB
                        tabPanel(title = tags$b("Treatment"),
                                 # Average of Visit and Duration of Hospitalization
                                 fluidRow(column(6,
                                                 column(6, uiOutput("AvgVisit")),
                                                 column(6, uiOutput("AvgHos"))
                                 ),
                                 column(6,
                                        column(6, uiOutput("AvgPostVisit")),
                                        column(6, uiOutput("AvgPostHos")),
                                 )
                                 ),
                                 # Flow of Chemotherapy
                                 fluidRow(htmlOutput("Sankey")
                                 )
                        )
                 )
        ),

        # Patient Level
        tabPanel("Patient level",
                 sidebarPanel(width = 2,
                              h4("Enter the Subject ID"),
                              sidebarSearchForm(textId = "searchSubject",
                                                label = " SubjectID",
                                                buttonId = "searchButton"
                              ),
                              br(),
                              h4("Lab list"),
                              dateInput("Labdate",
                                        label = "Search Date: yyyy-mm-dd",
                                        min = "1994-01-01",
                                        max = "2021-07-31",
                                        format = "yyyy-mm-dd"
                              ),
                              br(),
                              h4("Drug list"),
                              dateRangeInput("Drugdate",
                                             label = "Period: yyyy-mm-dd to yyyy-mm-dd",
                                             min = "1994-01-01",
                                             max = "2021-07-31",
                                             format = "yyyy-mm-dd")
                 ),

                 tabBox(width = 10,
                        # Subject Information
                        tabPanel("Info",
                                 mainPanel(width = 10,
                                           fluidRow(box(title = "Total Medical Schedule",
                                                        plotlyOutput("summary"),
                                                        width = "100%")
                                           )
                                 )
                        ),

                        # Subject Lab results
                        tabPanel("Lab",
                                 mainPanel(width = 10,
                                           fluidRow(column(6,
                                                           box(title = tags$b("Lab Result"),
                                                               width = "100%",
                                                               height = NULL,
                                                               DT::dataTableOutput("Lablist")
                                                           )
                                           ),
                                           column(6,
                                                  box(title = tags$b("Trend of Results"),
                                                      sidebarSearchForm(textId = "Labcode",
                                                                        label = "Search test code",
                                                                        buttonId = "CodeSearchButton"
                                                      ),
                                                      plotlyOutput("Labgraph"),
                                                      width = "100%",
                                                      height = NULL)
                                           )
                                           )
                                 )
                        ),

                        # Subject Drug
                        tabPanel("Drug",
                                 mainPanel(width = 10,
                                           fluidRow(box(title = tags$b("Drug Graph"),
                                                        plotlyOutput("DrugGraph"),
                                                        width = "100%",
                                                        height = NULL
                                           )
                                           ),

                                           fluidRow(box(title = tags$b("Drug List"),
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
        tabPanel("Download Cohort",
                 sidebarPanel(width = 9,
                              h3(strong("Select Options")),
                              fluidRow(column(4,
                                              box(h4(strong("Sex")),
                                                  checkboxGroupButtons(inputId = "allInput",
                                                                       choices = "All / NONE",
                                                                       size = "sm",
                                                                       selected = "All / NONE"),
                                                  checkboxGroupInput("selSex",
                                                                     label = NULL,
                                                                     choices = list('Female', 'Man'),
                                                  )
                                              )
                              ),
                              column(4,
                                     box(h4(strong("Age")),
                                         sliderInput('ageVar','Range of age:',
                                                     min = min(Cohort$age, na.rm = TRUE),
                                                     max = max(Cohort$age, na.rm = TRUE),
                                                     value = c(min(Cohort$age, na.rm = TRUE),
                                                               max(Cohort$age, na.rm = TRUE)
                                                     )
                                         )
                                     )
                              ),
                              column(4,
                                     box(h4(strong("TNM stage")),
                                         width = 10,
                                         column(4,
                                                checkboxGroupButtons(inputId = "allInput",
                                                                     label = "T stage",
                                                                     choices = "All / NONE",
                                                                     size = "sm",
                                                                     selected = "All / NONE"),
                                                checkboxGroupInput("selT",
                                                                   label = NULL,
                                                                   choices = c("T1", "T2", "T3", "T4"))
                                         ),
                                         column(4,
                                                checkboxGroupButtons(inputId = "allInput",
                                                                     label = "N stage",
                                                                     choices = "All / NONE",
                                                                     size = "sm",
                                                                     selected = "All / NONE"),
                                                checkboxGroupInput("selN",
                                                                   label = NULL,
                                                                   choices = c("N0","N1", "N2", "N3"))
                                         ),
                                         column(4,
                                                checkboxGroupButtons(inputId = "allInput",
                                                                     label = "M stage",
                                                                     choices = "All / NONE",
                                                                     size = "sm",
                                                                     selected = "All / NONE"),
                                                checkboxGroupInput("selM",
                                                                   label = NULL,
                                                                   choices = c("M0", "M1"))
                                         )
                                     )
                              )
                              ),
                              fluidRow(column(4,
                                              box(h4(strong("Year")),
                                                  dateRangeInput("selDate",
                                                                 label = "Period: ",
                                                                 min = "1994-01-01",
                                                                 max = "2021-07-31",
                                                                 format = "yyyy-mm-dd")
                                              )
                              )
                              )
                 ),
                 sidebarPanel(width = 3,
                              position = c("right"),
                              fluidRow(box(title = tags$b("DOWNLOAD"),
                                           textInput("fileName", label = "File Name..."),
                                           downloadButton("downloadData", "Download")
                              )
                              )
                 )
        )
    )


    # Define server logic required to draw a histogram
    , server <- function(input, output) {
        ### reactive
        ## Database Level
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

        ## Patient Level
        # summary
        rc_smPlotly <- reactive({
            subjectId <- as.numeric(input$searchSubject)
            # Make summary data table
            # Visit summary
            sql_visit <- "SELECT	distinct a.subject_id, m.visit_concept_id, z.concept_name, m.visit_start_date, m.visit_end_date
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
            sql_TNM <- "SELECT distinct a.subject_id,	b.dschdate, b.organcd, b.seer, c.stagedet
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.cancer_register b on a.subject_id = b.cdm_patno
    left outer join @cdm_database_schema.cancer_tnm c on a.subject_id = c.person_id
    where a.subject_id = @subjectID;"

            sql_TNM <- SqlRender::render(sql_TNM,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         cohort_table = cohortTable,
                                         subjectID = subjectId)

            df_TNM <- as.data.frame(DatabaseConnector::querySql(connection, sql_TNM))

            sm_TNM <- df_TNM %>%
                select(STAGEDET, DSCHDATE) %>%
                arrange(DSCHDATE) %>%
                mutate(type = "TNM")

            sm_TNM$tag <- sm_TNM$STAGEDET

            setnames(sm_TNM,
                     old = c("STAGEDET",
                             "DSCHDATE"),
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

        #
        #   # select options
        #   # updateCheckboxGroupInput()
        #   # updateSliderInput(session, "ageVar",
        #   #                   value = c())
        #

        sel_data <- function(){

            #filter ID
            filter_id <- Cohort %>% distinct(SUBJECT_ID)
            filter_id <- paste(filter_id, collapse = ", ")
            filter_id <- gsub('\n|\\(|\\)|c', '', filter_id, perl = T)

            # select Sex
            observe({
                sex <- input$allInput
                if(!is.null(sex)){
                    sex <- Cohort$GENDER_SOURCE_VALUE
                }else{
                    sex <- character(0)
                }

                updateCheckboxGroupInput(
                    session,
                    "",
                    label = NULL,
                    choices = Cohort$GENDER_SOURCE_VALUE,
                    selected = sex
                )
            })

            sql_subject <- "SELECT distinct a.subject_id, a.cohort_start_date, a.cohort_end_date, a.gender_source_value, a.year_of_birth
    FROM @cohort_database_schema.@cohort_table a
    where a.subject_id in (@subjectID) and a.gender_source_value in (@);"

            sql_subject <- SqlRender::render(sql_subject,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable,
                                             subjectID = filter_id)


            df_subject <- as.data.frame(DatabaseConnector::querySql(connection, sql_subject))
        }

        ### render
        # Database Level
        # Proportion
        output$Sex <- renderPlot(sexProportionGraph())
        output$Age <- renderPlot(ageProportionGraph())

        # Trends
        output$Occurrence <- renderPlot({
            # Deduplication and limit year
            Deduplication <- Cohort %>%
                distinct(SUBJECT_ID, CONDITION_CONCEPT_ID, CONCEPT_NAME, .keep_all = TRUE)

            # Frequency table
            yearTable <- as.data.frame(table(year(Deduplication$CONDITION_START_DATE)))

            # Graph
            yearTable %>%
                ggplot(aes(x = Var1, y = Freq, group = 1)) +
                geom_point() +
                geom_line() +
                geom_text(aes(label = Freq),
                          check_overlap = TRUE,
                          vjust = - 0.5) +
                labs(x = "year",
                     y = "count",
                     title = "Trends in occurrence by year") +
                theme_bw()
        })
        output$Death <- renderPlot({
            # Deduplication and limit year
            Deduplication <- Cohort %>%
                filter(!is.na(DEATH_DATE)) %>%
                distinct(SUBJECT_ID, .keep_all = TRUE)

            # Frequency table
            yearTable <- as.data.frame(table(year(Deduplication$DEATH_DATE)))

            # Graph
            yearTable %>%
                ggplot(aes(x = Var1, y = Freq, group = 1)) +
                geom_point() +
                geom_line() +
                geom_text(aes(label = Freq),
                          check_overlap = TRUE,
                          vjust = - 0.7) +
                labs(x = "year",
                     y = "count",
                     title = "Trends in death by year") +
                theme_bw()
        })

        # TNM stage
        output$TNM <- renderPlot(TNMProportionGraph())

        # Flow of Tx
        output$Sankey <- renderUI({
            tags$iframe(seamless = "seamless",
                        src = "http://14rg.abmi.kr/files/code/Dashboard/shinyAPP/CRC_tx_Flow.html",
                        width = "100%",
                        height = "800px")

        })

        # Average
        totalSubjectN <- Cohort %>%
            distinct(SUBJECT_ID) %>%
            summarise(n = n())

        output$AvgVisit <- renderUI({
            sql_AvgVisit <- "with CRC_concept as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (197500,74582)
    UNION  select c.concept_id
    from  @cdm_database_schema.CONCEPT c
    join  @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (197500,74582)
    and c.invalid_reason is null
    )
    SELECT distinct a.subject_id, b.condition_concept_id, b.condition_start_date, m.visit_start_date
    INTO #visitTemp
    FROM @cohort_database_schema.@cohort_table a
    LEFT OUTER JOIN @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
    LEFT OUTER JOIN @cdm_database_schema.condition_occurrence b on a.subject_id = b.person_id
    and b.condition_concept_id in (select concept_id from CRC_concept)
    where m.visit_start_date > b.condition_start_date

    SELECT COUNT(subject_id) FROM #visitTemp
    DROP TABLE #visitTemp;"

            sql_AvgVisit <- SqlRender::render(sql_AvgVisit,
                                              cohort_database_schema = cohortDatabaseSchema,
                                              cdm_database_schema = cdmDatabaseSchema,
                                              cohort_table = cohortTable)

            totalVisitN <- DatabaseConnector::querySql(connection, sql_AvgVisit)
            R_AvgVisit <- round(totalVisitN / totalSubjectN, digits = 0)

            summaryBox3(title = tags$b("Average number of Visit"),
                        value = R_AvgVisit,
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AvgHos <- renderUI({
            sql_AvgHos <- "with CRC_concept as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (197500,74582)
    UNION select c.concept_id
    from @cdm_database_schema.CONCEPT c
    join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (197500,74582)
    and c.invalid_reason is null
    )
    SELECT distinct a.subject_id, b.condition_concept_id, b.condition_start_date, m.visit_start_date, m.visit_end_date
    INTO #hosTemp
    FROM @cohort_database_schema.@cohort_table a
    LEFT OUTER JOIN @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
    LEFT OUTER JOIN @cdm_database_schema.condition_occurrence b on a.subject_id = b.person_id
    and b.condition_concept_id in (select concept_id from CRC_concept)
    where m.visit_start_date > b.condition_start_date

    SELECT SUM(DATEDIFF(dd, visit_start_date, visit_end_date))
    FROM #hosTemp

    DROP TABLE #hosTemp;"

            sql_AvgHos <- SqlRender::render(sql_AvgHos,
                                            cohort_database_schema = cohortDatabaseSchema,
                                            cdm_database_schema = cdmDatabaseSchema,
                                            cohort_table = cohortTable)

            totalHosN <- DatabaseConnector::querySql(connection, sql_AvgHos)
            R_AvgHos <- round(totalHosN / totalSubjectN, digits = 0)

            summaryBox3(title = tags$b("Average length of Hospitalization(day)"),
                        value = R_AvgHos,
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AvgPostVisit <- renderUI({
            sql_PostVisit <- "with Chemotherapy as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (37521981)
    UNION select c.concept_id
    from @cdm_database_schema.CONCEPT c
    join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (37521981)
    and c.invalid_reason is null
    )
    select distinct a.subject_id, b.procedure_concept_id, b.procedure_date, m.visit_start_date
    INTO #PostChemoVisit
    from @cohort_database_schema.@cohort_table a
    LEFT OUTER JOIN @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
    LEFT OUTER JOIN @cdm_database_schema.procedure_occurrence b on a.subject_id = b.person_id
    and b.procedure_concept_id in (select concept_id from Chemotherapy)
    where m.visit_start_date > b.procedure_date

    SELECT COUNT(visit_start_date) FROM #PostChemoVisit
    DROP TABLE #PostChemoVisit"

            sql_PostVisit <- SqlRender::render(sql_PostVisit,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               cohort_table = cohortTable)

            totalPostVisitN <- DatabaseConnector::querySql(connection, sql_PostVisit)
            R_AvgPostVisit <- round(totalPostVisitN / totalSubjectN, digits = 0)

            summaryBox3(title = tags$b("Average number of Post-treatment Visit"),
                        value = R_AvgPostVisit,
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })
        output$AvgPostHos <- renderUI({
            sql_PostHos <- "with Chemotherapy as (select concept_id from @cdm_database_schema.CONCEPT where concept_id in (37521981)
    UNION select c.concept_id
    from @cdm_database_schema.CONCEPT c
    join @cdm_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
    and ca.ancestor_concept_id in (37521981)
    and c.invalid_reason is null
    )
    select distinct a.subject_id, b.procedure_concept_id, b.procedure_date, m.visit_concept_id, m.visit_start_date
    INTO #PostChemoVisit
    from @cohort_database_schema.@cohort_table a
    LEFT OUTER JOIN @cdm_database_schema.visit_occurrence m on a.subject_id = m.person_id
    LEFT OUTER JOIN @cdm_database_schema.procedure_occurrence b on a.subject_id = b.person_id
    and b.procedure_concept_id in (select concept_id from Chemotherapy)
    where m.visit_start_date > b.procedure_date and (m.visit_concept_id = 9201 or m.visit_concept_id = 262)

    SELECT COUNT(visit_start_date) FROM #PostChemoVisit
    DROP TABLE #PostChemoVisit"

            sql_PostHos <- SqlRender::render(sql_PostHos,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_table = cohortTable)

            totalPostHosN <- DatabaseConnector::querySql(connection, sql_PostHos)
            R_AvgPostHos <- round(totalPostHosN / totalSubjectN, digits = 0)

            summaryBox3(title = tags$b("Average number of Post-treatment Hospitalization"),
                        value = R_AvgPostHos,
                        width = NULL,
                        style = "primary",
                        icon = NULL)
        })

        # Diagnosis list
        output$totalDiagnosis <- DT::renderDataTable(sortDiagnosis())
        output$genderDiagnosis <- DT::renderDataTable(rc_sex())
        output$ageDiagnosis <- DT::renderDataTable(rc_age())


        # Patient Level
        # Summary
        output$summary <- renderPlotly(rc_smPlotly())

        # Lab
        output$Lablist <- DT::renderDataTable(rc_labList())
        output$Labgraph <- renderPlotly(rc_labGraph())

        # Drug
        output$DrugGraph <- renderPlotly(rc_drugGraph())
        output$Druglist <- DT::renderDataTable(rc_druglist())


        ## Download cohort
        # download
        output$downloadData <- downloadHandler(
            filename  = function(){
                paste(input$fileName,
                      ".csv",
                      sep = "")
            },
            content = function(file){
                write.csv(sel_data(), file)
            }
        )

    }

)
