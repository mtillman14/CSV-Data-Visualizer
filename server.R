library(shiny)
library(plotly)
library(shinyjs)
library(DT)
library(dplyr)

source("getColNames.R")

server <- function(input, output, session) {

  #########################################################
  # INITIALIZE REACTIVE VALUES
  #########################################################

  # Initialize reactive values to store the original and modified data frame
  data_store_original <- reactiveVal(NULL)
  data_store <- reactiveVal(NULL)

  # Initialize reactive value to store the full list of factors.
  factor_list_original <- reactiveVal(NULL)

  #########################################################
  # LOAD CSV
  #########################################################

  observeEvent(input$browseFileButton, {
    # Code to handle file browsing and loading
    showModal(modalDialog(
      title = "File Browser",
      fileInput("fileInput", "Choose CSV File", accept = ".csv"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("loadFile", "Load File")
      )
    ))
  })

  observeEvent(input$loadFile, {
    req(input$fileInput)
    data <- read.csv(input$fileInput$datapath)    

    output$filePath <- renderText({
      paste("Loaded file name:", input$fileInput$name)
    })

    removeModal()

    factorColNames <- getFactorColNames(data)
    variableColNames <- getVariableColNames(data)

    # Store the full list of factors in a reactive value
    factor_list_original(factorColNames)

    # Make sure each of the factorColNames are factors in the data data frame
    for (col in factorColNames) {
        data[[col]] <- as.factor(data[[col]])
    }

    if (length(factorColNames) > 2) {
        defaultSelectedFactor <- factorColNames[2]
    } else {
        defaultSelectedFactor <- factorColNames[1]
    }

    updateSelectInput(session, "outcomeMeasureDropDown",
                      label = "Variable to Plot",
                      choices = variableColNames)

    updateCheckboxGroupInput(session, "dataReductionFactorsCheckboxGroup",
                             label = "Data Reduction Factors",
                             choices = factorColNames)

    updateCheckboxGroupInput(session, "plotReplicateCheckboxGroup",
                             label = "Plot Replication Factors",
                             choices = factorColNames)

    updateSelectizeInput(session, "tickFactorsSelectizeInput",
                         label = "XTick Factor Order",
                         choices = factorColNames,
                         selected = defaultSelectedFactor)  # Set the first two factor columns to be selected by default

    output$dataTable <- renderDT({
        datatable(data)
    })

    # Store the data in a reactive value
    data_store_original(data)
    data_store(data)
  })

  #########################################################
  # SHOW/HIDE SIDEBAR
  #########################################################
  observeEvent(input$exportButton, {
    # Code to handle export functionality
    showModal(modalDialog(
      title = "Export Data",
      "Export functionality is not implemented yet.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$settingsButton, {
    # Code to handle settings functionality
    showModal(modalDialog(
      title = "Settings",
      "Settings functionality is not implemented yet.",
      easyClose = TRUE,
      footer = NULL
    ))
  })  

  #########################################################
  # SIDEBAR: GROUPING SETTINGS
  #########################################################
  observeEvent(input$dataReductionFactorsCheckboxGroup, {
    req(data_store_original())
    data <- data_store_original()

    # Get the selected factors from the checkbox group
    selectedFactors <- input$dataReductionFactorsCheckboxGroup

    variableColNames <- getVariableColNames(data)
    factorColNames <- getFactorColNames(data)
    unselectedFactors <- setdiff(factorColNames, selectedFactors)

    if (length(selectedFactors) == 0) {
        averagedData <- data
        xtickFactors <- factorColNames
        plotReplicationFactors <- factorColNames
    } else {
        xtickFactors <- unselectedFactors
        plotReplicationFactors <- unselectedFactors
        # Average the data by the selected factors
        averagedData <- data %>%
        group_by(across(all_of(unselectedFactors))) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
        select(-one_of(selectedFactors))  # Remove the selected factors from the data frame
    }

    # Update the Plot Replication Factors checkbox group
    updateCheckboxGroupInput(session, "plotReplicateCheckboxGroup",
                             label = "Plot Replication Factors",
                             choices = plotReplicationFactors)

    # Update the XTick Factor Order selectize input
    updateSelectizeInput(session, "tickFactorsSelectizeInput",
                         label = "XTick Factor Order",
                         choices = xtickFactors)  # Set the first two factor columns to be selected by default

    # Update the data table with the averaged data
    output$dataTable <- renderDT({
    datatable(averagedData)
    })

    # Update the data store with the averaged data
    data_store(averagedData)
  }, ignoreNULL = FALSE)

  observeEvent(input$tickFactorsSelectizeInput, {
    dataReductionFactors <- factor_list_original()
    selectedDataReductionFactors <- input$dataReductionFactorsCheckboxGroup

    unselectedDataReductionFactors <- setdiff(dataReductionFactors, selectedDataReductionFactors)

    selectedXTickFactors <- input$tickFactorsSelectizeInput    

    # Plot replication factors are the unselected data reduction factors, and the unselected XTicks
    plotReplicationFactors <- setdiff(unselectedDataReductionFactors, selectedXTickFactors)

    updateCheckboxGroupInput(session, "plotReplicateCheckboxGroup",
                             label = "Plot Replication Factors",
                             choices = plotReplicationFactors)
  })

  #########################################################
  # SIDEBAR: EXPORT SETTINGS
  #########################################################

  #########################################################
  # SIDEBAR: RUN PLOT
  #########################################################
  observeEvent(input$runPlotButton, {
    data <- data_store()
    factorColNames <- factor_list_original()
    selectedOutcomeMeasure <- input$outcomeMeasureDropDown
    # Remove all of the variable columns from the data frame except for the selected outcome measure
    selectedDataReductionFactors <- input$dataReductionFactorsCheckboxGroup
    selectedXTickFactors <- input$tickFactorsSelectizeInput
    # selectedPlotReplicationFactors <- input$plotReplicateCheckboxGroup

    # Add defaultFactor column
    data$Observation <- 1:nrow(data)
    if (length(selectedXTickFactors) == 0 || length(selectedXTickFactors) == length(factorColNames)) {
        selectedXTickFactors <- c("Observation")
    }

    # Create a formula for the x-axis using the selected XTick factors
    xFormula <- as.formula(paste0("~", paste(selectedXTickFactors, collapse = " + ")))
    yFormula <- as.formula(paste0("~", selectedOutcomeMeasure))

    output$plot <- renderPlotly({
        # Example plotly plot
        plot_ly(
        data = data,
        x = xFormula,
        y = yFormula,
        type = "scatter"
        )
  })
    # You can use the order of the checkboxes here to update your plot or perform other actions
  })
}