source("getColNames.R")

init_ui_with_data <- function(data, session, output, factor_list_original) {
  # Initialize the user interface with all of the data
  
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
                           label = "Average Over Factors",
                           choices = factorColNames)
  
  updateCheckboxGroupInput(session, "plotReplicateCheckboxGroup",
                           label = "Plot Replication Factors",
                           choices = factorColNames)
  
  updateSelectInput(session, "tickFactorsselectInput",
                    label = "XTick Factor Order",
                    choices = factorColNames,
                    selected = defaultSelectedFactor)  # Set the first one or two factor columns to be selected by default
  
  updateSelectInput(session, "colorFactorSelectInput",
                    label = "Color Factor",
                    choices = factorColNames)
  
  updateCheckboxGroupInput(session, "facetFactorCheckboxGroup",
                           label = "Facet Factor",
                           choices = factorColNames)
  
  output$dataTable <- renderDT({
    datatable(data)
  })
}