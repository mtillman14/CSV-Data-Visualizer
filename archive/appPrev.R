library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    "title        top_grid    ",
    "sidebar_grid grouped_plot"
  ),
  row_sizes = c(
    "135px",
    "1fr"
  ),
  col_sizes = c(
    "240px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "title",
    card_body(gap = "0px", uiOutput(outputId = "title"))
  ),
  grid_card(
    area = "grouped_plot",
    card_body(plotlyOutput(outputId = "grouped_plot"))
  ),
  grid_card(
    area = "top_grid",
    card_body(
      gap = "0px",
      grid_container(
        layout = c(
          ". browseFileButton",
          ". browseFileButton"
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "1.38fr",
          "0.62fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "browseFileButton",
          card_body(
            gap = "0px",
            actionButton(
              inputId = "browseFileButton",
              label = "Browse File",
              width = "100%"
            )
          )
        )
      )
    )
  ),
  grid_card(
    area = "sidebar_grid",
    card_body(
      grid_container(
        layout = c(
          "tickNestingCheckbox tickNestingCheckbox",
          "tickNestingCheckbox tickNestingCheckbox"
        ),
        row_sizes = c(
          "1.73fr",
          "0.27fr"
        ),
        col_sizes = c(
          "0.84fr",
          "1.16fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "tickNestingCheckbox",
          card_body(
            gap = "0px",
            checkboxGroupInput(
              inputId = "tickNestingCheckboxGroup",
              label = "Tick Nesting",
              choices = list("choice b" = "b")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
   
  output$plot <- renderPlotly({
    plot_ly(
      diamonds[diamonds$cut == input$cut,], 
      x = ~carat
    ) |> 
    add_histogram() 
  })
  

  observeEvent(input$browseFileButton, {
    output$title <- renderUI({
      h1("Hello, World!")
    })

    updateCheckboxGroupInput(session, "tickNestingCheckboxGroup", 
                             label = "Tick Nesting",
                             choices = list("Choice A" = "a", "Choice B" = "b", "Choice C" = "c"))
  })
}

shinyApp(ui, server)
  
