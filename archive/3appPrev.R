# From here: https://stackoverflow.com/questions/60693960/shiny-layout-build-a-shiny-page-with-a-scrollable-panel-and-a-panel-that-remain

data <- mtcars

ui <- fluidPage(
    tags$head(
        tags$style("html, body { height: 100%; width: 100%}"),
        tags$style("#panel1 {background: #ADD8E6; height: 100px; position: fixed}"),
        tags$style("#panel2 {
            overflow: auto;
            background: orange;
            margin-left: 5px;
        }"),
        tags$style("#panel3 {background: green}")
    ),
    absolutePanel(id = "panel1",
                  height = "100%", width = "20%", right = "80%",
                  selectizeInput(inputId= "obs", label= "Obs", 
                                 choices= names(mtcars), 
                                 selected= names(mtcars)[1],
                                 multiple=F),
                  selectizeInput(inputId= "sublevel", label= "Sublevel", 
                                 choices= sort(unique(mtcars$cyl)), 
                                 selected= sort(unique(mtcars$cyl))[1],
                                 multiple=F)
    ), 
    absolutePanel(id = "panel2", 
                  top = "0%", left = "20%", height = "80%", width = "80%", right = "0%",bottom = "20%",
                  fluidRow(tableOutput("tab")),
                  HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>
                         <br><br><br><br><p>sdsdsd</p>"),
                  fluidRow(textOutput("hi"))

    ),
    absolutePanel(id = "panel3",
                  top = "80%", left = "20%", height = "20%", width = "80%", right = "0%",bottom = "0",
                  p("haha")
    )
)

server <- function(input, output){
    sorted <- reactive({data %>% arrange_(input$obs) %>% filter(cyl == input$sublevel)})
    output$tab= renderTable(sorted())
    output$hi<-renderPrint(paste0("hello"))
}

shinyApp(ui = ui, server = server)
