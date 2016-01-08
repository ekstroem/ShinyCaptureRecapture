library(shiny)
data(mtcars)


ui <- fluidPage(

    ## Application title
    titlePanel("Distribution of estimates for capture-recapture model"),

    ## Sidebar with controls to select the random distribution type
    ## and number of observations to generate. Note the use of the
    ## br() element to introduce extra vertical spacing

    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "True population size:",
                        value = 100,
                        min = 10,
                        max = 20000),
            br(),

            sliderInput("n1",
                        "Number of individual to capture:",
                        value = 20,
                        min = 1,
                        max = 200),
            br(),

            sliderInput("n2",
                        "Number of individual to recapture:",
                        value = 100,
                        min = 10,
                        max = 500),
            br(),


            sliderInput("reps",
                        "Number of repetitions:",
                        value = 200,
                        min = 1,
                        max = 1000),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
            ),

        mainPanel(
            plotOutput("plot")
            )


        )
    )

server <- function(input, output) {
  output$tbl <- renderDataTable(
    trees,
    options = list(pageLength = 10),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected').siblings().removeClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').indexes().toArray());
    });
}"
  )

  output$dataplot <- renderPlot({
    par(mar=c(4,4,0,0)+.1, cex=1.4)
    plot(Volume ~ Height, data=trees, pch=20)
    if (!is.null(input$rows)) {
        points(trees$Height[input$rows+1], trees$Volume[input$rows+1], col="red", cex=1.6, pch=20)
    }
  })


  output$rows_out <- renderText({
    paste(c('You selected these rows on the page:', input$rows, "UUU"),
          collapse = ' ')
  })

  # Compute the forumla text in a reactive expression since it is
  # shared by the output$caption and output$mpgPlot expressions
    formulaText <- reactive({
      paste("mpg ~", )
    })

  # Return the formula text for printing as a caption
    output$caption <- renderText({
      formulaText()
    })




}

shinyServer(server)


shinyApp(ui = ui, server = server)


