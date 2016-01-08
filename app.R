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

    ## Do sanity checks
    ##    if input$n2


    N <- reactive(sapply(1:input$reps, function(i) {input$n1*input$n2 / sum(sample(c(rep(1,input$n1), rep(0, input$n-input$n1)), size=intput$n2))}))

  output$plot <- renderPlot({
    par(mar=c(4,4,0,0)+.1, cex=1.4)
    hist(N, col="lightblue")
  })


#  output$rows_out <- renderText({
#    paste(c('You selected these rows on the page:', input$rows, "UUU"),
#          collapse = ' ')
#  })


}

shinyServer(server)


shinyApp(ui = ui, server = server)


