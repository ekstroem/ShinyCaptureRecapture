library(shiny)
data(mtcars)


ui <- fluidPage(

    ## Application title
    titlePanel("Distribution of population size estimates for capture-recapture model"),

    ## Sidebar with controls to select the random distribution type
    ## and number of observations to generate. Note the use of the
    ## br() element to introduce extra vertical spacing

    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "True population size:",
                        value = 1000,
                        min = 10,
                        max = 5000),
            br(),

            sliderInput("n1",
                        "Number of individual to capture:",
                        value = 100,
                        min = 20,
                        max = 200),
            br(),

            sliderInput("n2",
                        "Number of individual to recapture:",
                        value = 100,
                        min = 20,
                        max = 200),
            br(),


            sliderInput("reps",
                        "Number of repetitions:",
                        value = 200,
                        min = 1,
                        max = 1000),
            br(),br()
            ),

        mainPanel(
            plotOutput("plot", height="600")
            )


        )
    )

server <- function(input, output) {

    ## Do sanity checks
    ##    if input$n2


    output$plot <- renderPlot({

        n <- input$n
        n1 <- input$n1
        n2 <- input$n2
        reps <- input$reps

        N <- sapply(1:reps, function(i) {n1*n2 / sum(sample(c(rep(1,n1), rep(0, n-n1)), size=n2))})
        meanN <- mean(N)

        par(mar=c(4,4,0,0)+.1, cex=1.4)
        hist(N, col="lightblue", main="")
        abline(v=meanN, lty=3, lwd=3)
  })


#  output$rows_out <- renderText({
#    paste(c('You selected these rows on the page:', input$rows, "UUU"),
#          collapse = ' ')
#  })


}

shinyServer(server)


shinyApp(ui = ui, server = server)


