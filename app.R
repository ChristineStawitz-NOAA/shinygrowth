#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    # Application title
    titlePanel("Simulate Growth Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("L1",
                        "L1 (cm):",
                        min=1,
                        max=40,
                        value=17),
            sliderInput("L_inf",
                        "Base L infinity (cm):",
                        min = 10,
                        max = 400,
                        value = 55),
            sliderInput("a3",
                        "Recruit age:",
                        min=0,
                        max=10,
                        value=2),
            sliderInput("A",
                        "Plus group age:",
                        min=10,
                        max=200,
                        value = 17),
            sliderInput("g",
                        "Density dependent effect (g):",
                        min=-5,
                        max=5,
                        value=0),
            sliderInput("meanB",
                        "Mean biomass density (kg/ha):",
                        min=0,
                        max=25,
                        value=0),
           sliderInput("degC",
                        "Change in degrees Celsius", 
                       min=-15,
                       max=15,
                       value=0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lengthPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    require(ggplot2)

    output$lengthPlot <- renderPlot({
        
        #outlist <- isolate(reactiveValuesToList(input))
        #list2env(outlist,environment())
        k <- 0.14
        ages <- seq(input$a3, input$A)
        
        getLengths <- function(L_inf, L1, k, ages, l_var = NULL, k_var = NULL){
        #Calculate mean lengths
            meanLength <- L_inf+(L1-L_inf)*exp(-k*(ages-min(ages)))
            mat <- data.frame(cbind(ages, meanLength))
            names(mat) <- c("ages", "lengths")
            return(mat)
        }
        
        meanL <- getLengths(input$L_inf, input$L1, k, ages)
        ddL <- getLengths(input$L_inf-input$g*input$meanB,
                          input$L1,
                          k,
                          ages)
        envLinked <- getLengths(input$L_inf-input$L_inf*.019*input$degC, input$L1, 
                                k=k+k*0.043*input$degC,
                                ages)

        # draw the histogram with the specified number of bins
        p <- ggplot(aes(ages, lengths), data = data.frame(meanL)) +
            geom_point() +
            geom_line(data=ddL, colour="blue") +
            geom_line(data=envLinked, colour = "green") +
            theme_classic()
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)