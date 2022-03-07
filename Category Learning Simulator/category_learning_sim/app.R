#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

block <- function(trials, groups) {
  responses <- floor(runif(trials, 0, groups))
  correct <- responses == 1
  return(sum(correct)/trials)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Category Learning Random Responder Simulation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            numericInput("nsims",
                         "Number of Simulations",
                         value = 10000,
                         min = 5000,
                         max = 100000,
                         step = 5000),
            numericInput("trialsPerBlock",
                         "Trials per Learning Block",
                         value = 50,
                         min = 1,
                         max = 200,
                         step = 1),
            numericInput("ncats",
                         "Number of Categories",
                         value = 2,
                         min = 2,
                         max = 10,
                         step = 1),
            numericInput("threshold",
                         "Accuracy Percentile",
                         value = .95,
                         min = .5,
                         max = .999,
                         step = .001)
        ),

        mainPanel(
            fluidRow(
                plotOutput("distPlot")
            ),
            fluidRow(
                plotOutput("distPlot2")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        sims <- c()
        for (i in 1:input$nsims) {
            sims <- append(sims, block(input$trialsPerBlock, input$ncats))
        }
        upperLimit <- quantile(sims, input$threshold)
        maxDensity <- max(density(sims)$y)
        
        ggplot() +
            geom_density(aes(x = sims), fill = "gray", color="black", alpha = .6) +
            geom_vline(xintercept = upperLimit, color = "red", linetype = "dashed", size = 2, alpha = .8) +
            geom_label(aes(label = paste0(input$threshold*100, "th Percentile:\n", round(upperLimit*100,2), "%")), x = upperLimit, y = maxDensity -1, hjust = 0.5) +
            scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
            labs(x = "Accuracy",
                 y = "Density",
                 title = "Accuracy of Random Responders",
                 subtitle = paste0(input$nsims, " simulations, ", input$ncats, " categories, ", input$trialsPerBlock, " trials per block of training")
            ) +
            theme(
                panel.background = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.line.x.bottom = element_line(),
                axis.line.y.left = element_line()
            )
    })
    
    output$distPlot2 <- renderPlot({
        sims <- c()
        runningMean <- c()
        for (i in 1:input$nsims) {
            sims <- append(sims, block(input$trialsPerBlock, input$ncats))
            runningMean <- append(runningMean, mean(sims))
        }
        upperLimit <- quantile(sims, input$threshold)
        maxDensity <- max(density(sims)$y)
        
        ggplot() +
            geom_hline(yintercept = 1/input$ncats, color = "red", linetype = "dashed", size = 2, alpha = .8) +
            geom_line(aes(x = 1:input$nsims, y = runningMean), size = 1.2, alpha = .6) +
            # geom_label(aes(label = paste0("Chance: ", round(100*(1/input$ncats), 2), "%")), x = (9/10)*input$nsims, y = 1/input$ncats + .01, hjust = 0.5) +
            scale_y_continuous(expand = c(0,0)) + #scale_x_continuous(breaks = 1) +
            labs(
                x = "",
                y = "Running Mean",
                title = paste0("Running Mean accuracy across ", input$nsims, " simulations")
            ) +
            theme(
                panel.background = element_blank(),
                # axis.ticks.x = element_blank(),
                # axis.text.x = element_blank(),
                axis.line.x.bottom = element_line(),
                axis.line.y.left = element_line()
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
