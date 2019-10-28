#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(tidyverse)
library(quantreg)
library(DT)
source("model.R")

# Define UI
ui <- fluidPage(
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word frequency distribution:",
                        list("Zipfian" = "zipf", 
                             "Uniform" = "uniform")),
            sliderInput("vocab_size", "Total vocabulary size:", 
                        min=500, max=10000, value=5000, step=500),
            sliderInput("n_learners", "Number of learners:", 
                        min=10, max=200, value=50, step=10),
            sliderInput("input_rate", "Input rate (tokens/day):", # HR welfare: 616, prof: 2153
                        min=100, max=6000, value=1000, step=100),
            sliderInput("threshold", "Number of occurrences needed to learn a word:", 
                        min=0, max=1000, value=100, step=10),
            sliderInput("max_age", "Age range (months):", 
                        min=0, max=48, value=36, step=3),
            sliderInput("learning_rate", "Mean learning rate (scales value of occurrence; truncated at .1):", 
                        min = 1, max = 10, value = 1, step= 1),
            sliderInput("proc_speed", "Mean processing speed:", 
                        min = 0, max = 1, value = 0.5, step= 0.1),
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ageVocab"),
           textOutput("summary"),
           DT::dataTableOutput("mytable")
        )
    )
)


# server logic
server <- function(input, output) {
    sim_data = reactive({
        simulate(input$vocab_size, input$distro, input$input_rate, 
            input$n_learners, input$threshold, input$max_age, input$learning_rate)
    })
    
    output$ageVocab <- renderPlot({
        #sim = simulate(input$vocab_size, input$distro, input$input_rate, 
        #               input$n_learners, input$threshold, input$max_age, input$learning_rate)
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data(), aes(x=month, y=words)) + geom_jitter(width=.1, alpha=.1) + geom_point(alpha=.1) +  
            geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            labs(colour="Quantile") + geom_smooth() + 
            geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) + 
            xlab("Age (months") + ylab("Vocabulary Size") + theme_bw() + 
            ylim(0,input$vocab_size) + xlim(0, input$max_age) 
    })
    
    output$summary <- renderText({ 
        paste("Mean of cumulative words known per month.") # input$distro
    })
    
    output$mytable = DT::renderDataTable({
        sim_data() %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words)) %>%
            datatable() %>% formatRound(columns=c("mean","sd"), digits=0)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
