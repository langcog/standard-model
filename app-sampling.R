
library(shiny)
library(tidyverse)
library(quantreg)
library(DT)


# without loss of generality parameters (fixed):
n_learners = 100
max_age = 48 # months
vocab_size = 10000
max_tokens_per_day = 6000 # maximum reasonable input rate

#source("generate_tokens.R") # generate all tokens at outset (takes 60-90s)
source("model.R")

theme_set(theme_classic())
# Define UI
ui <- fluidPage(
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word frequency distribution:",
                        list("Zipfian" = "zipf", 
                             "Uniform" = "uniform")),
            #sliderInput("vocab_size", "Total vocabulary size:", 
            #            min=500, max=10000, value=5000, step=500),
            #sliderInput("n_learners", "Number of learners:", 
            #            min=10, max=200, value=50, step=10),
            sliderInput("input_rate", "Effective learning instances per day:", 
                        min=10, max=300, value=180, step=10), 
            helpText("e.g., 1% of tokens per day (which may be 8000-28000, according to Hart & Risley)"),
            #helpText("e.g., Hart & Risley low SES: 616/hr; high SES: 2153/hr"),
            sliderInput("threshold", "Mean occurrences needed to learn a word:", 
                        min=0, max=100, value=40, step=5),
            #checkboxInput("threshold_varies", "Threshold varies (i.e., word difficulty normally distributed)"),
            sliderInput("threshold_sd", "Standard deviation of threshold distribution:", 
                        min=0, max=100, value=20, step=1),
            helpText("McMurray (2007) used a mean of 4000 and a large SD."),
            #sliderInput("max_age", "Age range (months):", 
            #            min=0, max=48, value=36, step=3),
            sliderInput("learning_rate", "Mean learning rate (scales value of occurrence; truncated at .1):", 
                        min = 1, max = 10, value = 3, step= 1),
            sliderInput("proc_speed_dev", "Mean processing speed rate of development:", 
                        min = 0, max = 1, value = 0.72, step= 0.02),
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs", 
                tabPanel("Vocabulary Growth by Age", plotOutput("ageVocab")),
                tabPanel("Processing Speed by Age", plotOutput("ageRT")),
                tabPanel("Vocabulary Growth Table", textOutput("summary"), DT::dataTableOutput("mytable"))
            )
        )
    )
)


# server logic
server <- function(input, output) {
    sim_data = reactive({
        # fix: vocab_size=10000, n_learners=100, max_age=48
        simulate(vocab_size, input$distro, input$input_rate, 
            n_learners, input$threshold, max_age, 
            input$learning_rate, input$threshold_sd, 
            input$proc_facilitates, input$proc_speed_dev)
    })
    
    output$ageVocab <- renderPlot({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$known_words, aes(x=month, y=words)) + 
            geom_line(aes(group = id), alpha = .1) + geom_smooth() + 
            # geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            # labs(colour="Quantile") + 
            xlab("Age (months)") + 
            ylab("Vocabulary Size") + 
            ylim(0, vocab_size) + xlim(1, max_age) 
            # geom_point(alpha=.1) +
            # geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) 
    })
    
    output$ageRT <- renderPlot({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$proc_speed, aes(x=month, y=words)) + geom_line(aes(group=id), alpha=.1) +  
            #geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            labs(colour="Quantile") + geom_smooth() + 
            #geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) + 
            xlab("Age (months)") + ylab("Response Time (seconds)") + 
            xlim(1, max_age) + ylim(0,2)
    })
    
    output$summary <- renderText({ 
        paste("Mean of cumulative words known per month.") # input$distro
    })
    
    output$mytable = DT::renderDataTable({
        sim_data()$known_words %>% group_by(month) %>% 
            summarise(mean=mean(words), sd=sd(words)) %>%
            mutate(cumulative_tokens=input$input_rate*30.42*month) %>%
            datatable(options = list(lengthMenu = c(12, 24, 36), pageLength=49)) %>% 
            formatRound(columns=c("mean","sd"), digits=0)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
