
library(shiny)
library(tidyverse)
library(quantreg)
library(DT)


source("model-nonsampling.R")

theme_set(theme_classic())
# Define UI
ui <- fluidPage(
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word frequency distribution:",
                        list("Zipfian" = "zipf",
                             "log(Zipfian)" = "logzipf",
                             "Uniform" = "uniform")),
            sliderInput("input_rate", "Input rate (tokens/hour):", 
                        min=100, max=6000, value=1000, step=100), 
            sliderInput("input_rate_sd", "Standard deviation of input rate:", 
                        min=0, max=1000, value=100, step=10), 
            helpText("e.g., Hart & Risley low SES: 616/hr; high SES: 2153/hr, and let's assume 12 waking hours per day"),
            sliderInput("threshold", "Mean occurrences needed to learn a word:", 
                        min=100, max=6000, value=1000, step=100),
            sliderInput("threshold_sd", "Standard deviation of threshold distribution:", 
                        min=0, max=1000, value=300, step=20),
            helpText("McMurray (2007) used a mean of 4000 and a large SD."),
            #sliderInput("learning_rate", "Mean learning rate (scales value of occurrence; truncated at .1):", 
            #            min = .5, max = 10, value = 1, step=.5),
            sliderInput("learning_rate", "Mean adult processing speed asymptote (a; scales value of occurrence; truncated at .01):", 
                        min = .01, max = 1, value = .56, step=.01),
            sliderInput("learning_rate_sd", "Standard deviation of a distribution:", 
                        min = 0, max = 1, value = .1, step=.01),
            sliderInput("proc_speed_dev", "Mean processing speed rate of development (c):", 
                        min = 0, max = 1, value = 0.72, step= 0.02),
            sliderInput("proc_speed_dev_sd", "Standard deviation of c distribution:",
                        min = 0, max = 1, value = .1, step=.01),
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs", 
                tabPanel("Vocabulary Growth by Age", plotOutput("ageVocab"), textOutput("acceleration")),
                tabPanel("Processing Speed by Age", plotOutput("ageRT")),
                tabPanel("Vocabulary Growth Table", textOutput("summary"), DT::dataTableOutput("mytable"))
            )
        )
    )
)


# server logic
server <- function(input, output) {
    print(input)
    sim_data = reactive({
        # fix: vocab_size=10000, n_learners=100, max_age=48
        parms = list(distro=input$distro,
                     input_rate = input$input_rate,
                     input_rate_sd = input$input_rate_sd,
                     threshold = input$threshold,
                     threshold_sd = input$threshold_sd,
                     mean_learning_rate = input$learning_rate,
                     learning_rate_sd = input$learning_rate_sd,
                     proc_facilitates = input$proc_facilitates,
                     proc_speed_dev = input$proc_speed_dev, 
                     proc_speed_dev_sd = input$proc_speed_dev_sd
                )
        print(parms)
        simulate(parms)
        #simulate(vocab_size=vocab_size, distro=input$distro, input_rate=input$input_rate, input_rate_sd=input$input_rate_sd,
        #    n_learners=n_learners, threshold=input$threshold, max_age=max_age, 
        #    mean_learning_rate=input$learning_rate, learning_rate_sd=input$learning_rate_sd, threshold_sd=input$threshold_sd, 
        #    proc_faciliates=input$proc_facilitates, proc_speed_dev=input$proc_speed_dev, proc_speed_dev_sd=input$proc_speed_dev_sd)
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
    
    # fit linear and quadratic models to mean words known per month for all months where vocab is not at ceiling
    # NOT DONE - do we just want to check the r.squared of a model with a quadratic term? or 
    output$acceleration <- renderText({ 
        accel = acceleration_test(sim_data()) 
        paste("Average acceleration in vocabulary growth during the second year: ", round(accel, 2)) # input$distro
    })
    
    output$mytable = DT::renderDataTable({
        sim_data()$known_words %>% group_by(month) %>% 
            summarise(mean=mean(words), sd=sd(words)) %>% 
            mutate(cumulative_tokens=input$input_rate*waking_hours_per_day*30.42*month) %>%
            datatable(options = list(lengthMenu = c(12, 24, 36), pageLength=49)) %>% 
            formatRound(columns=c("mean","sd"), digits=0)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
