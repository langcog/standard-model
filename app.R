#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
require(tidyverse)

# Define UI
ui <- fluidPage(
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word difficulty distribution:",
                        list("Zipfian" = "zipf", 
                             "Uniform" = "uniform")),
            sliderInput("vocab_size", "Vocabulary size:", 
                        min=100, max=10000, value=2000, step=200),
            sliderInput("n_learners", "Number of learners:", 
                        min=1, max=1000, value=100, step=50),
            sliderInput("input_rate", "Input rate (words/day):", 
                        min=0, max=10000, value=1000, step=100),
            sliderInput("threshold", "Number of occurrences needed to learn a word:", 
                        min=0, max=1000, value=100, step=10),
            sliderInput("max_age", "Age range (months):", 
                        min=0, max=48, value=24, step=3),
            sliderInput("learning_rate", "Mean learning rate:", 
                        min = 0, max = 1, value = 0.5, step= 0.1),
            sliderInput("proc_speed", "Mean processing speed:", 
                        min = 0, max = 1, value = 0.5, step= 0.1),
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ageVocab")
        )
    )
)

# need to add proc_speed term and proc_facilitates interaction term
#simulate(1000, "zipf", 100, 10, 15, 12)
simulate <- function(vocab_size, distro, input_rate, n_learners, threshold, max_age) {
    # could add learning_rate = rnorm(n_learners, mean_learning_rate) # individual learning rates
    if(distro=="zipf") {
        probs = 1:vocab_size / sum(1:vocab_size)
    } else if(distro=="uniform") {
        probs = rep(1/vocab_size, vocab_size)
    } 
    
    cumulative_word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
    known_words = matrix(0, nrow=n_learners, ncol=max_age) # known words per individual (row) per month (col)
    
    time_steps = round(30.42*max_age) # days/month * age
    # sample for all learners and time_steps at once
    #tokens = matrix(sample(1:vocab_size, input_rate*time_steps*n_learners, prob=probs, replace=T), nrow = n_learners)
    
    tokens_per_mo = round(input_rate*30.42) # tokens/day * days/month 
    for(t in 1:max_age) {
        # sample 1 month of tokens for all learners
        tokens = matrix(sample(1:vocab_size, tokens_per_mo*n_learners, prob=probs, replace=T), nrow=n_learners) 
        mo_word_occs = apply(tokens, 1, tabulate) # occurences of each word this month per subject (column)
        # mo_word_occs = mo_word_occs * learning_rate # learning rate scales value of occurrences
        cumulative_word_occs = cumulative_word_occs + t(mo_word_occs) # accumulate occurrences this month
        known_words[,t] = rowSums(cumulative_word_occs>threshold) # occasionally fails?
    }
    # return per individual per month 
    # reshape to long
    known_words = data.frame(known_words)
    names(known_words) = 1:max_age
    known_words$id = 1:nrow(known_words)
    known_words_l = gather(known_words, "month", "words", 1:max_age) 
    known_words_l$month = as.numeric(as.character(known_words_l$month))
    return(known_words_l)
}


# server logic
server <- function(input, output) {
    output$ageVocab <- renderPlot({
        age <- 0:input$max_age
        sim = simulate(input$vocab_size, input$distro, input$input_rate, input$n_learners, input$threshold, input$max_age)
        #sim = simulate(1000, "zipf", 100, 10, 15, 12)
        gd <- sim %>% group_by(month, id) %>% summarise(mean=mean(words), sd=sd(words))
        
        ggplot(sim, aes(x=month, y=words)) + geom_point(alpha=.7) + geom_smooth() + # position="jitter", 
            geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) + 
            xlab("Age (months") + ylab("Vocabulary Size") + theme_bw() + 
            ylim(0,input$vocab_size) + xlim(0, input$max_age) 
    })
}

# Run the app
shinyApp(ui = ui, server = server)
