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
    titlePanel("Standard Model of Early Word Learning"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word difficulty distribution:",
                        list("Zipfian" = "zipf", 
                             "Gaussian" = "gaussian", 
                             "Uniform" = "uniform")),
            sliderInput("vocab_size", "Vocabulary size:", 
                        min=100, max=10000, value=1000, step=100),
            sliderInput("n_learners", "Number of learners:", 
                        min=1, max=1000, value=10),
            sliderInput("input_rate", "Input rate (words/day):", 
                        min=0, max=10000, value=1000),
            sliderInput("ELI_thresh", "Mean effective learning instances needed to learn:", 
                        min=1, max=100, value=10),
            sliderInput("max_age", "Age range (months):", 
                        min=0, max=48, value=12),
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
simulate <- function(vocab_size, distro, input_rate, n_learners, ELI_thresh, max_age) {
    if(distro=="zipf") {
        probs = 1:vocab_size / sum(1:vocab_size)
    } else if(distro=="uniform") {
        probs = rep(1/vocab_size, vocab_size)
    }
    
    word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
    known_words = matrix(0, nrow=n_learners, ncol=max_age) # 
    
    time_steps = round(30.42*max_age) # days/month * age
    # sample for all learners and time_steps at once
    #tokens = matrix(sample(1:vocab_size, input_rate*time_steps*n_learners, prob=probs, replace=T), nrow = n_learners)
    
    tokens_per_mo = round(input_rate*30.42) # tokens/day * days/month 
    for(t in 1:max_age) {
        # sample 1 month of tokens for all learners
        tokens = matrix(sample(1:vocab_size, tokens_per_mo*n_learners, prob=probs, replace=T), nrow=n_learners) 
        mo_word_occs = apply(tokens, 1, table)
        word_occs = 
    }
    # return per individual per month 
    # reshape to long
    return(known_words)
}

# server logic
server <- function(input, output) {
    output$ageVocab <- renderPlot({
        age <- 0:input$max_age
        sim = simulate(input$vocab_size, input$distro, input_rate, n_learners, ELI_thresh, max_age)
        # gd <- sim %>% group_by(age, subject) %>% summarise(mean=mean(learned), sd=sd(learned))
        vocab_traj <- 0:input$max_age # call 
        plot(age, vocab_traj, xlab="Age (months)", ylab="Vocabulary")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
