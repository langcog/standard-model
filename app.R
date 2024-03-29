
library(shiny)
library(tidyverse)
library(quantreg)
library(DT)
library(shinyWidgets)
library(shinythemes)

#source("model-nonsampling.R")
source("model-childesfreqs.R")

theme_set(theme_classic(base_size=16))
# Define UI
ui <- fluidPage(
    theme = shinytheme("spacelab"),
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word frequency distribution:",
                        list("log(Zipfian)" = "logzipf",
                             "Uniform" = "uniform",
                             "Zipfian" = "zipf")),
            chooseSliderSkin("Modern"),
            setSliderColor(c("Black", # start_age
                             "DarkSlateGrey", "DarkSlateGrey", # Input
                             "DeepSkyBlue", "DeepSkyBlue", # Word Threshold
                             "Green","Green", #axes
                             "DeepPink", "DeepPink", # Proc Speed Adult Asymptote
                             "DarkRed", "DarkRed", # Proc Speed Rate of Development
                             "Green", "Green",
                             "Green", "Green",
                             "Green", "Green"), 
                           c(1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13, 14,15, 16,17)), # color code param means and SDs
            sliderInput("start_age", "Age (mos) when words start accumulating (e.g., age to segmentation):", 
                        min=1, max=8, value=1, step=1), 
            # best-fitting parms for start_age=1 logzipf: c(204, 2436, 6937, 2127, 0.56, 0.03, 0.72, 0.21)
            sliderInput("input_rate", "Input rate mean (tokens/hour):", 
                        min=0, max=2500, value=1000, step=50), 
            helpText("e.g., Hart & Risley low SES: 616/hr; high SES: 2153/hr; we assume 12 waking hours/day"),
            sliderInput("input_rate_sd", "Input rate standard deviation (SD):", 
                        min=0, max=3000, value=900, step=100), 
            helpText("Gilkerson et al. 2017 daily SD range: 4,100-8,200"),
            
            sliderInput("threshold", "Threshold mean (occurrences needed to learn a word):", 
                        min=100, max=7000, value=4000, step=100),
            sliderInput("threshold_sd", "Threshold standard deviation:", 
                        min=0, max=4000, value=1000, step=100),
            helpText("McMurray (2007) used a mean of 4000 and a large SD."),
            
            #sliderInput("learning_rate", "Mean learning rate (scales value of occurrence; truncated at .1):", 
            #            min = .5, max = 10, value = 1, step=.5),
            
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs", 
                tabPanel("Vocabulary Growth", 
                         h3("Overview"),
                         textOutput("overview"),
                         br(),
                         textOutput("rgGrowth"),
                         br(),
                         downloadButton('downloadPlotvocabGrowth', "Download Plot"),
                         br(),
                         plotOutput("ageVocab", click = "plot_click"), 
                         br(),
                         textOutput("acceleration"),
                         br(),
                        # verbatimTextOutput("info"),
                         # flexible axes
                         fluidRow(
                             column(6,
                                    sliderInput("x_range", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                             column(6,
                                    sliderInput("y_range", "Vocabulary Size",
                                                min=0, max = 8000, value = c(0,8000), step = 1000))
                         ),
                         br()
                    ),
                tabPanel("Processing Speed", 
                         textOutput("rgSpeed"),
                         br(),
                         downloadButton('downloadPlotprocspeed', "Download Plot"),
                         br(),
                         fluidRow(
                             column(3,
                                    sliderInput("proc_speed_asymp", 
                                                div(HTML("Adult processing speed asymptote mean (<em>a</em>; scales value of occurrence; truncated at .01):")), 
                                                min = .01, max = 1, value = .56, step=.01)
                             ), 
                             column(3,
                                    sliderInput("proc_speed_asymp_sd", div(HTML("Adult processing speed <em>a</em> SD:")), 
                                                min = 0, max = 1, value = .1, step=.01)
                             ), 
                             column(3,
                                    sliderInput("proc_speed_dev", "Processing speed rate of development mean (c):", 
                                                min = 0, max = 1, value = 0.72, step= 0.02)
                             ), 
                             column(3,
                                    sliderInput("proc_speed_dev_sd", 
                                                min = 0, max = 1, value = .1, step=.01, "Processing speed rate SD:")
                             )
                         ),
                         plotOutput("ageRT"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range1", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                             column(6,
                                    sliderInput("y_range1", "Response time",
                                                min=0, max = 1.5, value = c(0,1.5), step = 0.1))
                         ),
                         br()
                    ),
                tabPanel("Vocabulary Growth Table", 
                         textOutput("summary"), 
                         br(),
                         downloadButton("download_table", "Download Table",
                                        class = "btn-default btn-xs"),
                         br(),
                         br(),
                         DT::dataTableOutput("mytable"),
                         br()
                    ),
                tabPanel("Growth per Word",
                         textOutput("rgWords"),
                         br(),
                         downloadButton('downloadPlotageWord', "Download Plot"),
                         br(),
                         selectInput("selectword", "Select a word",
                                     cdi_list,
                                     multiple = TRUE),
                         plotOutput("ageWord"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range3", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                             column(6,
                                    sliderInput("y_range3", "Proportion of learners knowing words",
                                                min=0, max = 1, value = c(0,1), step = 0.1))
                         ),
                         br()), # add selector(s) to show particular words
                tabPanel("Part of Speech I",
                         textOutput("rgPoSI"),
                         br(),
                         plotOutput("agePos"),
                         br(),
                         plotOutput("propPos"),
                         br()),
                tabPanel("Part of Speech II",
                         textOutput("rgPoSII"),
                         br(),
                         downloadButton('downloadPlotpos2', "Download Plot"),
                         br(),
                         plotOutput("avgPoS"),
                         br()
                ),
                tabPanel("CDI vs. Full Vocab",
                         textOutput("rgCDIvsTot"),
                         br(),
                         downloadButton('downloadPlotCDIfull', "Download Plot"),
                         br(),
                         plotOutput("cdi_vs_full"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range2", "CDI words known",
                                                min=0, max = 700, value = c(0,700), step = 200)),
                             column(6,
                                    sliderInput("y_range2", "Total vocabulary",
                                                min=0, max = 8000, value = c(0,8000), step = 2000))
                         ),
                         br() # also show correlation of CDI and full, and mean words outside of CDI known (per age?)
                         )
            )
        )
    )
)


# server logic
server <- function(input, output) {
    print(input)
    sim_data <- reactive({
        # fix: vocab_size=10000, n_learners=100, max_age=48
        parms = list(distro=input$distro,
                     input_rate = input$input_rate,
                     input_rate_sd = input$input_rate_sd,
                     threshold = input$threshold,
                     threshold_sd = input$threshold_sd,
                     mean_learning_rate = input$proc_speed_asymp, # ToDo: re-name param in model-nonsampling.R
                     learning_rate_sd = input$proc_speed_asymp_sd, # ToDo: re-name param in model-nonsampling.R
                     proc_facilitates = input$proc_facilitates,
                     proc_speed_dev = input$proc_speed_dev, 
                     proc_speed_dev_sd = input$proc_speed_dev_sd,
                     start_age = input$start_age
                )
        print(parms)
        simulate(parms)
        #simulate(vocab_size=vocab_size, distro=input$distro, input_rate=input$input_rate, input_rate_sd=input$input_rate_sd,
        #    n_learners=n_learners, threshold=input$threshold, max_age=max_age, 
        #    mean_learning_rate=input$learning_rate, learning_rate_sd=input$learning_rate_sd, threshold_sd=input$threshold_sd, 
        #    proc_faciliates=input$proc_facilitates, proc_speed_dev=input$proc_speed_dev, proc_speed_dev_sd=input$proc_speed_dev_sd)
    })
    
    plotageVocab <- reactive({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$known_words, aes(x=month, y=words)) + 
            geom_line(aes(group = id), alpha = .1) + 
            geom_smooth(aes(x=month, y=words), color="black") + 
            # geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            # labs(colour="Quantile") + 
            xlab("Age (months)") + 
            ylab("Vocabulary Size") + #ylim(0, vocab_size) +
            xlim(input$x_range[1], input$x_range[2]) +
            ylim(input$y_range[1], input$y_range[2]) +
            geom_line(aes(x = month, y = cdi_words, group = id), alpha=.1, color = "red") +
            geom_smooth(aes(x = month, y = cdi_words), color="red")
            # geom_point(alpha=.1) +
            # geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) 
    })
    
    output$ageVocab <- renderPlot({
        print(plotageVocab())
    })
    
    output$downloadPlotvocabGrowth <- downloadHandler(
        filename = "vocabGrowth.png",
        content = function(file){
            png(file = file)
            print(plotageVocab())
            dev.off()
        }
    )
    
    output$info <- renderText({
        paste0("x = ", input$plot_click$x, " months", "\ny = ",  input$plot_click$y, " words")
    })
    
    output$rgSpeed <- renderText({ 
        paste("This plot shows the trajectory of processing speed (seconds) as a function of age. Each individual’s variation from the conditional mean (the dark blue line) may be observed from the plot.") 
    })
    
    output$overview <- renderText({ 
        paste("This interactive webpage simulates the early word learning of one hundred children.",
              "The simulations use a simple accumulator model of input, with the hourly input rate",
              "experienced per child drawn from a normal distribution with mean and standard deviation chosen by the user (at left).",
              "The start age of word accumulation is assumed to be the same for all children,",
              "and the average waking hours is assumed to be 12 hours per day. By adjusting",
              "the parameters, users may observe the growth curves of vocabulary and the",
              "trajectories of each child. The words and the word frequency of each word",
              "are provided by the CHILDES dataset.") 
    })
    
    output$rgWords <- renderText({ 
        paste("This plot shows the growth curve of any selected word.",
              "Users may select any word included in the CDI dataset.") # we can add all CHILDES words
    })
    
    output$rgCDIvsTot <- renderText({ 
        paste("This plot depicts the relationship between known CDI words and total vocabulary.", 
              "As the CDI Words & Sentences checklist only includes 656 words,",
              "the number of known CDI words may no longer be reflective of children's",
              "total vocabulary in children close to 48 months of age.")
    })
    
    output$rgGrowth <- renderText({ 
        paste("The plot below shows simulated vocabulary growth as a function of age:",
              "red lines denote growth curves for words on the CDI,",
              "and black lines show simulated vocabulary growth of the most",
              "frequent 10,179 words in the CHILDES corpus.")
    })
    
    output$rgPoSII <- renderText({ 
        paste("This plot depicts the composition (adjective, noun, verb, and other) of children’s vocabulary. Among these four lexical categories, nouns are the largest constituent of children’s vocabulary.")
    })
    
    output$rgPoSI <- renderText({ 
        paste("The first plot depicts the growth curves of these four lexical categories (adjective, noun, verb, and other) separately. Note that each individual is a line in the first plot. The second plot is proportional: the proportion of words known to each type as a function of the proportion of total words known. Note that each individual is a dot in the second plot.")
    })
    
    # three plots related to lexical categories
   plotavgPoS <- reactive({
        ggplot(sim_data()$known_PoS, aes(x=Category, y=words, fill = PoS)) + 
            geom_bar(stat="identity") + 
            xlab("Age (months)") + 
            ylab("Mean Vocabulary Size") 
    })
    
    output$avgPoS <- renderPlot({
        print(plotavgPoS())
    })
    
    output$downloadPlotpos2 <- downloadHandler(
        filename = "lexicalCategories.png",
        content = function(file){
            png(file = file)
            print(plotavgPoS())
            dev.off()
        }
    )
    
    output$propPos <- renderPlot({
        ggplot(sim_data()$known_pos, aes(x=Proportion, y=words/twords)) + 
            xlab("Vocabulary Size") + ylab("Proportion of category") +
            facet_wrap(~ PoS) +
            geom_point(aes(group=id), alpha=.1) + geom_smooth() 
    })
    
    output$agePos <- renderPlot({
        ggplot(sim_data()$known_pos, aes(x=month, y=words)) + 
            xlab("Months") + ylab("Vocabulary Size") +
            facet_wrap(~ PoS) +
            geom_line(aes(group=id), alpha=.1) + geom_smooth() 
    })
    
    # show proportion of learners knowing each word over time
    # (could also create a table of mean AoA per word)
    plotAgeWord <- reactive({
        # maybe make this dataframe in model code..
        dw <- sim_data()$prop_knowing_word # need to make this long
        dl = data.frame(dw)
        names(dl) = 1:max_age
        dl$word = rownames(dw)
        dl = gather(dl, "month", "prop_know", 1:max_age) 
        dl$month = as.numeric(as.character(dl$month))
        dl$on_cdi = wf$on_cdi
        
        # select a small number of words..use selectizeInput ?
        #plot_words = c("you", "the", "have", "wanna", "mommy", 
                       #"daddy", "book", "dog", "boy", "baby")
        plot_word = c(input$selectword)
        ggplot(subset(dl, is.element(word, plot_word)), 
               aes(x=month, y=prop_know)) + 
            geom_line(aes(group = word, color=word), alpha = .8) + 
            #geom_smooth(aes(x=month, y=words), color="black") + 
            xlab("Age (months)") + 
            ylab("Proportion of Learners Knowing Word") +
            xlim(input$x_range3[1], input$x_range3[2]) +
            ylim(input$y_range3[1], input$y_range3[2]) 
    })
    
    output$ageWord <- renderPlot({
        print(plotAgeWord())
    })
    
    output$downloadPlotageWord <- downloadHandler(
        filename = "ageword.png",
        content = function(file){
            png(file = file)
            print(plotAgeWord())
            dev.off()
        }
    )
    
   plotcdiFull <- reactive({
        dat <- sim_data()$known_words
        dat$age_group = cut_interval(dat$month, 16)
        ggplot(dat, aes(x=cdi_words, y=words)) + 
            xlab("CDI Words Known") + ylab("Total Vocabulary") +
            #facet_wrap(~ age_group, nrow=4) +
            geom_line(aes(group=id), alpha=.1) + geom_smooth() + # , color=age_group
            xlim(input$x_range2[1], input$x_range2[2]) +
            ylim(input$y_range2[1], input$y_range2[2])
    })
    
    output$cdi_vs_full <- renderPlot({
        print(plotcdiFull())
    })
    
    output$downloadPlotCDIfull <- downloadHandler(
        filename = "CDIfull.png",
        content = function(file){
            png(file = file)
            print(plotcdiFull())
            dev.off()
        }
    )
    
    plotageRT <- reactive({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$proc_speed, aes(x=month, y=words)) + geom_line(aes(group=id), alpha=.1) +  
            #geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            labs(colour="Quantile") + geom_smooth() + 
            #geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) + 
            xlab("Age (months)") + ylab("Response Time (seconds)") + 
            xlim(input$x_range1[1], input$x_range1[2]) +
            ylim(input$y_range1[1], input$y_range1[2]) 
            #+ ylim(0,1.7)
        #print(sim_data()$proc_speed)
    })
    
    output$ageRT <- renderPlot({
        print(plotageRT())
    })
    
    output$downloadPlotprocspeed <- downloadHandler(
        filename = "processingSpeed.png",
        content = function(file){
            png(file = file)
            print(plotageRT())
            dev.off()
        }
    )
    
    output$summary <- renderText({ 
        paste("Mean of cumulative words known per month.") # input$distro
    })
    
    # fit linear and quadratic models to mean words known per month for all months where vocab is not at ceiling
    # NOT DONE - do we just want to check the r.squared of a model with a quadratic term? or 
    output$acceleration <- renderText({ 
        accel = acceleration_test(sim_data()) 
        paste("Average acceleration in vocabulary growth during the second year: ", round(accel, 2)) # input$distro
    })
    
    # vocabulary growth table
    output$mytable = DT::renderDataTable({
        sim_data()$known_words %>% group_by(month) %>% 
            summarise(mean=mean(words), sd=sd(words)) %>% 
            mutate(cumulative_tokens=input$input_rate*waking_hours_per_day*30.42*month) %>%
            datatable(options = list(lengthMenu = c(12, 24, 36), pageLength=49)) %>% 
            formatRound(columns=c("mean","sd"), digits=0)
    })
    
    # download button
    output$download_table <- downloadHandler(
        filename = function() paste0("standard_model_sim", ".csv"), # maybe a version number?
        content = function(file) {
            voc_mo <- sim_data()$known_words %>% group_by(month) %>% 
                summarise(mean=mean(words), sd=sd(words)) %>% 
                mutate(cumulative_tokens=input$input_rate*waking_hours_per_day*30.42*month)
            write.csv(voc_mo, file, row.names = FALSE)
        })
}

# Run the app
shinyApp(ui = ui, server = server)
