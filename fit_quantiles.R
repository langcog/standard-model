# fit wordbank quantile data
require(tidyverse)
require(DEoptim)

source("model-nonsampling.R")
load("wordbank_quantiles.Rdata") # quantiles from wordbank EN WG+WS productive vocab

max_age = 30 # months
n_learners = 500

qs <- c(0.10,0.25,0.50,0.75,0.90)

ex_parms = list(distro="uniform",
                input_rate = 1000,
                input_rate_sd = 100,
                threshold = 1000,
                threshold_sd = 100,
                mean_learning_rate = .5, # proc_speed_asymp
                learning_rate_sd = .1,
                proc_facilitates = T,
                proc_speed_dev = .72, 
                proc_speed_dev_sd = .1
)


fitSSE <- function(parms, proc_facilitates=T, graph="") {
  full_parms = list(distro="logzipf",
                  input_rate = parms[1],
                  input_rate_sd = parms[2],
                  threshold = parms[3],
                  threshold_sd = parms[4],
                  mean_learning_rate = parms[5], # proc_speed_asymp
                  learning_rate_sd = parms[6],
                  proc_facilitates = proc_facilitates,
                  proc_speed_dev = parms[7], 
                  proc_speed_dev_sd = parms[8]
  )
  simdat = simulate(full_parms)$known_words
  sim_quants <- simdat %>%
    group_by(month) %>%
    group_modify(~{
      quantile(.x$words, probs=qs) %>%
        tibble::enframe()
    }) %>% spread(name, value)
  
  if(graph!="") {
    theme_set(theme_classic())
    ggplot(simdat, aes(x=month, y=words)) + geom_line(aes(group=id), alpha=.05) +  
      geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
      labs(colour="Quantile") + xlab("Age (months)") + ylab("Vocabulary Size") + xlim(1, max_age) 
    ggsave(paste0(graph,".pdf"), width=6, height=5)
  }
  
  return(sum((sim_quants[8:max_age,] - quants)^2))
}


#set.seed(1234)
#fit <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .01, .01, .01, .01), upper=c(9000, 9000, 9000, 9000, 1, 2, 1, 2), 
#               control = DEoptim.control(NP = 80, itermax = 200)) # , F = 1.2, CR = 0.7
# 256282.537500 bestmemit:  
#pars = c(377.67, 2091.39, 8034.29, 2638.25, 0.402713, 0.015136, 0.627340, 0.109277)
#fitSSE(pars, graph=T) # SSE= 265834
#fitSSE(fit$optim$bestmem, graph="unconstrained_proc_facil_TRUE") 


#fit2 <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .555, .01, .715, .01), upper=c(9000, 9000, 9000, 9000, .564, 2, .724, 2), 
#                control = DEoptim.control(NP = 80, itermax = 200))
# SSE=254416.43
#pars2 = c(111.33, 2886.68, 7965.46, 2340.04, 0.56, 0.011, 0.716244, 0.129518)
#fitSSE(pars2, graph=T) # 224546.4
#fitSSE(fit2$optim$bestmem, graph="constrained_proc_facil_TRUE")