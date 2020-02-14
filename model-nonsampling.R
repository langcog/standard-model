# without loss of generality parameters (fixed):
n_learners = 100
max_age = 48 # months
vocab_size = 10000
waking_hours_per_day = 12


sigmoid <- function(x) {
  return( 1 / (1 + exp(-x)) )
}

acceleration_test <- function(dat) {
  d = dat$known_words %>% filter(month>11 & month<25) %>% group_by(month) %>% summarize(mean=mean(words)) 
  return( mean(diff(diff(d$mean))) )
}

#library(profvis)
#profvis({

make_long_df <- function(df, n_learners, max_age) {
  #df = cbind(rep(zeroval,n_learners), df) # value at 0 mos
  df = data.frame(df)
  names(df) = 1:max_age
  df$id = 1:nrow(df)
  df_l = gather(df, "month", "words", 1:max_age) 
  df_l$month = as.numeric(as.character(df_l$month))
  return(df_l)
}


# eventually: what happens when you have a larger vocab (60k) but are only testing 500 (CDI sim)
# test for acceleration across all param values
# consolidate learning rate and threshold ?
#  variability in # effective instances to learn


# function(vocab_size=10000, distro, input_rate, input_rate_sd, n_learners=100, threshold, 
#  max_age=48, mean_learning_rate, learning_rate_sd, threshold_sd, 
#  proc_facilitates, proc_speed_dev, proc_speed_dev_sd) {

get_proc_speed <- function(print_plot=F) {
  a = rnorm(n_learners, mean=.56, sd=.1) # individual adult asymptotes for proc speed
  c = rnorm(n_learners, mean=.72, sd=.1)
  b = 1.04
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age)
  #proc_speed = apply(get_proc_speed)
  for(t in 1:max_age) {
    proc_speed[,t] = a + b*exp(-c*log(t+1)) #/ vocab_size 
  }

  if(print_plot) {
    proc_speed_long = make_long_df(proc_speed, n_learners, max_age)
    gg <- ggplot(proc_speed_long, aes(x=month, y=words)) + 
      geom_line(aes(group=id), alpha=.1) +  
      labs(colour="Quantile") + geom_smooth() + 
      xlab("Age (months)") + ylab("Response Time (seconds)") 
    print(gg)
  }
  
  return(proc_speed)
}

#get_proc_speed(print_plot=T)


# need to add proc_speed term and proc_facilitates interaction term
simulate <- function(parms) {
  #probs = wf_distros[[distro]] # "zipf" or "uniform"
  if(parms$distro=="uniform") {
    probs = rep(1/vocab_size, vocab_size)
  } else {
    probs = 1 / (1:vocab_size + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
    probs = probs/sum(probs)
    probs = sample(probs, length(probs)) 
  }
  if(parms$distro=="logzipf") {
    lp = -log(probs)
    probs = lp / sum(lp) # max=.001, min=.003 (2x min(zipf))
  }
  
  input_rate = rnorm(n_learners, mean=parms$input_rate, sd=parms$input_rate_sd) # per-child variability in input rate
  #learning_rate = rnorm(n_learners, mean=mean_learning_rate, sd=learning_rate_sd) # individual learning rates
  #learning_rate[which(learning_rate<.1)] = 0.1 # truncate
  a = rnorm(n_learners, mean=parms$mean_learning_rate, sd=parms$learning_rate_sd) # individual adult asymptotes for proc speed
  c = rnorm(n_learners, mean=parms$proc_speed_dev, sd=parms$proc_speed_dev_sd) # individual rate of processing speed development
  # changing learning_rate to be adult processing speed asymptote
  a[which(a<.01)] = 0.01 # truncate
  c[which(c<.01)] = 0.01 # truncate
  # maybe beta-distributed?
  
  # fixed threshold for all words, or normal distribution over word difficulty (like McMurray 2007)
  #if(threshold_varies) threshold = rnorm(vocab_size, mean=threshold, sd=20)
  threshold = rnorm(vocab_size, mean=parms$threshold, sd=parms$threshold_sd)
  
  cumulative_word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
  known_words = matrix(0, nrow=n_learners, ncol=max_age) # known words per individual (row) per month (col)
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age) # do we want per word instead of per learner?
  
  # processing speed - each of these parameters could have individual variability
  # Kail (1991): RT slopes follow an exponential, such that Y(i) = a + b*exp(−c*i), where Y=predicted var, i=age (mos? year?). 
  #a = 0.56 # eventual (adult) asymptote VARIABILITY HERE
  b = 1.04 # multiplier for the (infant) intercept - SES or preemie VARIABILITY HERE
  #c = proc_speed_dev #rate of development (0.72 in Frank, Lewis, & MacDonald, 2016)
  # age vs. # of vocab terms as leverage for learning rate
  # how to model clinically-important variables, e.g., they tell parents to follow-in (e.g., trains not animals for ASD)
  # can we get a true late-talker by 1) screwing around with input factors, and/or 2) screwing around with child-level variables

  tokens_per_mo = round(input_rate*waking_hours_per_day*30.42) # tokens/hour * waking hours/day * days/month)
  for(t in 1:max_age) {
    # expected occurences of each word this month per subject (column)
    mo_word_occs = probs*tokens_per_mo # no sampling -- just expected tokens per mo
    mo_word_occs = matrix(rep(mo_word_occs, n_learners), byrow=F, ncol=n_learners)
    
    #proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size 
    #proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size 
    proc_speed[,t] = a + b*exp(-c*log(t+1)) #/ vocab_size 
    
    # learning rate scales value of occurrences
    if(parms$proc_facilitates) { # further scale value of word occurrences by processing speed
      #cumulative_word_occs = cumulative_word_occs + (3-proc_speed[,t])*learning_rate*t(mo_word_occs) 
      cumulative_word_occs = cumulative_word_occs + (1/proc_speed[,t])*t(mo_word_occs) 
    } else {
      #cumulative_word_occs = cumulative_word_occs + learning_rate*t(mo_word_occs) # accumulate occurrences this month
      cumulative_word_occs = cumulative_word_occs + 1*t(mo_word_occs)
    }
    known_words[,t] = rowSums(cumulative_word_occs>threshold) 
  }
  
  # return known words and mean RT per individual per month 
  # reshape to long
  known_words_l = make_long_df(known_words, n_learners, max_age)
  proc_speed_l = make_long_df(proc_speed, n_learners, max_age)
  return(list(known_words=known_words_l, proc_speed=proc_speed_l))
}


# vocab_size, distro, input_rate, n_learners, threshold, max_age, mean_learning_rate, threshold_sd, proc_facilitates, proc_speed_dev)
#sim = simulate(10000, "uniform", 1000, 100, 100, 48, 2, .5, 10, F, .72)
#sim %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words))

#}) # profiler

ex_parms = list(distro="uniform",
             input_rate = 1000,
             input_rate_sd = 100,
             threshold = 1000,
             threshold_sd = 100,
             mean_learning_rate = .5,
             learning_rate_sd = .1,
             proc_facilitates = T,
             proc_speed_dev = .72, 
             proc_speed_dev_sd = .1
  )

simulate(ex_parms)
