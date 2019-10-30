
sigmoid <- function(x) {
  return( 1 / (1 + exp(-x)) )
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

# load generated tokens

# need to add proc_speed term and proc_facilitates interaction term
simulate <- function(vocab_size=10000, distro, input_rate, n_learners=100, threshold, max_age=48, mean_learning_rate, threshold_sd, proc_facilitates, proc_speed_dev) {
  #probs = wf_distros[[distro]] # "zipf" or "uniform"
  if(distro=="uniform") {
    probs = rep(1/vocab_size, vocab_size)
  } else {
    probs = 1 / (1:vocab_size + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
    probs = probs/sum(probs)
  }
  
  learning_rate = rnorm(n_learners, mean=mean_learning_rate, sd=1) # individual learning rates
  learning_rate[which(learning_rate<0)] = 0.1 # >0
  
  # fixed threshold for all words, or normal distribution over word difficulty (like McMurray 2007)
  #if(threshold_varies) threshold = rnorm(vocab_size, mean=threshold, sd=20)
  threshold = rnorm(vocab_size, mean=threshold, sd=threshold_sd)
  
  cumulative_word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
  known_words = matrix(0, nrow=n_learners, ncol=max_age) # known words per individual (row) per month (col)
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age) # do we want per word instead of per learner?
  
  # processing speed
  # Kail (1991): RT slopes follow an exponential, such that Y(i) = a + b*exp(−c*i), where Y=predicted var, i=age. 
  a = 0.56 # eventual (adult) asymptote
  b = 1.04 # multiplier for the (infant) intercept
  c = proc_speed_dev #rate of development (0.72 in Frank, Lewis, & MacDonald, 2016)
  
  tokens_per_mo = round(input_rate*30.42)*n_learners # tokens/day * days/month 
  tokens = sample(1:vocab_size, tokens_per_mo*48, prob=probs, replace=T)
  for(t in 1:max_age) {
    # sample 1 month of tokens for all learners
    #tokens_mo = matrix(sample(1:vocab_size, tokens_per_mo, prob=probs, replace=T), nrow=n_learners) # 3340ms
    tok_i = (t-1)*tokens_per_mo + 1 # for t=1, 1:tokens_per_mo, for t=2, tokens_per_mo
    #tokens_mo = matrix(tokens[[distro]][tok_i:(t*tokens_per_mo)], nrow=n_learners) # 500ms
    tokens_mo = matrix(tokens[tok_i:(t*tokens_per_mo)], nrow=n_learners) # 500ms
    mo_word_occs = apply(tokens_mo, 1, tabulate, nbins=vocab_size) # 1020ms
    # occurences of each word this month per subject (column)
    
    proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size # 930ms
    
    # learning rate scales value of occurrences
    if(proc_facilitates) { # further scale value of word occurrences by processing speed
      cumulative_word_occs = cumulative_word_occs + (3-proc_speed[,t])*learning_rate*t(mo_word_occs) # 380ms
    } else {
      cumulative_word_occs = cumulative_word_occs + learning_rate*t(mo_word_occs) # 360ms accumulate occurrences this month
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
sim = simulate(10000, "uniform", 1000, 100, 100, 48, 2, 10, F, .72)
#sim %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words))

#}) # profiler
