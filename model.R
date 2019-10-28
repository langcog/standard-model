sigmoid <- function(x) {
  return( 1 / (1 + exp(-x)) )
}

make_long_df <- function(df, n_learners, max_age, zeroval) {
  df = cbind(rep(zeroval,n_learners), df) # value at 0 mos
  df = data.frame(df)
  names(df) = 0:max_age
  df$id = 1:nrow(df)
  df_l = gather(df, "month", "words", 1:(max_age+1)) 
  df_l$month = as.numeric(as.character(df_l$month))
  return(df_l)
}

# need to add proc_speed term and proc_facilitates interaction term
simulate <- function(vocab_size, distro, input_rate, n_learners, threshold, max_age, mean_learning_rate, threshold_varies, proc_facilitates, proc_speed_dev) {
  learning_rate = rnorm(n_learners, mean=mean_learning_rate, sd=1) # individual learning rates
  learning_rate[which(learning_rate<0)] = 0.1 # >0
  if(distro=="zipf") {
    probs = 1 / (1:vocab_size + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
    probs = probs/sum(probs)
    probs = sample(probs, length(probs)) # shuffle
  } else if(distro=="uniform") {
    probs = rep(1/vocab_size, vocab_size)
  } 
  
  # fixed threshold for all words, or normal distribution over word difficulty (like McMurray 2007)
  if(threshold_varies) threshold = rnorm(vocab_size, mean=threshold, sd=1)
  
  cumulative_word_occs = matrix(0, nrow=n_learners, ncol=vocab_size) # number of times each word has appeared per learner
  known_words = matrix(0, nrow=n_learners, ncol=max_age) # known words per individual (row) per month (col)
  proc_speed = matrix(0, nrow=n_learners, ncol=max_age) # do we want per word instead of per learner?
  
  # processing speed
  # Kail (1991): RT slopes follow an exponential, such that Y(i) = a + b*exp(−c*i), where Y=predicted var, i=age. 
  a = 0.56 # eventual (adult) asymptote
  b = 1.04 # multiplier for the (infant) intercept
  c = proc_speed_dev #rate of development (0.72 in Frank, Lewis, & MacDonald, 2016)
  
  time_steps = round(30.42*max_age) # days/month * age
  # sample for all learners and time_steps at once
  #tokens = matrix(sample(1:vocab_size, input_rate*time_steps*n_learners, prob=probs, replace=T), nrow = n_learners)
  
  tokens_per_mo = round(input_rate*30.42) # tokens/day * days/month 
  for(t in 1:max_age) {
    # sample 1 month of tokens for all learners
    tokens = matrix(sample(1:vocab_size, tokens_per_mo*n_learners, prob=probs, replace=T), nrow=n_learners) 
    mo_word_occs = apply(tokens, 1, tabulate, nbins=vocab_size) # occurences of each word this month per subject (column)
    
    proc_speed[,t] = a + rowSums(b*exp(-c*log(cumulative_word_occs+1))) / vocab_size
    
    # learning rate scales value of occurrences
    if(proc_facilitates) { # further scale value of word occurrences by processing speed
      cumulative_word_occs = cumulative_word_occs + (3-proc_speed[,t])*learning_rate*t(mo_word_occs)
    } else {
      cumulative_word_occs = cumulative_word_occs + learning_rate*t(mo_word_occs) # accumulate occurrences this month
    }
    known_words[,t] = rowSums(cumulative_word_occs>threshold) 
  }
  
  # return known words and mean RT per individual per month 
  # reshape to long
  known_words_l = make_long_df(known_words, n_learners, max_age, 0)
  proc_speed_l = make_long_df(proc_speed, n_learners, max_age, a+exp(-c))
  return(list(known_words=known_words_l, proc_speed=proc_speed_l))
}

# vocab_size, distro, input_rate, n_learners, threshold, max_age, mean_learning_rate, threshold_varies, proc_facilitates, proc_speed_dev)
#sim = simulate(5000, "uniform", 500, 10, 15, 12, 1, F, F, .72)
#sim %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words))
