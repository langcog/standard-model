sigmoid <- function(x) {
  return( 1 / (1 + exp(-x)) )
}


# need to add proc_speed term and proc_facilitates interaction term
simulate <- function(vocab_size, distro, input_rate, n_learners, threshold, max_age, mean_learning_rate) {
  learning_rate = rnorm(n_learners, mean=mean_learning_rate, sd=1) # individual learning rates
  learning_rate[which(learning_rate<0)] = 0.1 # >0
  if(distro=="zipf") {
    # should shuffle these if we will add difficulty distribution
    probs = 1 / (1:vocab_size + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
    probs = probs/sum(probs)
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
    mo_word_occs = apply(tokens, 1, tabulate, nbins=vocab_size) # occurences of each word this month per subject (column)
    # mo_word_occs = mo_word_occs * learning_rate # learning rate scales value of occurrences
    cumulative_word_occs = cumulative_word_occs + learning_rate*t(mo_word_occs) # accumulate occurrences this month
    known_words[,t] = rowSums(cumulative_word_occs>threshold) 
  }
  # return per individual per month 
  # reshape to long
  known_words = cbind(rep(0,n_learners), known_words)
  known_words = data.frame(known_words)
  names(known_words) = 0:max_age
  known_words$id = 1:nrow(known_words)
  known_words_l = gather(known_words, "month", "words", 1:(max_age+1)) 
  known_words_l$month = as.numeric(as.character(known_words_l$month))
  return(known_words_l)
}

#sim = simulate(5000, "uniform", 500, 10, 15, 12, 1)
#sim %>% group_by(month) %>% summarise(mean=mean(words), sd=sd(words))
