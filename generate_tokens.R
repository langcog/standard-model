# run once to generate word frequency distributions and sample tokens

# tokens for one learner: months * days * waking hrs * tokens/hr (600 - 2200)
48 * 30.42 * 13 * 1000 # 18,982,080 

# maybe give up on raw tokens and call them Effective Learning Instances,
# assume they crop up once every hundred tokens..

# without loss of generality parameters (fixed):
n_learners = 100
max_age = 48 # months
vocab_size = 10000
max_tokens_per_day = 6000 # maximum reasonable input rate
# 4000 takes ~60s on startup
set.seed(123)

wf_distros = list()
zprobs = 1 / (1:vocab_size + 2.7) # f(r) = 1 / (r+beta)^alpha, alpha=1, beta=2.7 (Mandelbrot, 1953, 1962)
zprobs = zprobs/sum(zprobs)
wf_distros[["zipf"]] = sample(zprobs, length(zprobs)) # shuffle
wf_distros[["uniform"]] = rep(1/vocab_size, vocab_size)
# takes 2 ms

tokens = list()
tot_tokens = round(max_tokens_per_day*30.42)*n_learners*max_age 
# maximum 6000 tokens/day, totaling 876m over 48mos
start_time <- Sys.time()
tokens[["zipf"]] = matrix(sample(1:vocab_size, tot_tokens, prob=wf_distros[["zipf"]], replace=T), nrow=n_learners) # 50s for 876m tokens
tokens[["uniform"]] = matrix(sample(1:vocab_size, tot_tokens, prob=wf_distros[["uniform"]], replace=T), nrow=n_learners) 
end_time <- Sys.time() 

end_time - start_time # 95 seconds
object.size(tokens)

# too big to easily read/write from disk (3.76 Gb compressed)
# save(wf_distros, tokens, file="cached_tokens.RData") # 1.75 b tokens 6.5 Gb RAM
