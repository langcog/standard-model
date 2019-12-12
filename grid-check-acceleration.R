# McMurray 2007: studies consistently reveal that, during the second postnatal year, 
# word learning accelerates dramatically 

# DO THE GRID
# looking for parameter regimes that create acceleration

# vocab_size, distro, input_rate, n_learners, threshold, max_age, 
#  mean_learning_rate, threshold_sd, proc_facilitates, proc_speed_dev)
source("model-nonsampling.R")

start_t = Sys.time()
dat = simulate(vocab_size=10000, distro="zipf", 1000, n_learners=100, 500, max_age=24, 1, .5, 10, F, .72)
stop_t = Sys.time()
stop_t - start_t # 1.2s

input_rates = c(500, 1000, 1500, 2000, 2500, 3000)
thresholds = c(500, 1000, 1500, 2000, 2500, 3000, 4000)
threshold_sds = c(0, 10, 100, 200, 300)
learning_rates = c(.5, 1, 2, 3)
learning_rate_sds = c(0, .5, 1, 2, 3)
# distro="uniform" / "zipf"
# processing_facilitates T / F (if T: 
proc_speeds = seq(.1,1,.2)

length(input_rates)*length(thresholds)*length(learning_rates)*length(threshold_sds)*length(learning_rate_sds)
# 4200 * 2 * 2 = 16800 * 1.2 / 60 = 336

acceleration_test <- function(dat) {
  d = dat$known_words %>% filter(month>11) %>% group_by(month) %>% summarize(mean=mean(words)) 
  return( mean(diff(diff(d$mean))) )
}

# quick testing parms
#input_rates = c(500, 1000)
#thresholds = c(500, 1000)
#threshold_sds = c(0, 10)
#learning_rates = c(1)
#learning_rate_sds = c(0)

do_grid <- function(distro, processing_facilitates) {
  dat = data.frame(distro=NA, input_rate=NA, threshold=NA, 
                   threshold_sd=NA, learning_rate=NA, learning_rate_sd=NA, proc_facilitates=NA, proc_speed=NA, acceleration=NA)
  for(i in input_rates) {
    for(t in thresholds) {
      for(lr in learning_rates) {
        for(t_sd in threshold_sds) {
          for(lr_sd in learning_rate_sds) {
            if(processing_facilitates) {
              for(ps in proc_speeds) {
                sim = simulate(vocab_size=10000, distro, lr, n_learners=100, t, max_age=24, lr, t_sd, lr_sd, T, ps)
                accel = acceleration_test(sim)
                dat = rbind(dat, c(distro, i, t, t_sd, lr, lr_sd, T, ps, accel))
              }
            } else {
              sim = simulate(vocab_size=10000, distro, lr, n_learners=100, t, max_age=24, lr, t_sd, lr_sd, F, .72)
              accel = acceleration_test(sim)
              dat = rbind(dat, c(distro, i, t, t_sd, lr, lr_sd, F, NA, accel))
            }
          }
        }
      }
    }
  }
  return(dat[2:nrow(dat),])
}

grid_uniform = do_grid("uniform", F)
grid_zipf = do_grid("zipf", F)

grid_uniform_proc = do_grid("uniform", T)
grid_zipf_proc = do_grid("zipf", T)

all = rbind(grid_uniform, grid_zipf, grid_uniform_proc, grid_zipf_proc)
for(c in 2:ncol(all)) all[,c] = as.numeric(all[,c])
save(all, file="acceleration-grid.RData")

ggplot(subset(all, proc_facilitates==T & distro=="zipf"), aes(input_rate, threshold)) + geom_tile(aes(fill = acceleration), colour = "white") + 
  facet_grid(rows=vars(input_rate), cols=vars(threshold)) + scale_colour_gradient2()

ggplot(all, aes(input_rate, threshold)) + geom_tile(aes(fill = acceleration), colour = "white") + 
  facet_grid(rows=vars(input_rate), cols=vars(threshold)) + scale_colour_gradient2()

ggplot(all, aes(input_rate, threshold)) + geom_tile(aes(fill = acceleration), colour = "white") + 
   facet_grid(rows=vars(input_rate), cols=vars(distro)) + scale_colour_gradient2()

ggplot(subset(all, proc_facilitates==F & distro=="zipf"), aes(learning_rate, threshold)) + geom_tile(aes(fill = acceleration), colour = "white") + 
  facet_grid(rows=vars(learning_rate), cols=vars(threshold)) + scale_colour_gradient2()

acc = subset(all, acceleration>1)
table(acc$distro)
table(acc$input_rate, acc$threshold)
table(acc$input_rate, acc$proc_speed)
table(acc$proc_facilitates)
subset(acc, proc_facilitates==F)

summary(acc$acceleration)
# median = 9, max=54

dim(subset(all, acceleration>9)) # 1578 

View(subset(all, acceleration>30))


