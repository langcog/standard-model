# McMurray 2007: studies consistently reveal that, during the second postnatal year, 
# word learning accelerates dramatically 

# DO THE GRID
# looking for parameter regimes that create acceleration

# vocab_size, distro, input_rate, n_learners, threshold, max_age, 
#  mean_learning_rate, threshold_sd, proc_facilitates, proc_speed_dev)
source("model-nonsampling.R")

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

start_t = Sys.time()
dat = simulate(ex_parms)
stop_t = Sys.time()
stop_t - start_t # 1.1s

input_rates = seq(500, 3000, 500)
thresholds = seq(500, 6000, 1000)
threshold_sds = c(0, 100)
#learning_rates = c(.5, 1, 2, 3)
#learning_rate_sds = c(0, .5, 1, 2, 3)
proc_speed_asymps = c(.3, .6, .9) # adult = .56
proc_speed_asymp_sds = c(.1)
# distro="uniform" / "zipf"
# processing_facilitates T / F (if T: 
proc_speed_devs = c(.4, .7, 1) # .72
proc_speed_dev_sds = c(.1)

length(input_rates)*
  length(thresholds)*length(threshold_sds)*
  length(proc_speed_asymps)*length(proc_speed_asymp_sds)*
  length(proc_speed_devs)*length(proc_speed_dev_sds)
# 648 * 2 * 2 = 2592 * 1.1 / 60 = 47.5/60 = .79hrs

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
      for(lr in proc_speed_asymps) { 
        for(t_sd in threshold_sds) {
          for(lr_sd in proc_speed_asymp_sds) {
              for(ps in proc_speed_devs) {
                parms = list(distro=distro,
                     input_rate = i,
                     input_rate_sd = 100, 
                     threshold = t,
                     threshold_sd = t_sd,
                     mean_learning_rate = lr, # proc_speed_asymp
                     learning_rate_sd = lr_sd,
                     proc_facilitates = processing_facilitates,
                     proc_speed_dev = ps, 
                     proc_speed_dev_sd = .1
                )
                sim = simulate(parms)
                #sim = simulate(vocab_size=10000, distro, lr, n_learners=100, t, max_age=24, lr, t_sd, lr_sd, T, ps)
                accel = acceleration_test(sim)
                dat = rbind(dat, c(distro, i, t, t_sd, lr, lr_sd, T, ps, accel))
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


