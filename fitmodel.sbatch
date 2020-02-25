#!/bin/bash
#
#SBATCH --job-name=standardmodel1
#
#SBATCH -p hns
#SBATCH --time=20:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2G

# load the module
ml R

#R CMD BATCH fit-quantiles.R &

R --no-save << EOF

source("fit-quantiles.R")
set.seed(1234)

start_time <- Sys.time()

fit <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .01, .01, .01, .01, 1), 
               upper=c(9000, 9000, 9000, 9000, 1, 2, 1, 2, 9), distro="uniform",
        control = DEoptim.control(NP = 80, itermax = 100)) 

end_time <- Sys.time()
print(end_time - start_time)

fitSSE(fit$optim$bestmem, graph="unif_unconstrained_proc_facil_TRUE") 

#save.image(file='fit_unif_unconstrained_proc_facil_TRUE.RData') # to save session
save(fit, file="fit_unif_uncon_pfT.RData")
EOF