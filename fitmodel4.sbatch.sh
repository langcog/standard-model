#!/bin/bash
#
#SBATCH --job-name=standardmodel4
#
#SBATCH -p hns
#SBATCH --time=10:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2G

# load the module
ml R

R --no-save << EOF

source("fit-quantiles.R")
set.seed(1234)

start_time <- Sys.time()

fit <- DEoptim(fitSSE, lower=c(1, 1, 1, 1, .555, .01, .715, .01), 
               upper=c(9000, 9000, 9000, 9000, .564, 2, .724, 2), proc_facilitates=FALSE,
               control = DEoptim.control(NP = 80, itermax = 200))

end_time <- Sys.time()
print(end_time - start_time)

fitSSE(fit$optim$bestmem, graph="constrained_proc_facil_FALSE")

save.image(file='fit_constrained_proc_facil_FALSE.RData')
save(fit, file="fit_con_pfF.RData")
EOF