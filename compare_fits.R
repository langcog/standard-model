require(tidyverse)

fits = list.files("fits")
fl = list()
df = data.frame(matrix(NA, nrow=1, ncol=10))
names(df) = c("file", "SSE", paste0("par", 1:8))

for(f in fits) {
  load(paste0("fits/",f))
  f_ = str_replace(f, ".RData", "")
  fl[[ f_ ]] = fit$optim
  df = rbind(df, c(f_, fit$optim$bestval, fit$optim$bestmem))
}

df = na.omit(df)
for(c in 2:ncol(df)) {
  df[,c] = as.numeric(df[,c])
}

df %>% arrange(SSE)

# values for 1st parameter in final population
# hist(fit$member$pop[,1]) 

fl$fit_con_pfF$bestval