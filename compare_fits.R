require(tidyverse)
source("fit_quantiles.R") # for fitSSE function, which also runs the model and generates plot

fits = list.files("fits")
fl = list()
df = data.frame(matrix(NA, nrow=1, ncol=10))
names(df) = c("file", "SSE", paste0("par", 1:8))

for(f in fits) {
  load(paste0("fits/",f))
  f_ = str_replace(f, ".RData", "")
  fl[[ f_ ]] = fit$optim
  df = rbind(df, c(f_, fit$optim$bestval, fit$optim$bestmem))
  fitSSE(fit$optim$bestmem, graph=paste0("figures/",f_))
}

df = na.omit(df)
for(c in 2:ncol(df)) {
  df[,c] = as.numeric(df[,c])
}

df %>% arrange(SSE)

# values for 1st parameter in final population
# hist(fit$member$pop[,1]) 

fl$fit_con_pfF$bestval


# new:
#   49.5695480 2594.3134028 6577.4729702 1843.6366455    0.5580672    0.0349513    0.7199274    0.1699810    2.8869321 

#               file      SSE       par1     par2     par3      par4       par5       par6      par7      par8
#            fit_con_pfT 199304.8  203.82263 2435.811 6937.411 2126.7332 0.55876783 0.03276086 0.7163567 0.2114046
#          fit_uncon_pfT 220464.5  484.02464 3565.781 6831.880 1971.0810 0.96113257 0.02994715 0.8399516 0.4592199
#       fit_unif_con_pfT 224526.9   49.56955 2594.313 6577.473 1843.6366 0.55806718 0.03495130 0.7199274 0.1699810
#            fit_con_pfF 227651.5 1787.79027 2935.880 8199.231 2277.5152 0.55765519 0.55612994 0.7225925 0.2151317
#          fit_uncon_pfF 227651.5 1787.79027 2935.880 8199.231 2277.5152 0.30207068 0.55612994 0.8451734 0.2151317
#       fit_unif_con_pfF 229190.9  380.06784 3308.063 6590.985 2189.0662 0.55525454 1.96168584 0.7236889 0.7844817
#     fit_unif_uncon_pfF 229190.9  380.06784 3308.063 6590.985 2189.0662 0.03799925 1.96168584 0.9657807 0.7844817
#  fit_logzipf_uncon_pfT 232111.0  126.35493 4175.163 8043.818 2746.0623 0.81827050 0.03482425 0.6988839 0.1196900
#     fit_unif_uncon_pfT 245361.3  651.22143 3869.455 7432.155 2501.6678 0.91298455 0.01291388 0.9663620 0.4081933
#     fit_zipf_uncon_pfT 585323.3 1265.74015 2635.739 7602.936  229.4546 0.37843405 0.02830156 0.7527202 0.6873395
#      fit30k_uncon_pfT 664712.9 1221.27163 6720.450 6548.140 1758.3217 0.82494581 0.03077249 0.5954060 0.2508111