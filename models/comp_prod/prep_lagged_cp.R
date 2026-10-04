# Add production-onset risk sets to the lagged exposure data (run prep_lagged.R first).
#
# Production risk set for child i, noun j: CDI months from the first "understands"
# (inclusive -- a word can be first understood and first said in the same month)
# through the first "says". Exposure is still lagged to months < t, so input that
# follows the child's first production of a word never enters; input that follows
# comprehension does, which is real input the child receives.
#
# Usage: Rscript models/comp_prod/prep_lagged_cp.R
suppressPackageStartupMessages(library(tidyverse))
here <- "models/comp_prod"
lagged <- readRDS(file.path(here, "seedlings_lagged_standata.rds"))
rows <- lagged$all_rows

ps <- rows %>% arrange(i, j, age) %>% group_by(i, j) %>%
  mutate(comp_on = cumsum(y >= 1) > 0, said = cumsum(y == 2)) %>%
  filter(comp_on, said == 0 | (said == 1 & y == 2)) %>%
  mutate(first = as.integer(row_number() == 1), event = as.integer(y == 2),
         age_prev = lag(age, default = 0),
         res_lag_prev = lag(res_lag, default = 0),
         res_full_prev = lag(res_full, default = 0)) %>%
  ungroup()
cat(sprintf("production risk-set rows: %d | production onsets: %d | said at comprehension onset: %d\n",
            nrow(ps), sum(ps$event), sum(ps$event & ps$first)))

add_prod <- function(sdl, variant) c(sdl, list(
  P = nrow(ps), pkid = ps$i, pitem = ps$j, page = ps$age, page_prev = ps$age_prev,
  pfirst = ps$first, pevent = ps$event,
  pres = ps[[paste0("res_", variant)]], pres_prev = ps[[paste0("res_", variant, "_prev")]]))
saveRDS(list(lag = add_prod(lagged$lag, "lag"), full = add_prod(lagged$full, "full"),
             k = lagged$k, prod_rows = ps),
        file.path(here, "seedlings_lagged_cp_standata.rds"))
