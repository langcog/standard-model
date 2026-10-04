# Compare lambda (logit per log-token) across rulers and models fit so far.
suppressPackageStartupMessages(library(tidyverse))
here <- "models/comp_prod"
rd <- function(f, par, ruler, model) {
  s <- read.csv(file.path(here, f), row.names = 1)
  tibble(ruler = ruler, model = model, med = s[par, "X50."], lo = s[par, "X2.5."], hi = s[par, "X97.5."])
}
d <- bind_rows(
  rd("summary_comp_prod.csv", "lambda_c", "Between-child: LENA adult words", "142 items, all CDI rows"),
  rd("summary_lagged_cp_full.csv", "lambda_child", "Between-child: noun tokens", "Onset, all months"),
  rd("summary_lagged_cp_lag.csv", "lambda_child", "Between-child: noun tokens", "Onset, lagged"),
  rd("summary_lagged_cp_full.csv", "lambda_word", "Between-word: SEEDLingS frequency", "Onset, all months"),
  rd("summary_lagged_cp_lag.csv", "lambda_word", "Between-word: SEEDLingS frequency", "Onset, lagged"),
  rd("summary_lagged_cp_full.csv", "lambda", "Within child × word", "Onset, all months"),
  rd("summary_lagged_cp_lag.csv", "lambda", "Within child × word", "Onset, lagged")) %>%
  mutate(label = factor(paste0(ruler, "  ·  ", model), levels = rev(unique(paste0(ruler, "  ·  ", model)))),
         lagged = model == "Onset, lagged")
write_csv(d, file.path(here, "lambda_by_ruler.csv"))
p <- ggplot(d, aes(y = label)) +
  geom_vline(xintercept = 1, colour = "#52514e", linewidth = .4, linetype = "22") +
  geom_linerange(aes(xmin = lo, xmax = hi, colour = lagged), linewidth = 1.6) +
  geom_point(aes(x = med, colour = lagged), size = 2.6) +
  scale_colour_manual(values = c(`FALSE` = "#2a78d6", `TRUE` = "#eb6834"),
                      labels = c("Unlagged / all months", "Lagged (months before t only)"), name = NULL) +
  labs(x = "λ: logit change per log-token of exposure (dashed: SM2's fixed λ = 1)", y = NULL,
       title = "Each exposure ruler implies a different λ",
       subtitle = "SEEDLingS, 44 children; posterior median and 95% interval") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#e4e3df", linewidth = .3),
        axis.text = element_text(colour = "#52514e"), plot.subtitle = element_text(colour = "#52514e"),
        legend.position = "top", plot.background = element_rect(fill = "#fcfcfb", colour = NA))
ggsave(file.path(here, "fig_lambda_rulers.png"), p, width = 9, height = 4, dpi = 200)
