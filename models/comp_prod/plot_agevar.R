# Each SEEDLingS ruler's coefficient as a function of age, from fit_agevar.R,
# against the constant-coefficient estimates in lambda_by_ruler.csv.
# Usage: Rscript models/comp_prod/plot_agevar.R
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
here <- "models/comp_prod"
ages <- seq(7, 18, by = 0.5)
band <- function(base, slope, ruler) {
  m <- outer(base, rep(1, length(ages))) + outer(slope, log(ages / 12))
  tibble(ruler = ruler, age = ages, med = apply(m, 2, median),
         lo = apply(m, 2, quantile, 0.025), hi = apply(m, 2, quantile, 0.975))
}
o <- as.data.frame(readRDS(file.path(here, "fit_agevar_onset.rds")),
                   pars = c("lambda", "beta_v", "beta_w", "gamma_r", "gamma_v", "gamma_w"))
l <- as.data.frame(readRDS(file.path(here, "fit_agevar_lena.rds")), pars = c("lambda_c", "gamma_r"))
lev <- c("Within child × word (lagged noun tokens)", "Between words (SEEDLingS frequency)",
         "Between children (noun tokens)", "Between children (LENA adult words)")
d <- bind_rows(
  band(o$lambda, o$gamma_r, lev[1]),
  band(o$lambda * o$beta_w, o$gamma_w, lev[2]),
  band(o$lambda * o$beta_v, o$gamma_v, lev[3]),
  band(l$lambda_c, l$gamma_r, lev[4])) %>% mutate(ruler = factor(ruler, levels = lev))
write_csv(d, file.path(here, "lambda_by_ruler_age.csv"))

const <- read_csv(file.path(here, "lambda_by_ruler.csv"), show_col_types = FALSE) %>%
  filter(lagged | str_detect(ruler, "LENA")) %>%
  transmute(ruler = factor(lev[c(4, 3, 2, 1)], levels = lev), med)

p <- ggplot(d, aes(age)) +
  geom_hline(yintercept = 0, colour = "#b5b4b0", linewidth = .3) +
  geom_hline(yintercept = 1, colour = "#52514e", linewidth = .4, linetype = "22") +
  geom_hline(data = const, aes(yintercept = med), colour = "#eb6834", linewidth = .5) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#2a78d6", alpha = .18) +
  geom_line(aes(y = med), colour = "#2a78d6", linewidth = .9) +
  facet_wrap(~ruler, nrow = 1, labeller = label_wrap_gen(24)) +
  scale_x_continuous(breaks = seq(8, 18, 2)) +
  coord_cartesian(ylim = c(-2, 4)) +
  labs(x = "Age (months)", y = "λ: logit per log-unit of input",
       title = "Ruler coefficients by age, SEEDLingS comprehension (6–18 months)",
       subtitle = "Blue: age-varying fit, posterior median and 95% band. Orange: constant-coefficient estimate. Dashed: λ = 1.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e4e3df", linewidth = .3),
        axis.text = element_text(colour = "#52514e"), plot.subtitle = element_text(colour = "#52514e"),
        strip.text = element_text(hjust = 0), plot.background = element_rect(fill = "#fcfcfb", colour = NA))
ggsave(file.path(here, "fig_lambda_rulers_age.png"), p, width = 11, height = 3.8, dpi = 200)
