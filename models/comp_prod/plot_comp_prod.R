# Posterior predictive check + item-level structure for the comp->prod fit.
suppressPackageStartupMessages({ library(rstan); library(tidyverse) })
here <- "models/comp_prod"
b <- readRDS(file.path(here, "seedlings_standata.rds"))
fit <- readRDS(file.path(here, "fit_comp_prod.rds"))
sd <- b$standata

ink2 <- "#52514e"; grid <- "#e4e3df"
cols <- c(Understands = "#2a78d6", Says = "#eb6834")
theme_cp <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = grid, linewidth = .3),
        axis.text = element_text(colour = ink2), plot.subtitle = element_text(colour = ink2),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA), legend.position = "top")

# ---- PPC by age: observed vs model-expected proportion per admin ----
obs <- tibble(a = sd$tt, y = sd$y) %>% group_by(a) %>%
  summarise(Understands = mean(y >= 1), Says = mean(y == 2)) %>%
  mutate(age = sd$age[a], kid = sd$adm_kid[a])
draws <- rstan::extract(fit, pars = c("E_comp", "E_prod"))
pred <- tibble(a = seq_len(sd$A), age = sd$age, kid = sd$adm_kid,
               Understands = colMeans(draws$E_comp), Says = colMeans(draws$E_prod))
by_age <- function(m) m %>% group_by(age) %>% summarise(across(c(Understands, Says), mean))
age_draws <- map_dfr(c(Understands = "E_comp", Says = "E_prod"), \(p) {
  m <- draws[[p]]
  map_dfr(sort(unique(sd$age)), \(ag) {
    v <- rowMeans(m[, sd$age == ag, drop = FALSE])
    tibble(age = ag, lo = quantile(v, .05), hi = quantile(v, .95))
  })
}, .id = "measure")
ppc <- bind_rows(observed = by_age(obs), model = by_age(pred), .id = "source") %>%
  pivot_longer(c(Understands, Says), names_to = "measure")
p1 <- ggplot() +
  geom_ribbon(data = age_draws, aes(age, ymin = lo, ymax = hi, fill = measure), alpha = .2) +
  geom_line(data = filter(ppc, source == "model"), aes(age, value, colour = measure), linewidth = .8) +
  geom_point(data = filter(ppc, source == "observed"), aes(age, value, colour = measure), size = 2.2) +
  scale_colour_manual(values = cols, NULL) + scale_fill_manual(values = cols, guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age (months)", y = "Mean proportion of items", subtitle = "Points: observed; lines + 90% band: model") +
  theme_cp

# per-child fit (all 44 kids): observed vs predicted per admin
p2 <- bind_rows(observed = obs, model = pred, .id = "source") %>%
  pivot_longer(c(Understands, Says), names_to = "measure") %>%
  pivot_wider(names_from = source, values_from = value) %>%
  ggplot(aes(model, observed, colour = measure)) +
  geom_abline(colour = ink2, linewidth = .3) + geom_point(alpha = .5, size = 1.4) +
  scale_colour_manual(values = cols, NULL) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  labs(x = "Model", y = "Observed", subtitle = "Each point = one child × month") + theme_cp

# ---- item structure: what predicts each threshold ----
post <- rstan::extract(fit, pars = c("psi", "Delta", "beta_p", "kappa"))
items <- b$items %>% mutate(
  log_p = sd$logp,
  # effective comprehension threshold relative to frequency: log tokens needed beyond frequency
  comp_difficulty = colMeans(post$psi - as.vector(post$beta_p) * matrix(sd$logp, nrow(post$psi), sd$J, byrow = TRUE)),
  prod_gap = colMeans(post$Delta), n_syll = n_syll)
item_cor <- items %>% summarise(
  `comp difficulty ~ log freq` = cor(comp_difficulty, log_p),
  `comp difficulty ~ syllables` = cor(comp_difficulty, log(n_syll)),
  `production gap ~ log freq` = cor(prod_gap, log_p),
  `production gap ~ syllables` = cor(prod_gap, log(n_syll)),
  `comp difficulty ~ production gap` = cor(comp_difficulty, prod_gap))
print(item_cor)
write_csv(items %>% select(item, lexical_category, log_p, n_syll, comp_difficulty, prod_gap),
          file.path(here, "item_estimates.csv"))
p3 <- items %>% ggplot(aes(comp_difficulty, prod_gap)) +
  geom_point(colour = "#2a78d6", alpha = .7, size = 1.8) +
  geom_text(data = \(d) d %>% filter(abs(scale(prod_gap)) > 1.8 | abs(scale(comp_difficulty)) > 1.8),
            aes(label = item), size = 2.8, colour = ink2, vjust = -0.7, check_overlap = TRUE) +
  facet_wrap(~lexical_category, nrow = 1) +
  labs(x = "Comprehension difficulty (log-token threshold, net of frequency)",
       y = "Production gap Δ (logit)", subtitle = "Per-word posterior means") + theme_cp

ggsave(file.path(here, "fig_ppc_age.png"), p1, width = 6, height = 4.2, dpi = 200)
ggsave(file.path(here, "fig_ppc_admin.png"), p2, width = 5, height = 4.2, dpi = 200)
ggsave(file.path(here, "fig_items.png"), p3, width = 12, height = 3.8, dpi = 200)
