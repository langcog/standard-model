# Summarize the lambda-identifiability fits (run after run_sim.R A..E).
suppressPackageStartupMessages(library(tidyverse))
here <- "sims/lambda_identifiability"
truth <- readRDS(file.path(here, "sim_data.rds"))$truth

labels <- c(A = "A  λ = 1, no frequency (SM2 no_freq)",
            B = "B  λ free, no ruler",
            C = "C  λ free, frequency ruler",
            D = "D  λ free, observed-input ruler",
            E = "E  λ free, noisy frequency ruler")
fits <- map_dfr(names(labels), \(id) {
  f <- file.path(here, paste0("fit_", id, ".rds"))
  if (file.exists(f)) readRDS(f)$draws else NULL
}) %>% mutate(label = factor(labels[fit], levels = rev(labels)))

long <- fits %>%
  select(label, lambda, delta, pi_alpha) %>%
  pivot_longer(-label, names_to = "param") %>%
  mutate(param = factor(param, c("lambda", "delta", "pi_alpha"),
                        c("λ (logit per log-token)", "δ (acceleration)", "π_α (efficiency share)")))
summ <- long %>% group_by(label, param) %>%
  summarise(med = median(value), lo = quantile(value, .025), hi = quantile(value, .975),
            .groups = "drop")
write_csv(summ, file.path(here, "summary.csv"))
print(summ, n = Inf)

truth_df <- tibble(param = levels(long$param),
                   value = c(truth$lambda, truth$delta, truth$pi_alpha))

ink <- "#0b0b0b"; ink2 <- "#52514e"; grid <- "#e4e3df"; series <- "#2a78d6"; ref <- "#eb6834"
theme_sim <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = grid, linewidth = .3),
        strip.text = element_text(colour = ink, face = "bold", hjust = 0),
        axis.text = element_text(colour = ink2), plot.title = element_text(colour = ink),
        plot.subtitle = element_text(colour = ink2), plot.caption = element_text(colour = ink2),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA))

p1 <- ggplot(summ, aes(y = label)) +
  geom_vline(data = truth_df, aes(xintercept = value), colour = ref, linewidth = .8,
             linetype = "22") +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = series, linewidth = 1.6) +
  geom_point(aes(x = med), colour = series, size = 2.6) +
  facet_wrap(~param, scales = "free_x") +
  labs(x = NULL, y = NULL,
       title = "Fixing the logit scale at λ = 1 manufactures SM2-like δ and π_α",
       subtitle = "Posterior median and 95% interval; dashed = true value (λ = 2.5, δ = 3.4, π_α = 0.65)",
       caption = "600 simulated children × 100 CDI items, ages 16–30 mo; σ_r pinned at 0.44 except in D") +
  theme_sim
ggsave(file.path(here, "fig_lambda_forest.png"), p1, width = 11, height = 3.8, dpi = 200)

# Ridge: what the logit-scale data fix (age slope B, child SD S) vs. lambda
B <- truth$lambda * (1 + truth$delta); S <- truth$lambda * sqrt(truth$sigma_r^2 + truth$sigma_alpha^2)
ridge <- tibble(lambda = seq(0.6, S / truth$sigma_r, length.out = 200)) %>%
  mutate(delta = B / lambda - 1, pi_alpha = 1 - lambda^2 * truth$sigma_r^2 / S^2)
pts <- fits %>% group_by(fit) %>% slice_sample(n = 600) %>% ungroup() %>%
  mutate(label = factor(labels[fit], levels = labels))
p2 <- ggplot() +
  geom_path(data = ridge, aes(delta, pi_alpha), colour = ink2, linewidth = .6) +
  geom_point(data = pts, aes(delta, pi_alpha), colour = series, alpha = .25, size = .9) +
  annotate("point", x = truth$delta, y = truth$pi_alpha, colour = ref, size = 3.2, shape = 18) +
  facet_wrap(~label, nrow = 1, labeller = label_wrap_gen(22)) +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, 1)) +
  labs(x = "δ", y = "π_α",
       title = "The data pin a curve in (δ, π_α), not a point",
       subtitle = "Grey: every (δ, π_α) giving identical logit-scale fit as λ varies; blue: posterior draws; orange: truth") +
  theme_sim + theme(panel.grid.major.y = element_line(colour = grid, linewidth = .3))
ggsave(file.path(here, "fig_lambda_ridge.png"), p2, width = 12, height = 3.6, dpi = 200)

# Same ridge evaluated at SM2's published English numbers
sm2 <- tibble(lambda = c(0.5, 1, 1.5, 2, 3, 4.2)) %>%
  mutate(delta = 10.89 / lambda - 1, input_share = lambda^2 * 0.44^2 / 1.85^2)
print(sm2)
