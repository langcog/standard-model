# Fig. 2: the ridge between lambda and delta, and recovery with and without a ruler.
# Uses the simulation fits in sims/lambda_identifiability (run_sim.R, fits A-E).
# Usage: Rscript measurement_paper/figures/fig2_simulation.R
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })
source("measurement_paper/figures/common.R")
sim <- "sims/lambda_identifiability"; out <- "measurement_paper/figures"
truth <- readRDS(file.path(sim, "sim_data.rds"))$truth
B <- truth$lambda * (1 + truth$delta)                                  # logit age slope
S <- truth$lambda * sqrt(truth$sigma_r^2 + truth$sigma_alpha^2)        # logit child SD

fit_lab <- c(A = "λ fixed at 1", B = "λ free, no ruler", C = "Word-frequency ruler",
             D = "Measured-input ruler", E = "Frequency ruler\nmeasured with error")
grid <- grid_col
cols <- c(A = ink, B = blue, C = orange, D = aqua, E = orange)
draws <- map_dfr(names(fit_lab), \(id) readRDS(file.path(sim, paste0("fit_", id, ".rds")))$draws) %>%
  mutate(input_share = 1 - pi_alpha)

th <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = grid, linewidth = .3),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink),
        strip.text = element_text(colour = ink, hjust = 0, size = 10),
        plot.title = element_text(colour = ink, size = 10, face = "bold"),
        plot.title.position = "plot", legend.position = "none")

# ---- A: the ridge ----
ridge <- tibble(lambda = seq(0.55, 3.6, length.out = 300), delta = B / lambda - 1)
set.seed(1)
pts <- draws %>% filter(fit %in% c("B", "C", "D")) %>% group_by(fit) %>% slice_sample(n = 500) %>% ungroup()
a_int <- draws %>% filter(fit == "A") %>%
  summarise(med = median(delta), lo = quantile(delta, .025), hi = quantile(delta, .975))
lab <- tribble(~fit, ~x, ~y, ~label, ~hjust,
               "A", 1.09, 11.0, "λ fixed at 1", 0,
               "B", 1.62, 7.6, "λ free, no ruler", 0,
               "C", 2.80, 5.2, "Word-frequency ruler", 0.5,
               "D", 1.75, 1.7, "Measured-input ruler", 0.5)
pA <- ggplot() +
  geom_path(data = ridge, aes(lambda, delta), colour = "#b5b4b0", linewidth = .7) +
  geom_point(data = pts, aes(lambda, delta, colour = fit), alpha = .28, size = .7, stroke = 0) +
  geom_linerange(data = a_int, aes(x = 1, ymin = lo, ymax = hi), colour = ink, linewidth = 1.1) +
  geom_point(data = a_int, aes(1, med), colour = ink, size = 2.2) +
  annotate("point", x = truth$lambda, y = truth$delta, shape = 23, size = 3.2, stroke = 1,
           colour = ink, fill = "white") +
  annotate("text", x = truth$lambda + .08, y = truth$delta + .75, label = "truth", hjust = 0,
           size = 3.1, colour = ink, fontface = "italic") +
  annotate("text", x = 3.5, y = 13.3, hjust = 1, vjust = 1, size = 2.9, colour = ink2, lineheight = .95,
           label = "Grey curve: every (λ, δ) with the\nsame age slope, λ(1 + δ) = 10.9") +
  geom_text(data = lab, aes(x, y, label = label, colour = fit, hjust = hjust), size = 3.1, fontface = "bold") +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(.5, 3.5, .5)) + scale_y_continuous(breaks = seq(0, 14, 2)) +
  coord_cartesian(xlim = c(0.5, 3.6), ylim = c(0, 14), expand = FALSE) +
  labs(x = "λ: logits per log-unit of input", y = "δ: acceleration", title = "A") + th

# ---- B: recovery ----
par_lab <- c(lambda = "λ", delta = "δ", input_share = "Input share of\nchild variance")
summ <- draws %>% select(fit, lambda, delta, input_share) %>%
  pivot_longer(-fit, names_to = "param") %>% group_by(fit, param) %>%
  summarise(med = median(value), lo = quantile(value, .025), hi = quantile(value, .975), .groups = "drop") %>%
  mutate(param = factor(par_lab[param], levels = par_lab),
         label = factor(fit_lab[fit], levels = rev(fit_lab)), noisy = fit == "E")
tr <- tibble(param = factor(par_lab, levels = par_lab),
             value = c(truth$lambda, truth$delta, 1 - truth$pi_alpha))
write_csv(summ, file.path(out, "fig2_simulation.csv"))
pB <- ggplot(summ, aes(y = label)) +
  geom_vline(data = tr, aes(xintercept = value), colour = ink, linewidth = .4, linetype = "22") +
  geom_linerange(aes(xmin = lo, xmax = hi, colour = fit), linewidth = 1.3) +
  geom_point(aes(x = med, colour = fit, fill = ifelse(noisy, "white", cols[fit])), shape = 21, size = 2.4, stroke = 1) +
  facet_wrap(~param, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = cols) + scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = .08),
                     labels = \(x) if (max(x, na.rm = TRUE) < 1) paste0(round(100 * x), "%") else x) +
  labs(x = "Posterior median and 95% interval (dashed: true value)", y = NULL, title = "B") + th +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = ink, size = 9),
        panel.spacing.x = unit(10, "pt"))

p <- pA + pB + plot_layout(widths = c(1, 1.45))
save_fig(p, "fig2_simulation", width = 10, height = 3.9)
