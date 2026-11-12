# Fig. 1: where the worth of a log-unit of input (lambda) enters the accumulator,
# the four rulers that measure it, and why growth curves alone cannot.
#   A  the model as an equation, with each ruler pointing at the term it varies
#   B  lambda as a slope: probability of knowing a word against tokens heard
#   C  two (lambda, delta) pairs with the same age slope give the same growth curve
#      but different effects of hearing more input
# Usage: Rscript measurement_paper/figures/fig1_schematic.R
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })
source("measurement_paper/figures/common.R")

lam_lo <- 0.57; lam_hi <- 1.16      # within-child lagged ruler; LENA ruler
B_age <- 10.9                       # logit age slope, lambda (1 + delta), SM2 English

# ---- A: schematic ----------------------------------------------------------
box <- function(xmin, xmax, ymin, ymax, fill = "white", colour = rule, lw = .4)
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, colour = colour, linewidth = lw)
txt <- function(x, y, label, size = 3.1, colour = ink, ...) annotate("text", x = x, y = y, label = label, size = size, colour = colour, lineheight = .95, ...)
arrow_up <- function(x, y, xend, yend)
  annotate("segment", x = x, y = y, xend = xend, yend = yend, colour = ink2, linewidth = .4,
           arrow = arrow(length = unit(4, "pt"), type = "closed"))

terms <- tribble(~xmin, ~xmax, ~sym, ~name,
                 36, 50, "log~r[i]", "input rate",
                 54, 68, "log~p[j]", "word frequency",
                 72, 92, "(1 + delta)~log(t / a[0])", "age, with acceleration",
                 96, 106, "log~alpha[i]", "efficiency",
                 110, 118, "psi[j]", "threshold") %>% mutate(xmid = (xmin + xmax) / 2)
ops <- tibble(x = c(52, 70, 94, 108), lab = c("+", "+", "+", "−"))
rulers <- tribble(~xmin, ~xmax, ~head, ~body, ~est,
                  0, 21, "LENA", "adult words per hour,\nacross children", "λ = 1.16 [0.81, 1.69]",
                  23, 44, "Bilingual exposure", "share of input in each of\na child's two languages", "λ ≈ 1.1",
                  46, 67, "Own noun tokens", "each child's tokens of each\nnoun, before each CDI", "λ = 0.57 [0.51, 0.63]",
                  69, 90, "Word frequency", "tokens of each word,\nacross words", "λ = 1.04 [0.89, 1.18]") %>%
  mutate(xmid = (xmin + xmax) / 2)
ey0 <- 34; ey1 <- 42; ry0 <- 6; ry1 <- 23        # equation row and ruler row

pA <- ggplot() +
  # outcome and lambda
  box(0, 19, ey0, ey1) + txt(9.5, 38, "log-odds child i\nknows word j", size = 2.9) +
  txt(21.5, 38, "=", size = 4) +
  box(24, 31, ey0, ey1, fill = ink, colour = ink) + txt(27.5, 38, "λ", size = 4.6, colour = "white", fontface = "bold") +
  txt(27.5, 47.2, "worth of a log-unit\nof input, in logits", size = 2.9, fontface = "bold") +
  annotate("segment", x = 27.5, xend = 27.5, y = 44.6, yend = 42.4, colour = ink, linewidth = .4) +
  txt(33.5, 38, "×", size = 4) +
  # bracket around the exposure terms
  annotate("path", x = c(36, 35, 35, 36), y = c(ey1 + 1.2, ey1 + 1.2, ey0 - 1.2, ey0 - 1.2), colour = ink2, linewidth = .5) +
  annotate("path", x = c(118, 119, 119, 118), y = c(ey1 + 1.2, ey1 + 1.2, ey0 - 1.2, ey0 - 1.2), colour = ink2, linewidth = .5) +
  txt(77, 50.2, "log tokens accumulated, relative to the word's threshold", size = 2.9, colour = ink2) +
  annotate("segment", x = 36, xend = 118, y = 48.4, yend = 48.4, colour = rule, linewidth = .3) +
  geom_rect(data = terms, aes(xmin = xmin, xmax = xmax, ymin = ey0, ymax = ey1), fill = "white", colour = rule, linewidth = .4) +
  geom_text(data = terms, aes(xmid, 38, label = sym), size = 3.5, colour = ink, parse = TRUE) +
  geom_text(data = terms, aes(xmid, 45.2, label = name), size = 2.7, colour = ink2) +
  geom_text(data = ops, aes(x, 38, label = lab), size = 3.6, colour = ink) +
  # rulers
  txt(0, 3.2, "Rulers: independently measured variation in log input. Each arrow points to the term the ruler varies.",
      size = 2.9, hjust = 0, colour = ink2) +
  geom_rect(data = rulers, aes(xmin = xmin, xmax = xmax, ymin = ry0, ymax = ry1), fill = "#f0efec", colour = NA) +
  geom_text(data = rulers, aes(xmid, 20.2, label = head), size = 3.0, colour = ink, fontface = "bold") +
  geom_text(data = rulers, aes(xmid, 14.6, label = body), size = 2.6, colour = ink2, lineheight = .95) +
  geom_text(data = rulers, aes(xmid, 8.6, label = est), size = 2.9, colour = ink) +
  arrow_up(14, ry1, 39, ey0 - .6) + arrow_up(36, ry1, 42, ey0 - .6) +           # LENA, bilingual -> input rate
  arrow_up(54, ry1, 46.5, ey0 - .6) + arrow_up(58, ry1, 58, ey0 - .6) +         # own tokens -> rate and frequency
  arrow_up(76, ry1, 64, ey0 - .6) +                                             # frequency -> word frequency
  # what growth data give
  annotate("segment", x = 82, xend = 82, y = ey0 - .6, yend = 24, colour = rule, linewidth = .4, linetype = "22") +
  annotate("segment", x = 101, xend = 101, y = ey0 - .6, yend = 24, colour = rule, linewidth = .4, linetype = "22") +
  txt(94, 15, "No ruler here. Vocabulary-by-age data\ngive only the products\nλ(1 + δ), the age slope, and\nλσ, the spread across children.",
      size = 2.7, colour = ink2, hjust = 0) +
  annotate("segment", x = 92.5, xend = 92.5, y = ry0, yend = ry1, colour = rule, linewidth = .4) +
  coord_cartesian(xlim = c(0, 120), ylim = c(1.5, 52), expand = FALSE, clip = "off") +
  labs(title = "A") + theme_void(base_size = 10) +
  theme(plot.title = element_text(colour = ink, size = 10, face = "bold"), plot.title.position = "plot",
        plot.margin = margin(4, 6, 8, 6))

# ---- shared theme for B and C ----------------------------------------------
th <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = grid_col, linewidth = .3),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink, size = 9),
        plot.title = element_text(colour = ink, size = 10, face = "bold"), plot.title.position = "plot",
        plot.subtitle = element_text(colour = ink2, size = 9), legend.position = "none")
lam_cols <- c(hi = blue, lo = orange)

# ---- B: lambda as a slope --------------------------------------------------
dB <- expand_grid(ratio = exp(seq(log(1 / 8), log(8), length.out = 200)), lam = c(hi = lam_hi, lo = lam_lo)) %>%
  mutate(which = names(lam), p = plogis(lam * log(ratio)))
ptB <- tibble(which = c("hi", "lo"), ratio = 2, p = plogis(c(lam_hi, lam_lo) * log(2)))
pB <- ggplot(dB, aes(ratio, p, colour = which)) +
  geom_vline(xintercept = 1, colour = rule, linewidth = .4) +
  geom_line(linewidth = .9) +
  geom_point(data = ptB, size = 2.4, shape = 21, fill = "white", stroke = 1) +
  annotate("text", x = 2.25, y = ptB$p[1] + .005, hjust = 0, vjust = 0.5, size = 2.9, colour = ink,
           label = sprintf("%.0f%%", 100 * ptB$p[1])) +
  annotate("text", x = 2.25, y = ptB$p[2] - .025, hjust = 0, vjust = 0.5, size = 2.9, colour = ink,
           label = sprintf("%.0f%%", 100 * ptB$p[2])) +
  annotate("text", x = 7.6, y = .985, hjust = 1, size = 3.0, colour = blue, fontface = "bold", label = "λ = 1.16") +
  annotate("text", x = 7.6, y = .685, hjust = 1, size = 3.0, colour = orange, fontface = "bold", label = "λ = 0.57") +
  scale_colour_manual(values = lam_cols) +
  scale_x_continuous(trans = "log", breaks = c(1 / 8, 1 / 4, 1 / 2, 1, 2, 4, 8),
                     labels = c("⅛×", "¼×", "½×", "1×", "2×", "4×", "8×")) +
  scale_y_continuous(labels = \(x) paste0(round(100 * x), "%"), breaks = seq(0, 1, .25)) +
  coord_cartesian(ylim = c(0, 1.02), expand = FALSE, clip = "off") +
  labs(x = "Tokens heard, relative to the word's threshold", y = "Probability of knowing the word",
       title = "B", subtitle = "λ sets how much one doubling of input buys") + th

# ---- C: same growth curve, different worth of input ------------------------
# logit P = B_age log(t / 24) + lambda d, with d = log(input / median input)
dC <- expand_grid(age = seq(14, 34, by = .1), which = c("hi", "lo"), mult = c(1, 4)) %>%
  mutate(lam = c(hi = lam_hi, lo = lam_lo)[which], p = plogis(B_age * log(age / 24) + lam * log(mult)))
shift <- 24 - 24 * 4^(-c(hi = lam_hi, lo = lam_lo) / B_age)       # months ahead at the 50% point
pC <- ggplot() +
  geom_line(data = filter(dC, mult == 1, which == "hi"), aes(age, p), colour = ink, linewidth = 1) +
  geom_line(data = filter(dC, mult == 4), aes(age, p, colour = which), linewidth = .9) +
  annotate("text", x = 24.6, y = .44, hjust = 0, size = 2.9, colour = ink, lineheight = .95,
           label = "median input:\nidentical under\nboth values of λ") +
  annotate("text", x = 19.6, y = .93, hjust = 1, size = 2.9, colour = ink2, label = "4× the input:") +
  annotate("text", x = 19.6, y = .84, hjust = 1, size = 2.9, colour = blue, fontface = "bold", lineheight = .95,
           label = sprintf("λ = 1.16\n%.1f months ahead", shift["hi"])) +
  annotate("text", x = 19.6, y = .69, hjust = 1, size = 2.9, colour = orange, fontface = "bold", lineheight = .95,
           label = sprintf("λ = 0.57\n%.1f months ahead", shift["lo"])) +
  scale_colour_manual(values = lam_cols) +
  scale_x_continuous(breaks = seq(14, 34, 4)) +
  scale_y_continuous(labels = \(x) paste0(round(100 * x), "%"), breaks = seq(0, 1, .25)) +
  coord_cartesian(xlim = c(14, 34), ylim = c(0, 1.02), expand = FALSE) +
  labs(x = "Age (months)", y = "Probability of producing a typical word",
       title = "C", subtitle = "Same age slope λ(1 + δ), different worth of input") + th

p <- pA / (pB | pC) + plot_layout(heights = c(1.15, 1))
save_fig(p, "fig1_schematic", width = 10, height = 7)
