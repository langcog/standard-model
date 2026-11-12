# Shared colours and output helper for the measurement-paper figures.
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e4e3df"; rule <- "#b5b4b0"
blue <- "#2a78d6"; orange <- "#eb6834"; aqua <- "#1baf7a"

# PNG via ragg, plus a vector PDF. Greek letters need a Unicode-aware PDF device:
# quartz on macOS, cairo elsewhere.
save_fig <- function(p, name, width, height, dir = "measurement_paper/figures") {
  ggplot2::ggsave(file.path(dir, paste0(name, ".png")), p, width = width, height = height,
                  dpi = 300, bg = "white", device = ragg::agg_png)
  f <- file.path(dir, paste0(name, ".pdf"))
  if (Sys.info()[["sysname"]] == "Darwin") grDevices::quartz(file = f, type = "pdf", width = width, height = height)
  else grDevices::cairo_pdf(f, width = width, height = height)
  print(p); invisible(grDevices::dev.off())
}
