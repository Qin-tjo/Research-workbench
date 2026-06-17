## Shared plot styling for the Target Intelligence pipeline.
## Publication-quality theme + helpers for wrapping long subtitles/captions.

suppressPackageStartupMessages({
  library(ggplot2)
})

# Wrap long text strings to a target width, returning a single newline-joined
# string suitable for plot.title / plot.subtitle / plot.caption.
wrap_text <- function(x, width = 120L) {
  if (is.null(x) || !nzchar(x)) return(x)
  paste(strwrap(x, width = width), collapse = "\n")
}

# Consistent theme used by all stage plotters.
theme_target_intel <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title       = element_text(face = "bold", size = base_size + 3,
                                      color = "#1a1a18",
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "grey35",
                                      size = base_size - 0.5,
                                      lineheight = 1.2,
                                      margin = margin(b = 12)),
      plot.caption     = element_text(color = "grey45",
                                      size = base_size - 2,
                                      hjust = 0, lineheight = 1.25,
                                      margin = margin(t = 12)),
      panel.grid.minor = element_blank(),
      legend.text      = element_text(size = base_size - 1),
      legend.title     = element_text(size = base_size - 1, face = "bold"),
      strip.text       = element_text(size = base_size - 1, face = "bold",
                                      color = "#1a1a18"),
      plot.margin      = margin(14, 18, 12, 14)
    )
}
