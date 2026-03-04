# Darkly palette (Bootswatch) ----
  # source: https://github.com/thomaspark/bootswatch/blob/v5/dist/darkly/_variables.scss
  darkly_primary    <- "#375a7f"  # blue
  darkly_secondary  <- "#444444"  # gray-700
  darkly_body_bg    <- "#222222"  # gray-900
  darkly_body_color <- "#ffffff"  # text on dark bg
  darkly_success    <- "#00bc8c"  # teal
  darkly_info       <- "#3498db"  # cyan

# Darkly ggplot theme ----
theme_darkly <- function(
  base_size = 10,
  base_family = "Lato"
){
  theme_minimal(base_size = base_size, base_family = base_family, ink = darkly_body_color) +
    theme(
      # Base text
      text = element_text(
        family = base_family,
        colour = darkly_body_color,
        size = base_size,
        lineheight = 1.15
      ),

      # Titles
      plot.title = element_text(
        size   = base_size * 1.25,
        face   = "bold",
        margin = margin(b = 10),
        colour = darkly_body_color
      ),
      plot.subtitle = element_text(
        size   = base_size * 1.02,
        face   = "italic",
        colour = darkly_success,  # Darkly uses darkly_success/teal for links & accents
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size   = base_size * 0.72,
        colour = scales::alpha(darkly_body_color, 0.6),
        hjust  = 0
      ),
      plot.title.position = "plot",

      # Axes
      axis.title = element_text(size = base_size * 0.88),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(
        size = base_size * 0.78,
        colour = scales::alpha(darkly_body_color, 0.8)
      ),
      axis.ticks = element_blank(),

      # Grid (subtle, light lines on dark)
      panel.grid.major.y = element_line(
        colour   = scales::alpha(darkly_body_color, 0.10),
        linewidth = 0.4
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),

      # Backgrounds
      panel.background = element_rect(fill = darkly_body_bg, colour = NA),
      plot.background  = element_rect(fill = darkly_body_bg, colour = NA),

      # Legend
      legend.position = "bottom",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.title = element_text(
        size = base_size * 0.85,
        face = "bold",
        colour = darkly_body_color
      ),
      legend.text = element_text(size = base_size * 0.78, colour = darkly_body_color),
      legend.key  = element_blank(),

      # Facets
      strip.background = element_rect(
        fill = scales::alpha(darkly_secondary, 0.25),
        colour = NA
      ),
      strip.text = element_text(
        colour = darkly_body_color,
        face   = "bold",
        size   = base_size * 0.85
      ),

      # Margins
      plot.margin = margin(t = 18, r = 22, b = 22, l = 22)
    )
}

# Darkly gt theme ----
gt_theme_darkly <- function(data, base_size = 16.8, ...) {
  data |>
    opt_table_font(
      font = "Lato",
      stack = NULL,
      size = px(base_size)
    ) |>
    tab_options(
      data_row.padding = px(6),

      # Remove outer borders
      table.border.top.style    = "none",
      table.border.bottom.style = "none",
      table.border.left.style   = "none",
      table.border.right.style  = "none",

      # Background and striping tuned for dark mode
      table.background.color = darkly_body_bg,
      row.striping.background_color = scales::alpha(darkly_secondary, 0.75), 
      row.striping.include_table_body = TRUE,

      # No internal grid rules (keeps it clean)
      table_body.hlines.style = "none",
      table_body.vlines.style = "none"
    ) |>

    # Body cells
    tab_style(
      style = list(
        cell_text(
          color = darkly_body_color,
          weight = "400",
          size = px(base_size * 0.95),
          align = "left"
        )
      ),
      locations = cells_body()
    ) |>

    # Column labels
    tab_style(
      style = list(
        cell_text(
          weight = "700",
          color  = darkly_body_color,
          size   = px(base_size * 1.02)
        ),
        cell_borders(
          sides  = "bottom",
          color  = scales::alpha(darkly_body_color, 0.55),
          weight = px(1.5)
        )
      ),
      locations = cells_column_labels()
    ) |>

    # Title
    tab_style(
      style = list(
        cell_text(
          size   = px(base_size * 1.25),
          weight = "700",
          color  = darkly_body_color
        )
      ),
      locations = cells_title(groups = "title")
    ) |>

    # Subtitle
    tab_style(
      style = list(
        cell_text(
          style = "italic",
          size  = px(base_size * 1.00),
          color = darkly_success  # Darkly darkly_success (teal) as accent
        )
      ),
      locations = cells_title(groups = "subtitle")
    ) |>

    opt_align_table_header(align = "left") |>

    # Source notes
    tab_style(
      style = list(
        cell_text(
          color = scales::alpha(darkly_body_color, 0.65),
          transform = "uppercase",
          size = px(base_size * 0.68)
        )
      ),
      locations = cells_source_notes()
    )
}