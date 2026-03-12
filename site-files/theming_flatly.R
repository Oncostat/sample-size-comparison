#* GGPLOT2 and GT Themes for Flatly

# Flatly palette (Bootswatch) ----
# source: https://github.com/thomaspark/bootswatch/blob/v5/dist/flatly/_variables.scss
flatly_primary <- "#2C3E50"
flatly_secondary <- "#18BC9C"
flatly_body_color <- "#2C3E50"
flatly_body_bg <- "#FFFFFF"

# Flatly ggplot theme ----
theme_flatly <- function(
  base_size = 10,
  base_family = "Lato"
) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    ink = flatly_body_color
  ) +
    theme(
      text = element_text(
        family = base_family,
        colour = flatly_body_color,
        size = base_size,
        lineheight = 1.15
      ),

      # Titles
      plot.title = element_text(
        size = base_size * 1.25,
        face = "bold",
        margin = margin(b = 10),
        colour = flatly_primary
      ),
      plot.subtitle = element_text(
        size = base_size * 1.02,
        face = "italic",
        colour = flatly_secondary,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = base_size * 0.72,
        colour = scales::alpha(flatly_body_color, 0.6),
        hjust = 0
      ),

      plot.title.position = "plot",

      # Axes
      axis.title = element_text(
        size = base_size * 0.88
      ),
      axis.title.x = element_text(
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        size = base_size * 0.78,
        colour = scales::alpha(flatly_body_color, 0.8)
      ),
      axis.ticks = element_blank(),

      # Grid
      panel.grid.major.y = element_line(
        colour = scales::alpha(flatly_primary, 0.12),
        linewidth = 0.4
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      # Backgrounds
      panel.background = element_rect(
        fill = flatly_body_bg,
        colour = NA
      ),
      plot.background = element_rect(
        fill = flatly_body_bg,
        colour = NA
      ),

      # Legend
      legend.position = "bottom",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.title = element_text(
        size = base_size * 0.85,
        face = "bold",
        colour = flatly_primary
      ),
      legend.text = element_text(
        size = base_size * 0.78
      ),
      legend.key = element_blank(),

      # Facets
      strip.background = element_rect(
        fill = scales::alpha(flatly_secondary, 0.10),
        colour = NA
      ),
      strip.text = element_text(
        colour = flatly_primary,
        face = "bold",
        size = base_size * 0.85
      ),

      # Margins
      plot.margin = margin(
        t = 18,
        r = 22,
        b = 22,
        l = 22
      )
    )
}

# Flatly gt theme ----

gt_theme_flatly <- function(data, base_size = 16.8, ...) {
  data |>
    opt_table_font(
      font = "Lato",
      stack = NULL,
      size = px(base_size)
    ) |>
    tab_options(
      data_row.padding = px(6),
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.left.style = "none",
      table.border.right.style = "none",
      table.background.color = flatly_body_bg,

      # Soft teal row striping, very light
      row.striping.background_color = scales::alpha(flatly_primary, 0.035),
      row.striping.include_table_body = TRUE,

      # No internal rules
      table_body.hlines.style = "none",
      table_body.vlines.style = "none"
    ) |>

    # Body cells
    tab_style(
      style = list(
        cell_text(
          color = flatly_body_color,
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
          weight = "600",
          color = flatly_primary,
          size = px(base_size * 1.02)
        ),
        cell_borders(
          sides = "bottom",
          color = scales::alpha(flatly_primary, 0.22),
          weight = px(1.5)
        )
      ),
      locations = cells_column_labels()
    ) |>

    # Title
    tab_style(
      style = list(
        cell_text(
          size = px(base_size * 1.25),
          weight = "700",
          color = flatly_primary
        )
      ),
      locations = cells_title(groups = "title")
    ) |>

    # Subtitle
    tab_style(
      style = list(
        cell_text(
          style = "italic",
          size = px(base_size * 1.00),
          color = flatly_secondary
        )
      ),
      locations = cells_title(groups = "subtitle")
    ) |>

    opt_align_table_header(align = "left") |>

    # Source notes
    tab_style(
      style = list(
        cell_text(
          color = scales::alpha(flatly_body_color, 0.55),
          transform = "uppercase",
          size = px(base_size * 0.68)
        )
      ),
      locations = cells_source_notes()
    )
}
