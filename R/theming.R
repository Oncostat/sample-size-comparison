primary   <- "#086788"
secondary <- "#06AED5"
accent    <- "#F0C808"

body_bg    <- "#EDF5FF"
body_color <- "#1C2B3A"
dark       <- "#0F172A"

color_high   <- "#A7FFC4"
color_medium <- "#F0E292"
color_low    <- "#FFA194"

theme_ssc <- function(
  base_size = 14,
  base_family = "Inter"
){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(
        family = base_family,
        colour = body_color,
        size = base_size,
        lineheight = 1.15
      ),
      plot.title = element_text(
        size = base_size * 1.25,
        face = "bold",
        margin = margin(b = 10),
        colour = primary
      ),
      plot.subtitle = element_text(
        size = base_size * 1.02,
        face = "italic",
        colour = secondary,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = base_size * 0.72,
        colour = alpha(body_color, 0.6),
        hjust = 0
      ),
      plot.title.position = "plot",
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
        colour = alpha(body_color, 0.8)
      ),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(
        colour = alpha(primary, 0.10),
        linewidth = 0.4
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        fill = body_bg,
        colour = NA
      ),
      plot.background = element_rect(
        fill = body_bg,
        colour = NA
      ),
      legend.position = "right",
      legend.justification = "left",
      legend.background = element_blank(),
      legend.title = element_text(
        size = base_size * 0.85,
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.78
      ),
      legend.key = element_blank(),
      strip.background = element_rect(
        fill = alpha(secondary, 0.06),
        colour = NA
      ),
      strip.text = element_text(
        colour = primary,
        face = "bold",
        size = base_size * 0.85
      ),
      plot.margin = margin(
        t = 18,
        r = 22,
        b = 22,
        l = 22
      )
    )
}

gt_theme_ssc <- function(data, base_size = 16.8, ...) {
  data |>
  opt_table_font(
    font = "Inter",
    stack = NULL,
    size = px(base_size),
  ) |> 
  tab_options(
    data_row.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    table.background.color = body_bg,
    row.striping.background_color = alpha(primary, 0.035),
    row.striping.include_table_body = TRUE,
    table_body.hlines.style = "none",
    table_body.vlines.style = "none"
  ) |>
  tab_style(
    style = list(
      cell_text(
        color = body_color,
        weight = "400",
        size = px(base_size * 0.95),
        align = "left"
      )
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = list(
      cell_text(
        weight = "600",
        color = primary,
        size = px(base_size * 1.02)
      ),
      cell_borders(
        sides = "bottom",
        color = alpha(primary, 0.22),
        weight = px(1.5)
      )
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_text(
        size = px(base_size * 1.25),
        weight = "700",
        color = primary
      )
    ),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = list(
      cell_text(
        style = "italic",
        size = px(base_size * 1.0),
        color = secondary
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) |>
  opt_align_table_header(align = "left") |>
  tab_style(
    style = list(
      cell_text(
        color = alpha(body_color, 0.55),
        transform = "uppercase",
        size = px(base_size * 0.68)
      )
    ),
    locations = cells_source_notes()
  )
}

scale_color_ssc <- function(...) {scale_colour_brewer(palette = "Set1", ...)}
scale_fill_ssc <- function(...) {scale_fill_brewer(palette = "Set1", ...)}