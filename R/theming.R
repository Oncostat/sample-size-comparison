primary = "#086788"
secondary = "#06AED5" 
accent = "#F0C808" 
body_bg = "#FBFEFF" 
body_color = "#182635" 
dark = "#0F172A"


color_high <- "#a7ffc4ff"
color_medium <- "#f0e292ff"
color_low <- "#ffa194ff"

theme_ssc <- function(
  base_size = 17,
  base_family = "Inter"
) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(
        family = base_family,
        colour = "#1A2B3C",
        size = base_size,
        lineheight = 1.2
      ),
      plot.title = element_text(
        size = base_size * 1.4,
        face = "bold",
        margin = margin(b = 12),
        colour = primary
      ),
      plot.subtitle = element_text(
        size = base_size * 1.05,
        face = "italic",
        colour = secondary,
        margin = margin(b = 18)
      ),
      plot.caption = element_text(
        size = base_size * 0.75,
        colour = "#6B7280",
        hjust = 0
      ),
      plot.title.position = "plot",
      axis.title = element_text(
        size = base_size * 0.9,
        face = "plain"
      ),
      axis.title.x = element_text(
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        margin = margin(r = 10)
      ),
      axis.text = element_text(
        size = base_size * 0.75,
        colour = "#334155"
      ),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(
        colour = alpha(primary, 0.12),
        linewidth = 0.45
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        fill = "#FAFDFF",
        colour = NA
      ),
      plot.background = element_rect(
        fill = "#FAFDFF",
        colour = NA
      ),
      legend.position = "bottom",
      legend.justification = "left",
      legend.background = element_rect(
        fill = alpha("white", 0.7),
        colour = alpha(primary, 0.15),
        linewidth = 0.6
      ),
      legend.title = element_text(
        size = base_size * 0.9,
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.8
      ),
      legend.key = element_blank(),
      strip.background = element_rect(
        fill = alpha(secondary, 0.08),
        colour = NA
      ),
      strip.text = element_text(
        colour = primary,
        face = "bold",
        size = base_size * 0.9
      ),
      plot.margin = margin(
        t = 20,
        r = 25,
        b = 25,
        l = 25
      )
    )
}

gt_theme_ssc <- function(data, base_size = 17, ...) {

  data |>
  tab_options(

    table.font.names = "Inter",
    table.font.size = px(base_size),

    data_row.padding = px(6),

    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",

    table.background.color = "#FAFDFF"
  ) |>

  tab_style(

    style = list(
      cell_text(
        color = "#1A2B3C",
        weight = "300",
        size = px(base_size * 0.95),
        align = "left"
      )
    ),

    locations = cells_body()
  ) |>

  tab_style(

    style = list(

      cell_text(
        weight = "700",
        color = primary,
        size = px(base_size * 1.05)
      ),

      cell_borders(
        sides = "bottom",
        color = alpha(primary, 0.25),
        weight = px(2)
      )

    ),

    locations = cells_column_labels()
  ) |>

  tab_style(

    style = list(
      cell_text(
        size = px(base_size * 1.35),
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
        size = px(base_size * 1.05),
        color = secondary
      )
    ),

    locations = cells_title(groups = "subtitle")
  ) |>

  opt_align_table_header(align = "left") |>

  tab_style(

    style = list(
      cell_text(
        color = "#6B7280",
        transform = "uppercase",
        size = px(base_size * 0.7)
      )
    ),

    locations = cells_source_notes()
  ) |>

  tab_options(

    row.striping.background_color = "#F3F9FF",
    row.striping.include_table_body = TRUE,

    table_body.border.bottom.color = alpha(primary, 0.18),
    table_body.border.bottom.width = px(2)
  ) |>

  tab_options(
    table_body.hlines.style = "none",
    table_body.vlines.style = "none"
  )
}
