# Load packages ----
suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(gt)
  library(S7)
  library(plotly)
}))

# Source S7 classes and methods ----
source("../R/S7/generics.R")
source("../R/S7/ssc_design.R")
source("../R/S7/ssc_results.R")
source("../R/functions/checks.R")
source("../R/functions/evaluate_relevancy.R")

# Load results ----
ssc <- read_rds("../ssc2.rds")
ssc$bin$fixed_exact <- read_rds("../ssc.rds")$bin$fixed_exact #cannot execute the whole pipeline for the moment

# Error rate ----
er_rate <- 0.1 # 10%

# Clean Theme colors ----
jet <- "#131516"
accent <- "#107895"
accent2 <- "#9a2515"
grey_border <- "#D3D3D3"
caption_grey <- "#666666"

# Relevancy colors ----
color_high <- "#a7ffc4ff"
color_medium <- "#f0e292ff"
color_low <- "#ffa194ff"

# Clean Theme ggplot::theme() ----
theme_clean <- function(base_size = 17, base_family = "Roboto") {
  theme_minimal(base_size = base_size, base_family = base_family) +
  theme(
    # Main text
    text = element_text(
      family = base_family,
      face = "plain",
      colour = jet,
      size = base_size
    ),

    # Title
    plot.title = element_text(
      size = base_size * 1.2,
      face = "plain",
      colour = jet,
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",

    # Subtitle
    plot.subtitle = element_text(
      size = base_size,
      face = "italic",
      colour = accent,
      margin = margin(b = 15)
    ),

    # Caption
    plot.caption = element_text(
      size = base_size * 0.7,
      colour = caption_grey
    ),
    
    # Axis
    axis.title = element_text(
      face = "plain",
      size = base_size * 0.8
    ),
    axis.text = element_text(
      face = "plain",
      size = base_size * 0.7
    ),
    axis.ticks = element_blank(),
    
    # Grid
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = grey_border,
      linewidth = 0.5
    ),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    
    # Legend
    legend.position = "bottom",
    legend.justification='left', 
    legend.title = element_text(
      face = "plain",
      colour = jet,
      size = base_size * 0.9
    ),
    legend.text = element_text(
      face = "plain",
      size = base_size * 0.8
    ),
    legend.key = element_blank(),

    # Facet
    strip.background = element_blank(),
    strip.text = element_text(
      face = "plain",
      colour = accent,
      size = base_size * 0.9
    ),
    
    # Margin
    plot.margin = margin(5, 20, 20, 20)
  )
}

# Scale with Brewer palette "Set1" ----
scale_color_clean <- function(...) {scale_colour_brewer(palette = "Set1", ...)}
scale_fill_clean <- function(...) {scale_fill_brewer(palette = "Set1", ...)}


# Clean gt theme ----
gt_theme_clean <- function(data, base_size = 17, ...) {
  data |>
  # Main font
  tab_options(
    table.font.names = "Roboto",
    table.font.size = px(base_size),
    data_row.padding = px(4),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  ) |>
  # Cells
  tab_style(
    style = list(
      cell_text(
        color = jet,
        weight = "300"
      )
    ),
    locations = cells_body()
  ) |>
  # Columns labels
  tab_style(
    style = list(
      cell_text(
        weight = "600",
        color = jet
      ),
      cell_borders(
        sides = "bottom",
        color = grey_border,
        weight = px(2)
      )
    ),
    locations = cells_column_labels()
  ) |>
  # Title
  tab_style(
    style = list(
      cell_text(
        size = px(base_size*1.2),
        weight = "400",
        color = jet
      )
    ),
    locations = cells_title(groups = "title")
  ) |>
  # Subtitle
  tab_style(
    style = list(
      cell_text(
        style = "italic",
        size = px(base_size),
        color = accent,
        weight = "400"
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) |>
  opt_align_table_header(align = "left") |> # Align Title and subtitle left
  # Caption
  tab_style(
    style = list(
      cell_text(
        color = caption_grey,
        transform = "uppercase",
        size = px(0.7*base_size)
      )
    ),
    locations = cells_source_notes()
  ) |>
  # Bottom border
  tab_options(
    table_body.border.bottom.color = grey_border,
    table_body.border.bottom.width = px(2)
  )
}

# Facet relevancy ----
relevancy_strips <- ggh4x::strip_themed(
  background_x = ggh4x::elem_list_rect(
    fill = c(color_high, color_medium, color_low)),
  by_layer_x = FALSE
)
facet_relevancy <- function(...){
  ggh4x::facet_wrap2(
    ~ relevancy, 
    strip = relevancy_strips, 
    scales = "free",
    ...
  )
}

# N-Ratio Tibble function ----
get_n_ratio <- function(tbl_combined, ref = "east"){
  ref_col_names <- paste0("n_", ref)
  ref_col <- tbl_combined[[ref_col_names]] |> na.omit()
  tbl_combined |> 
  select(-starts_with("e_")) |> 
  drop_na(all_of(ref_col_names)) |> 
  mutate(
    across(
      (starts_with("n_") & !all_of(ref_col_names)),
      ~ ./ref_col,
      .names = "{sub('^n_', '', .col)}"
    )
  ) |> 
  select(-(starts_with("n_") & !all_of(ref_col_names))) |> 
  pivot_longer(
    cols = any_of(
      c(
        "rpact",
        "nquery",
        "east",
        "rashnu",
        "gsdesign2",
        "bbssr",
        "oa2s",
        "sssas",
        "ahern",
        "combined"
      )),
    names_to = "method",
    values_to = "n_ratio"
  )
}

# N-Ratio plot ----
plot_n_ratio <- function(
  tbl_n_ratio,
  title,
  ref_name = "East"
  ){
  # Reference column
  ref_col <- tbl_n_ratio |> select(starts_with("n_"), -"n_ratio")  |> pull()

  # Plot
  ggplot(tbl_n_ratio) +
  aes(x = ref_col, y = .data[["n_ratio"]], color = .data[["method"]]) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1 - er_rate, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 1 + er_rate,  linetype = "dashed") +
  labs(
    title = title,
    subtitle = paste0("Par rapport aux valeurs de ", ref_name),
    x = paste0("N ", ref_name),
    y = "N-Ratio",
    color = "Méthodes : "
  ) +
  scale_color_clean() +
  facet_relevancy()
}

# N-Ratio gt ----
gt_n_ratio <- function(
  tbl_n_ratio, 
  title, 
  ref_name = "East", 
  digit = 2){
  
  nr_ratio_clean <- 
    tbl_n_ratio |> 
    group_by(relevancy, method) |> 
    summarise(
      Min = min(n_ratio, na.rm = TRUE),
      Q1 = quantile(n_ratio, probs = 0.25, na.rm = TRUE),
      Moyenne = mean(n_ratio, na.rm = TRUE), 
      Médiane = median(n_ratio, na.rm = TRUE), 
      Q3 = quantile(n_ratio, probs = 0.75, na.rm = TRUE),
      Max = max(n_ratio, na.rm = TRUE)
    ) |> 
    mutate(across(where(is.numeric), ~ signif(., digit = digit)))

  use_red <- any(nr_ratio_clean$Q1 < 1 - er_rate | nr_ratio_clean$Q3 > 1 + er_rate)

  gt_nr <- 
    nr_ratio_clean |> 
    gt(rowname_col = c("method")) |> 
    gt_theme_clean() |> 
    tab_header(
      title = title,
      subtitle = paste0("Par rapport aux valeurs de ", ref_name)
    ) |>  
    tab_stubhead(label = "Pertinence") |>  
    tab_style(
      style = list(
        cell_fill(color = color_low)
      ),
      location = cells_row_groups("low")
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = color_medium)
      ),
      location = cells_row_groups("medium")
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = color_high)
      ),
      location = cells_row_groups("high")
    )

  if(use_red){
    gt_nr <- 
      gt_nr |> 
      tab_style(
        style = list(
          cell_fill(color = "#9A2515"),
          cell_text(style = "italic", color = "white")
          ),
        locations = cells_body(
          rows = (Q1 < 1 - er_rate | Q3 > 1 + er_rate)
        )
      ) |> 
      tab_source_note(
        source_note = "Rouge : < 50% des ratios dans ±10%"
      )
  }

  gt_nr
}
