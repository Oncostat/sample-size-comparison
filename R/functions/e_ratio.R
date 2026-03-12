# Get E-Ratio ----
get_e_ratio <- function(
  tbl_combined,
  ref = c("east", "nquery")
) {
  ref <- arg_match(ref)
  ref_col_names <- paste0("e_", ref)
  ref_col <- tbl_combined[[ref_col_names]] |> na.omit()

  tbl_combined |>
    select(-starts_with("n_")) |>
    drop_na(all_of(ref_col_names)) |>
    mutate(
      across(
        (starts_with("e_") & !all_of(ref_col_names)),
        ~ . / ref_col,
        .names = "{sub('^e_', '', .col)}"
      )
    ) |>
    select(-(starts_with("e_") & !all_of(ref_col_names))) |>
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
          "ahern"
        )
      ),
      names_to = "method",
      values_to = "e_ratio"
    )
}

# Plot E-Ratio ----
## Plotting function ----
plot_e_ratio <- function(
  tbl_e_ratio,
  title,
  ref_name = c("East", "nQuery")
) {
  ref_name <- arg_match(ref_name)
  # Reference column
  ref_col <- tbl_e_ratio |> select(starts_with("e_"), -"e_ratio") |> pull()

  # Plot
  ggplot(tbl_e_ratio) +
    aes(x = ref_col, y = .data[["e_ratio"]], color = .data[["method"]]) +
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 1 - er_rate, linetype = "dashed") +
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = 1 + er_rate, linetype = "dashed") +
    labs(
      title = title,
      subtitle = paste0("According to ", ref_name, " sample sizes"),
      x = paste0("E ", ref_name),
      y = "E-Ratio",
      color = "Methods : "
    ) +
    scale_color_ssc() +
    facet_relevancy()
}

# GT E-Ratio ----
gt_e_ratio <- function(
  tbl_e_ratio,
  title,
  ref_name = c("East", "nQuery"),
  decimals = 2
) {
  ref_name <- arg_match(ref_name)
  e_ratio_clean <-
    tbl_e_ratio |>
    group_by(relevancy, method) |>
    summarise(
      Min = min(e_ratio, na.rm = TRUE),
      Q1 = quantile(e_ratio, probs = 0.25, na.rm = TRUE),
      Mean = mean(e_ratio, na.rm = TRUE),
      Median = median(e_ratio, na.rm = TRUE),
      Q3 = quantile(e_ratio, probs = 0.75, na.rm = TRUE),
      Max = max(e_ratio, na.rm = TRUE)
    )

  use_red <- any(
    e_ratio_clean$Q1 < 1 - er_rate | e_ratio_clean$Q3 > 1 + er_rate
  )

  gt_er <-
    e_ratio_clean |>
    gt(rowname_col = c("method")) |>
    tab_header(
      title = title,
      subtitle = paste0("According to ", ref_name, " sample sizes"),
    ) |>
    tab_stubhead(label = "Relevancy") |>
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
    ) |>
    fmt_number(decimals = decimals)

  if (use_red) {
    gt_er <-
      gt_er |>
      tab_style(
        style = list(
          cell_fill(color = red),
          cell_text(style = "italic", color = "white")
        ),
        locations = cells_body(
          rows = (Q1 < 1 - er_rate | Q3 > 1 + er_rate)
        )
      ) |>
      tab_source_note(
        source_note = "Red : < 50% of N-ratios ±10%"
      )
  }

  gt_er
}
