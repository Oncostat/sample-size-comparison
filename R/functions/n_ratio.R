get_n_ratio <- function(
  tbl_combined,
  ref = c("east", "nquery")
  ){
  ref <- arg_match(ref)
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
        "ahern"
      )),
    names_to = "method",
    values_to = "n_ratio"
  )
}

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

plot_n_ratio <- function(
  tbl_n_ratio,
  title,
  ref_name = c("East", "nQuery")
  ){
  ref_name <- arg_match(ref_name)
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
    subtitle = paste0("According to ", ref_name, " sample sizes"),
    x = paste0("N ", ref_name),
    y = "N-Ratio",
    color = "Methods : "
  ) +
  scale_color_ssc() +
  facet_relevancy()
}

# N-Ratio gt ----
gt_n_ratio <- function(
  tbl_n_ratio, 
  title, 
  ref_name = c("East", "nQuery"),
  decimals = 2){
  ref_name <- arg_match(ref_name)
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
    )

  use_red <- any(nr_ratio_clean$Q1 < 1 - er_rate | nr_ratio_clean$Q3 > 1 + er_rate)

  gt_nr <- 
    nr_ratio_clean |> 
    gt(rowname_col = c("method")) |>  
    tab_header(
      title = title,
      subtitle =  paste0("According to ", ref_name, " sample sizes"),
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
        source_note = "Red : < 50% of N-ratios ±10%"
      )
  }

  gt_nr
}
