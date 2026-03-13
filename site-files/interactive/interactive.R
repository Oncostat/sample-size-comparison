# LOAD PACKAGES AND THEMING ----
suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(gt)
  library(S7)
  library(plotly)
  library(crosstalk)
  library(bslib)
}))

source("../theming_flatly.R")
source("../theming_darkly.R")
# PLOT INTERACTIVE ----
plot_interactive <- function(tbl_n_ratio, type, ref = c("east", "nquery")) {
  ref = rlang::arg_match(ref)
  ref_col_name = if_else(ref == "east", "n_east", "n_nquery")
  ref_name = if_else(ref == "east", "East", "nQuery")

  p <- ggplot(tbl_n_ratio)

  if (type == "bin") {
    p <- p + aes("delta pi:" = delta_pi, "pi_c" = pi_c)
  }

  if (type == "surv") {
    p <- p + aes("HR:" = hr, "surv_t" = surv_t)
  }

  p +
    aes(
      x = .data[[ref_col_name]],
      y = .data[["n_ratio"]],
      color = .data[["method"]],
      "α:" = alpha,
      "Power:" = power
    ) +
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 1 - er_rate, linetype = "dashed") +
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = 1 + er_rate, linetype = "dashed") +
    labs(
      caption = paste0("According to ", ref_name, " sample sizes"),
      x = paste0("N ", ref_name),
      y = "N-Ratio",
      color = "Methods : "
    ) +
    scale_colour_brewer(palette = "Set1")
}


# FILTERS INTERACTIVE ----
filters_interactive <- function(shared_data, type) {
  if (type == "bin") {
    filters <-
      list(
        filter_checkbox("relevancy", "Relevancy:", shared_data, ~relevancy),
        filter_checkbox("alpha", "α:", shared_data, ~alpha),
        filter_checkbox("power", "Power:", shared_data, ~power),
        filter_checkbox("pi_c", "Control proportion:", shared_data, ~pi_c),
        filter_checkbox("delta_pi", "Delta-proportion:", shared_data, ~delta_pi)
      )
  }
  if (type == "surv") {
    filters <-
      list(
        filter_checkbox("relevancy", "Pertinence :", shared_data, ~relevancy),
        filter_checkbox("alpha", "α :", shared_data, ~alpha),
        filter_checkbox("power", "Puissance :", shared_data, ~power),
        filter_checkbox("hr", "Hazard ratio :", shared_data, ~hr),
        filter_checkbox("surv_t", "Survie à 3 ans :", shared_data, ~surv_t)
      )
  }

  filters
}

# BUILD INTERACTIVE OBJECT ----
build_interactive <- function(
  shared_data,
  title,
  type,
  ref = c("east", "nquery")
) {
  list(
    title = title,
    plot = plot_interactive(shared_data, type = type, ref = ref),
    filters = filters_interactive(shared_data, type = type)
  )
}
# CARD FLATLY ----
card_flatly <- function(interactive_object) {
  filename <-
    interactive_object$title |>
    str_to_lower() |>
    str_replace_all(" ", "_")

  gsub(" ", "_", tolower(interactive_object$title))
  card(
    full_screen = TRUE,
    card_header(interactive_object$title),
    layout_sidebar(
      class = "p-0", # zero padding
      scrollable = FALSE,
      height = 500,
      sidebar = sidebar(
        bg = flatly_body_bg,
        width = 200,
        class = "bslib-sidebar",
        interactive_object$filters
      ),
      card_body(
        ggplotly(
          interactive_object$plot + theme_flatly(base_size = 11),
          dynamicTicks = TRUE
        ) |>
          config(
            modeBarButtonsToRemove = c(
              'zoom',
              'pan',
              'select',
              'zoomIn',
              'zoomOut',
              'resetScale',
              'lasso2d',
              'hoverClosestCartesian',
              'hoverCompareCartesian'
            ),
            displaylogo = FALSE,
            toImageButtonOptions = list(filename = filename)
          )
      )
    )
  )
}

# CARD DARKLY ----
card_darkly <- function(interactive_object) {
  filename <-
    interactive_object$title |>
    str_to_lower() |>
    str_replace_all(" ", "_")

  card(
    full_screen = TRUE,
    card_header(interactive_object$title),
    layout_sidebar(
      class = "p-0", # zero padding
      scrollable = FALSE,
      height = 500,
      sidebar = sidebar(
        bg = darkly_body_bg,
        width = 200,
        class = "bslib-sidebar",
        interactive_object$filters
      ),
      card_body(
        ggplotly(
          interactive_object$plot + theme_darkly(base_size = 11),
          dynamicTicks = TRUE
        ) |>
          config(
            modeBarButtonsToRemove = c(
              'zoom',
              'pan',
              'select',
              'zoomIn',
              'zoomOut',
              'resetScale',
              'lasso2d',
              'hoverClosestCartesian',
              'hoverCompareCartesian'
            ),
            displaylogo = FALSE,
            toImageButtonOptions = list(filename = filename)
          )
      )
    )
  )
}
