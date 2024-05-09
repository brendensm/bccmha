#'Plot a lovely horizontal bar chart
#'
#'Use this to make indiviudal or grouped bar charts with labels
#'
#'@param df A data frame
#'@param var the main variable to plot
#'@param group_var the grouping variable (optional)
#'@param title An optional title as a string
#'@param x_label Optional label for the x axis
#'@param group Logical variable marked FALSE by default. To group the bars this must be set to TRUE
#'
#'
#'
#'@importFrom dplyr count mutate group_by n
#'@import ggplot2


nicebar <- function(df, var, group_var = NULL, title = NULL, x_label = NULL, group = F,
                    fill = NA, slice = T, slice_n = 10, nudge_lab = -3, wrap = 26,
                    xrange = NULL){


  if(isFALSE(group)){

    df |>
      dplyr::count({{var}}) |>
      dplyr::mutate(pct = round((n/sum(n)), 2)*100) |>
      dplyr::slice_max(n = ifelse(isTRUE(slice), slice_n, nrow(df)), order_by = pct) |>
      ggplot2::ggplot(ggplot2::aes(y = reorder({{var}}, pct), x = pct)) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1),
                                  limits = xrange) +
      ggplot2::geom_col(fill = ifelse(is.na(fill), "#ff8d13", fill)) +

      ggplot2::geom_label(ggplot2::aes(label = paste(paste0(pct, "%"), paste0("(", n, ")"))), nudge_x = nudge_lab) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = "Percentage", y = ifelse(is.null(x_label), "", x_label),
           title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
            plot.title = ggplot2::element_text(size = 18, face = "bold")) +
      scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = wrap))


  }else{

    df |>
      dplyr::count({{var}}, {{group_var}})  |>
      dplyr::group_by({{var}}) |>
      dplyr::mutate(pct = round((n/sum(n)), 2)*100) |>
      dplyr::ungroup() |>
      ggplot2::ggplot(ggplot2::aes(y = stats::reorder({{var}}, pct), x = pct, group = {{group_var}})) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      ggplot2::geom_col(ggplot2::aes(fill = {{group_var}}), position = "dodge") +
      ggplot2::geom_label(aes(label = paste(paste0(pct, "%"), paste0("(", n, ")"))), position = position_dodge(width = 1), hjust = 1.2) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = "Percentage", y = ifelse(is.null(x_label), "", x_label),
           title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
            plot.title = ggplot2::element_text(size = 18, face = "bold"))

  }

}


satf_blue = "#3179b4"
satf_lightblue = "#85c7d7"
satf_green = "#91b632"
satf_orange = "#ff8d13"
satf_red = "#ef4c55"
#
# palmerpenguins::penguins |>
#   dplyr::count(species, island) #|>
#   ggplot(aes(y = reorder(species, n), x = n, group = island)) +
#   geom_col(position = "dodge", aes(fill = island)) +
#   geom_label(aes(label = n), position = position_dodge(width = 1), hjust = 1)
#



#
#
# penguins |>
#   dplyr::count(species) |>
#   dplyr::mutate(pct = round((n/sum(n)), 2)*100) |>
#   ggplot(aes(y = reorder(species, pct), x = pct)) +
#
#   geom_col(aes(fill = species)) +
#   scale_x_continuous(labels = scales::label_percent(scale = 1)) +
#   geom_label(aes(label = paste(paste0(pct, "%"), paste0("(", n, ")"))), nudge_x = -4) +
#   theme_minimal(base_size = 14) +
#   scale_fill_manual(values = bccmha_palettes_discrete$SATF)
