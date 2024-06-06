


satf_blue = "#3179b4"
satf_lightblue = "#85c7d7"
satf_green = "#91b632"
satf_orange = "#ff8d13"
satf_red = "#ef4c55"
#



#'Plot a lovely horizontal bar chart
#'
#'Use this to make indiviudal or grouped bar charts with labels
#'
#'@param df A data frame
#'@param var the main variable to plot
#'@param group_var the grouping variable (optional)
#'@param title An optional title as a string
#'@param x_label Optional label for the x axis
#'@param y_label Optional label for the y axis
#'@param group Logical variable marked FALSE by default. To group the bars this must be set to TRUE
#'@param fill Optional fill for bar chart
#'@param slice logical, if plot should include a sample of the categories
#'@param slice_n optional number of top categories sliced
#'@param nudge_lab adjust position of label
#'@param sort_freq logical if data should be sorted by percentage
#'@param xrange optional range of x axis
#'
#'
#'@importFrom dplyr count mutate group_by n
#'@import ggplot2
bcb <- function(df, var, group_var = NULL, title = NULL, x_label = NULL, y_label = NULL, group = FALSE,
                           fill = NA, slice = TRUE, slice_n = 10, nudge_lab = -3, wrap = 26,
                           xrange = NULL, sort_freq = TRUE) {

  if (isFALSE(group)) {
    plot_df <- df |>
      dplyr::count({{var}}) |>
      dplyr::mutate(pct = round((n / sum(n)), 2) * 100) |>
      dplyr::slice_max(n = ifelse(isTRUE(slice), slice_n, nrow(df)), order_by = pct)

    if (isTRUE(sort_freq)) {
      plot_df <- plot_df |>
        dplyr::mutate({{var}} := factor({{var}}, levels = rev(unique({{var}}[order(pct)]))))
    }

    ggplot2::ggplot(plot_df, ggplot2::aes(y = {{var}}, x = pct)) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1), limits = xrange) +
      ggplot2::geom_col(fill = ifelse(is.na(fill), "#ff8d13", fill)) +
      ggplot2::geom_label(ggplot2::aes(label = paste(paste0(pct, "%"), paste0("(", n, ")"))), nudge_x = nudge_lab) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = ifelse(is.null(x_label), "", x_label), y = ifelse(is.null(y_label), "", y_label),
                    title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
                     plot.title = ggplot2::element_text(size = 18)) +
      ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = wrap))

  } else {
    plot_df <- df |>
      dplyr::count({{var}}, {{group_var}}) |>
      dplyr::group_by({{var}}) |>
      dplyr::mutate(pct = round((n / sum(n)), 2) * 100) |>
      dplyr::ungroup()

    if (isTRUE(sort_freq)) {
      plot_df <- plot_df |>
        dplyr::mutate({{var}} := factor({{var}}, levels = rev(unique({{var}}[order(pct)]))))
    }

    ggplot2::ggplot(plot_df, ggplot2::aes(y = {{var}}, x = pct, group = {{group_var}})) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      ggplot2::geom_col(ggplot2::aes(fill = {{group_var}}), position = "dodge") +
      ggplot2::geom_label(ggplot2::aes(label = paste(paste0(pct, "%"), paste0("(", n, ")"))), position = ggplot2::position_dodge(width = 1), hjust = 1.2) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(x = ifelse(is.null(x_label), "", x_label), y = ifelse(is.null(y_label), "", y_label),
                    title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
                     plot.title = ggplot2::element_text(size = 18))
  }
}


#'Plot a formatted histogram
#'
#'Use this to make individual or grouped histograms
#'
#'@param df A data frame
#'@param var the main variable to plot
#'@param group_var the grouping variable (optional)
#'@param group Logical variable marked FALSE by default. To group the bars this must be set to TRUE
#'
#'
#'@import ggplot2
bchist <- function(df, var, group_var = NULL, group = FALSE, binwidth = NULL, title = NULL, x_label = NULL, y_label = NULL, fill = "#3179b4",
                   color = "black", alpha = 0.8, wrap = 26, xrange = NULL, yrange = NULL) {

  if(isFALSE(group)){

    ggplot2::ggplot(df, ggplot2::aes(x = {{var}})) +
      ggplot2::geom_histogram(binwidth = binwidth, fill = fill, color = color, alpha = alpha) +
      ggplot2::scale_x_continuous(limits = xrange) +
      ggplot2::scale_y_continuous(limits = yrange) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(

        title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
                     plot.title = ggplot2::element_text(size = 18))

  }else{

    ggplot2::ggplot(df, ggplot2::aes(x = {{var}}, fill = {{group_var}})) +
      ggplot2::geom_histogram(binwidth = binwidth, color = color, alpha = alpha) +
      ggplot2::scale_x_continuous(limits = xrange) +
      ggplot2::scale_y_continuous(limits = yrange) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(

        title = ifelse(is.null(title), "", title)) +
      ggplot2::theme(plot.title.position = "plot",
                     plot.title = ggplot2::element_text(size = 18))


  }


}




