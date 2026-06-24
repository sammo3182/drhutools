#' @name cdplot
#' @title Visualizing the experimental outcome with cumulative distribution functions.
#' @description Packed \code{ggplot2} function to compare the empirical cumulative distribution functions (ECDF) between the treatment and control groups in an experiment or quasi-experiment.
#'
#' @param data A data.frame including two columns, one recording the outcome and the other recording the assignment. The assignment column must be named as `group`.
#' @param ks_test A logical option to indicate whether to show the Kolmogorov-Smirnov test result in the bottom-right corner. The default value is FALSE.
#' @param point_size An integer to indicate the size of the points at the largest difference. The default value is 3.
#' @param point_color An character or function to indicate the color of the points at the largest difference. The default value is `drhutools::gb_cols("red")`.
#' @param link_color An character or function to indicate the color of the link at the largest difference. The default value is `drhutools::gb_cols("red")`.
#'
#'
#' @importFrom stats ecdf median ks.test
#' @import ggplot2
#' @import purrr
#' @importFrom utils globalVariables combn
#'
#' @return A list of `ggplot2` objects comparing the ECDFs between the control and treatment groups and identifying at most three largest differences.
#'
#' @examples
#'
#' data("PlantGrowth")
#'
#' plot_plant <- cdplot(PlantGrowth, ks_test = TRUE)
#' plot_plant
#'
#' @export

globalVariables(c("KSD", "group"))

cdplot <- function(data,
                   ks_test = FALSE,
                   point_size = 3,
                   point_color = gb_cols("red"),
                   link_color = gb_cols("red")) {
  ls_result <- tb_ks(data)

  map2(
    ls_result$ls_compare,
    ls_result$result_ks,
    ~ cd_plot(.x, .y, point_size, point_color, link_color, ks_test)
  )
}


# Split the outcome by `group` and run a two-sample KS test on every pair of
# groups. Returns the paired outcome vectors and a label vector
# c(group1, group2, ks-annotation) for each comparison.
tb_ks <- function(data) {
  outcome_col <- setdiff(names(data), "group")[1]
  groups <- split(data[[outcome_col]], data$group)
  group_names <- names(groups)
  pairs <- combn(seq_along(groups), 2, simplify = FALSE)

  ls_compare <- lapply(pairs, function(p) list(groups[[p[1]]], groups[[p[2]]]))

  result_ks <- lapply(pairs, function(p) {
    kt <- ks.test(groups[[p[1]]], groups[[p[2]]], exact = TRUE)
    c(
      group_names[p[1]],
      group_names[p[2]],
      sprintf("KS Test:%.2f\n (p: %.2f)", kt$statistic, kt$p.value)
    )
  })

  list(result_ks = result_ks, ls_compare = ls_compare)
}

cd_plot <- function(comparison, labels,
                    point_size, point_color, link_color, ks_test) {
  sample1 <- comparison[[1]]
  sample2 <- comparison[[2]]
  df_combine <- data.frame(
    KSD = c(sample1, sample2),
    group = c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
  )

  # ECDFs and the location(s) of greatest vertical distance between them.
  cdf1 <- ecdf(sample1)
  cdf2 <- ecdf(sample2)
  grid <- seq(min(sample1, sample2), max(sample1, sample2), length.out = length(sample1))
  gap <- abs(cdf1(grid) - cdf2(grid))
  x0 <- grid[gap == max(gap)]
  if (length(x0) > 3) x0 <- median(x0)
  y0 <- cdf1(x0)
  y1 <- cdf2(x0)

  plot_ecdf <-
    ggplot(df_combine, aes(x = KSD, group = group, color = group)) +
    stat_ecdf(linewidth = 1) +
    xlab("Values") +
    ylab("ECDF") +
    scale_color_gb(labels = c(labels[1], labels[2])) +
    theme(legend.title = element_blank())

  # Mark each greatest-distance location with a dashed link and two end points.
  for (i in seq_along(x0)) {
    plot_ecdf <- plot_ecdf +
      annotate("segment",
        x = x0[i], xend = x0[i], y = y0[i], yend = y1[i],
        linetype = "dashed", color = link_color
      ) +
      annotate("point", x = x0[i], y = y0[i], color = point_color, size = point_size) +
      annotate("point", x = x0[i], y = y1[i], color = point_color, size = point_size)
  }

  if (ks_test) plot_ecdf <- plot_ecdf + labs(caption = labels[3])

  plot_ecdf
}
