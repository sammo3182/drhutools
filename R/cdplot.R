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
#' @importFrom stats ecdf median
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @importFrom utils globalVariables
#' @importFrom utils combn
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

globalVariables(c("KSD", "group", "D", "p.value"))

cdplot <- function(data,
                   ks_test = FALSE,
                   point_size = 3,
                   point_color = gb_cols("red"),
                   link_color = gb_cols("red")) {
  ls_result <- tb_ks(data)

  ls_plot <- map2(
    ls_result$ls_compare,
    ls_result$result_ks,
    ~ cd_plot(.x, .y, point_size, point_color, link_color, ks_test)
  )
  return(ls_plot)
}


tb_ks <- function(data) {
  df_compare <- group_split(data, group)

  vec_combine <- seq(length(df_compare)) %>%
    combn(m = 2, simplify = FALSE)

  ## KS test

  ls_ks <- map(vec_combine, ~ (bind_rows(df_compare[.])))

  name_group <- unique(data$group) %>%
    combn(m = 2, simplify = FALSE) %>%
    map(~ tribble(~group1, ~group2, as.character(.)[1], as.character(.)[2]))

  result_ks <- map2_dfr(ls_ks, name_group, ~ {
    m_ks <- paste(names(.x), collapse = " ~ ") %>%
      as.formula()
    result_ks <- ks.test(
      formula = m_ks,
      data = .x,
      exact = TRUE,
      simulate.p.value = TRUE
    )

    tb_ks <- tibble(
      D = result_ks$statistic,
      p.value = result_ks$p.value,
      result = paste0(
        "KS Test:",
        format(round(D, digits = 2), nsmall = 2),
        "\n (p: ",
        format(round(p.value, digits = 2), nsmall = 2),
        ")"
      )
    ) %>%
      bind_cols(.y, .)
  }) %>%
    select(-D, -p.value) %>%
    t() %>%
    as_tibble(.name_repair = "minimal")

  ## data prepare

  df_pair <- group_split(data, group, .keep = FALSE)

  ls_compare <- map(vec_combine, ~ df_pair[.] %>% map(pull))

  return(list(result_ks = result_ks, ls_compare = ls_compare))
}

cd_plot <- function(comparison, labels,
                    point_size = point_size,
                    point_color = point_color,
                    link_color = link_color,
                    ks_test = ks_test) {
  sample1 <- comparison[[1]]
  sample2 <- comparison[[2]]
  group <-
    c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
  df_combine <- data.frame(KSD = c(sample1, sample2), group = group)
  # create ECDF of data
  cdf1 <- ecdf(sample1)
  cdf2 <- ecdf(sample2)
  # find min and max statistics to draw line between points of greatest distance
  minMax <-
    seq(min(sample1, sample2),
      max(sample1, sample2),
      length.out = length(sample1)
    )
  x0 <-
    minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))))]

  if (length(x0) > 3) x0 <- median(x0)

  y0 <- cdf1(x0)
  y1 <- cdf2(x0)

  plot_base <-
    ggplot(df_combine, aes(x = KSD, group = group, color = group)) +
    stat_ecdf(size = 1) +
    xlab("Values") +
    ylab("ECDF") +
    scale_color_gb(labels = c(labels[1], labels[2])) +
    theme(legend.title = element_blank())

  layer_line <- pmap(list(x0, y0, y1), ~ {
    geom_segment(
      aes(
        x = ..1,
        xend = ..1,
        y = ..2,
        yend = ..3
      ),
      linetype = "dashed",
      color = link_color
    )
  })

  layer_uppoint <- map2(x0, y0, ~ {
    geom_point(aes(x = .x, y = .y),
      color = point_color,
      size = point_size
    )
  })

  layer_downpoint <- map2(x0, y1, ~ {
    geom_point(aes(x = .x, y = .y),
      color = point_color,
      size = point_size
    )
  })


  plot_ecdf <- plot_base

  for (i in seq(layer_line)) {
    plot_ecdf <- plot_ecdf +
      layer_line[[i]] +
      layer_uppoint[[i]] +
      layer_downpoint[[i]]
  }


  if (ks_test) plot_ecdf <- plot_ecdf + labs(caption = labels[3])

  return(plot_ecdf)
}
