#' @name plotKS
#' @title Packed \code{ggplot2} function to present the empirical cumulative distribution functions
#' 
#' @param comparison A list of numeric vectors for distribution comparison, e.g., the records of the subjects' responses in a control-treatment experiment.
#' @param ls_title A list of three names, the first two control and treatment groups, and the third is a caption line of the Kolmogorov-Smirnov test result.
#' 
#' @importFrom stats ecdf median
#' @import ggplot2
#' @import dplyr
#' 
#' @return A ggplot object.
#' 
#' @examples 
#' # Not Run
#' # ls_ks <- vector(mode = "list")
#' 
#' # ls_ks$hukou <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 1], 
#' # y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 0])
#' 
#' # ls_ks$benefit <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 2], 
#' # y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 0])
#' 
#' # ls_ks$compare <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 2], 
#' # y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 1])
#' 
#' # df_ks <- map_df(ls_ks, function(aResult){
#' #   tibble(D = aResult$statistic,
#' #            p.value = aResult$p.value)
#' #            })
#'
#' # df_ks$compared <- c("Control", "Control", "Hukou")
#' 
#' # df_ks$comparing <- c("Hukou", "Access", "Access")
#' 
#' # df_ks$result <- caption_plot <- paste0("Kolmogorov-Smirnov Test:", 
#' # format(round(df_ks$D, digits = 2), nsmall = 2), 
#' # " (p: ", 
#' # format(round(df_ks$p.value, digits = 2), nsmall = 2), ")")
#' 
#' # df_ks <- select(df_ks, -D, -p.value) %>% 
#' #   t %>% 
#' #   as.tibble
#' 
#' @export

plotKS <- function(comparison, ls_title) {
  sample1 <- comparison[[1]]
  sample2 <- comparison[[2]]
  group <-
    c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
  dat <- data.frame(KSD = c(sample1, sample2), group = group)
  # create ECDF of data
  cdf1 <- ecdf(sample1)
  cdf2 <- ecdf(sample2)
  # find min and max statistics to draw line between points of greatest distance
  minMax <-
    seq(min(sample1, sample2),
        max(sample1, sample2),
        length.out = length(sample1))
  x0 <-
    minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))))]
  y0 <- cdf1(x0)
  y1 <- cdf2(x0)
  
  ggplot(dat, aes(x = KSD, group = group, color = group)) +
    stat_ecdf(size = 1) +
    theme(legend.position = "top") +
    xlab("Values") +
    ylab("ECDF") +
    geom_segment(
      aes(
        x = median(x0),
        y = y0[1],
        xend = median(x0),
        yend = y1[1]
      ),
      linetype = "dashed",
      color = "red"
    ) +
    geom_point(aes(x = median(x0), y = y0[1]),
               color = gb_cols("red"),
               size = 3) +
    geom_point(aes(x = median(x0), y = y1[1]),
               color = gb_cols("red"),
               size = 3) +
    scale_color_gb(labels = c(ls_title[1], ls_title[2])) +
    labs(caption = ls_title[3]) +
    theme(legend.title = element_blank())
}
