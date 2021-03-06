% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ksplot.R
\name{plotKS}
\alias{plotKS}
\title{Packed \code{ggplot2} function to present the empirical cumulative distribution functions}
\usage{
plotKS(comparison, ls_title)
}
\arguments{
\item{comparison}{A list of numeric vectors for distribution comparison, e.g., the records of the subjects' responses in a control-treatment experiment.}

\item{ls_title}{A list of three names, the first two control and treatment groups, and the third is a caption line of the Kolmogorov-Smirnov test result.}
}
\value{
A ggplot object.
}
\description{
Packed \code{ggplot2} function to present the empirical cumulative distribution functions
}
\examples{
# Not Run
# ls_ks <- vector(mode = "list")

# ls_ks$hukou <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 1], 
# y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 0])

# ls_ks$benefit <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 2], 
# y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 0])

# ls_ks$compare <- ks.test(x = df_survey$response_migrateHK[df_survey$treat_migrateHK == 2], 
# y = df_survey$response_migrateHK[df_survey$treat_migrateHK == 1])

# df_ks <- map_df(ls_ks, function(aResult){
#   tibble(D = aResult$statistic,
#            p.value = aResult$p.value)
#            })

# df_ks$compared <- c("Control", "Control", "Hukou")

# df_ks$comparing <- c("Hukou", "Access", "Access")

# df_ks$result <- caption_plot <- paste0("Kolmogorov-Smirnov Test:", 
# format(round(df_ks$D, digits = 2), nsmall = 2), 
# " (p: ", 
# format(round(df_ks$p.value, digits = 2), nsmall = 2), ")")

# df_ks <- select(df_ks, -D, -p.value) \%>\% 
#   t \%>\% 
#   as.tibble

}
