% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traits.R
\name{traits}
\alias{traits}
\title{Calculate psychological traits based on}
\usage{
traits(survey)
}
\arguments{
\item{survey}{Psychological survey data as a \code{data.frame}.}
}
\value{
A data frame with trait scores for each observations
}
\description{
Calculate psychological traits based on
}
\details{
The current version can calculate the TOSCA-3SC scores and Grit-O score.
}
\examples{
column_names <- c("Q3|R3", "Q3|R4", "Q4|R3", "Q4|R4", "Q5|R5", "Q5|R6", "Q6|R3", "Q6|R4", "Q7|R3", 
"Q7|R4", "Q8|R5", "Q8|R6", "Q9|R5", "Q9|R6", "Q10|R5", "Q10|R6", "Q11|R5", "Q11|R6", "Q12|R3", 
"Q12|R4", "Q13|R3", "Q13|R4", "Q14|1", "Q15|1", "Q16|1", "Q17|1", "Q18|1", "Q19|1", "Q20|1", 
"Q21|1", "Q22|1", "Q23|1", "Q24|1", "Q25|1")

toy_data <- data.frame(matrix(sample(1:5, 10 * length(column_names), replace = TRUE), 
ncol = length(column_names)))

names(toy_data) <- column_names

traits(toy_data)

}
\references{
Duckworth, Angela L., Christopher Peterson, Michael D. Matthews, and Dennis R. Kelly. 2007.
“Grit: Perseverance and Passion for Long-Term Goals.” Journal of Personality and Social Psychology
92(6): 1087–1101. doi:10.1037/0022-3514.92.6.1087.

Tangney, June P. 1990. “Assessing Individual Differences in Proneness to Shame and Guilt: Development
of the Self-Conscious Affect and Attribution Inventory.” Journal of Personality and Social Psychology
59(1): 102–11. doi:10.1037/0022-3514.59.1.102.
}
