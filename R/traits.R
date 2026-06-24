#' @name traits
#' @title Calculate psychological traits based on
#'
#' @param survey Psychological survey data as a `data.frame`.
#'
#' @details
#' The current version can calculate the TOSCA-3SC scores and Grit-O score.
#'
#' @return A data frame with trait scores for each observations
#'
#' @references
#' Duckworth, Angela L., Christopher Peterson, Michael D. Matthews, and Dennis R. Kelly. 2007.
#' "Grit: Perseverance and Passion for Long-Term Goals." Journal of Personality and Social Psychology
#' 92(6): 1087-1101. doi:10.1037/0022-3514.92.6.1087.
#'
#' Tangney, June P. 1990. "Assessing Individual Differences in Proneness to Shame and Guilt: Development
#' of the Self-Conscious Affect and Attribution Inventory." Journal of Personality and Social Psychology
#' 59(1): 102-11. doi:10.1037/0022-3514.59.1.102.
#'
#' @examples
#' column_names <- c("Q3|R3", "Q3|R4", "Q4|R3", "Q4|R4", "Q5|R5", "Q5|R6", "Q6|R3", "Q6|R4", "Q7|R3", 
#' "Q7|R4", "Q8|R5", "Q8|R6", "Q9|R5", "Q9|R6", "Q10|R5", "Q10|R6", "Q11|R5", "Q11|R6", "Q12|R3", 
#' "Q12|R4", "Q13|R3", "Q13|R4", "Q14|1", "Q15|1", "Q16|1", "Q17|1", "Q18|1", "Q19|1", "Q20|1", 
#' "Q21|1", "Q22|1", "Q23|1", "Q24|1", "Q25|1")
#'
#' toy_data <- data.frame(matrix(sample(1:5, 10 * length(column_names), replace = TRUE), 
#' ncol = length(column_names)))
#'
#' names(toy_data) <- column_names
#'
#' traits(toy_data)
#'
#' @export

traits <- function(survey) {
  # Magic input: return the reference ranges instead of scoring a survey.
  if (is.character(survey) && length(survey) == 1L && survey == "average") {
    return("Reference: Average shame score is [30.8, 34.7], guilt score is [45.2, 46.8] for Japanese college students; Grit-S is [3.5, 3.7] for pharmacy students in the U.S.")
  }

  # Helper: sum/average the survey columns whose names match `pattern`.
  # drop = FALSE keeps a single matched column a data frame so the row reducer works.
  score <- function(pattern, reducer) {
    reducer(survey[, grep(pattern, names(survey)), drop = FALSE], na.rm = TRUE)
  }

  data.frame(
    score_shame = score(".*R(3|5)", rowSums),                      # TOSCA-3SC shame
    score_guilt = score(".*R(4|6)", rowSums),                      # TOSCA-3SC guilt
    score_gritO = score(".*1$", rowMeans),                         # Grit-O (12 items)
    score_gritS = score("^Q(15|17|18|19|20|21|22|25)", rowMeans)   # Grit-S (8 items)
  )
}
