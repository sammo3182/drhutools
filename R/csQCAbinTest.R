

#' A Simple Binomial Test for Type I Error in csQCA
#'
#' A Binomial test for crisp-set qualitative comparative analysis (csQCA),
#' designed to calculate the probability of a false positive given the number of
#' hypotheses implicitly tested and the number of confirming cases.
#' @param freq.y The frequency with which the dependent variable occurs in the
#' sample (the number of 1s divided by the total number of cases).
#' @param configs A list of configurations and the number of cases in which each
#' configuration occurs.
#' @param total.configs The total number of configurations used in the original
#' csQCA analysis. This will generally equal the number of lines in the
#' truth table used for Boolean minimization.
#' @param adj.method The method used to calculate adjusted p-values (see
#' [stats::p.adjust()] for details).
#' @return An object containing the results of the Binomial test.
#' @note Adopted from the archived CRAN package \pkg{QCAfalsePositive} by Bear
#'   Braumoeller. The original package has been removed from CRAN and is no
#'   longer maintained. It is included in \pkg{drhutools} for continued
#'   accessibility.
#' @concept Binomial csQCA multiple inference p-value adjust
#' @export
#' @examples
#' test <- csQCAbinTest(freq.y=0.7, configs=list(aB=5, bCD=3, Ce=2),
#'    total.configs=20)
#' summary(test)


csQCAbinTest <- function(freq.y, configs, total.configs, adj.method="holm"){
	.qca_binom_test(freq.y, configs, total.configs, adj.method, match.call(), "csQCAbt")
}
