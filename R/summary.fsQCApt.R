
#' Summarize Permutation Tests for fsQCA Data
#'
#' Displays observed values, confidence intervals, and raw and adjusted p-scores for both consistency and counterexamples following permutation test of fsQCA data.
#' @param object Object returned by [fsQCApermTest()].
#' @param ... Additional parameters to pass on.
#' @return Two matrices of values for counterexamples and consistency.
#' @note Adopted from the archived CRAN package \pkg{QCAfalsePositive} by Bear
#'   Braumoeller. The original package has been removed from CRAN and is no
#'   longer maintained. It is included in \pkg{drhutools} for continued
#'   accessibility.
#' @concept permutation test fsQCA
#' @method summary fsQCApt
#' @export
#' @examples
#' \donttest{
#' intersect  <- pmin(social.revolutions$breakdown, social.revolutions$pop.ins)
#' intersect2 <- pmin(social.revolutions$breakdown, (1 - social.revolutions$pop.ins))
#' intersect3 <- pmin((1 - social.revolutions$breakdown), social.revolutions$pop.ins)
#' intersect4 <- pmin((1 - social.revolutions$breakdown), (1 - social.revolutions$pop.ins))
#'
#' test <- fsQCApermTest(y = social.revolutions$soc.rev,
#'   configs = list(BI = intersect, Bi = intersect2,
#'                  bI = intersect3, bi = intersect4),
#'   total.configs = 4)
#' summary(test)
#' }




summary.fsQCApt <- function(object, ...){
	cat("Call:\n")
    print(object$call)
    cat("\n")
    cat("Counterexamples\n")
	printCoefmat(object$result.cex[,c(1,3,2,5,7)])
    cat("\n")
    cat("Consistency\n")
	printCoefmat(object$result.con[,c(1,2,3,5,7)])
	cat("\n")
    cat(paste("Total number of configurations:", object$total.configurations), "\n")
    cat(paste("Number of permutations:", length(object$permutations.cex[[1]])), "\n")
    cat(paste("p-value adjustment method:", object$p.adj.method))
    cat("\n")
    cat("\n")
	}
