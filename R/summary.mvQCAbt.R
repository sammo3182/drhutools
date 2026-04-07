
#' Summarize Binomial Tests for mvQCA Data
#'
#' Displays number of confirming cases and raw and adjusted p-scores 
#' following Binomial test of mvQCA data.
#' @param object Object returned by [mvQCAbinTest()].
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for counterexamples and consistency.
#' @note Adopted from the archived CRAN package \pkg{QCAfalsePositive} by Bear
#'   Braumoeller. The original package has been removed from CRAN and is no
#'   longer maintained. It is included in \pkg{drhutools} for continued
#'   accessibility.
#' @concept binomial test mvQCA
#' @method summary mvQCAbt
#' @export
#' @examples
#' test <- mvQCAbinTest(freq.y=0.7, configs=list(aB=5, bCD=3, Ce=2),
#'    total.configs=20)
#' summary(test)


summary.mvQCAbt <- function(object, ...){
	cat("Call:\n")
    print(object$call)
    cat("\n")
    cat("Counterexamples\n")
	printCoefmat(object$result[,1:3], P.values=TRUE, has.Pvalue=TRUE, tst.ind=1, signif.legend=FALSE)
    cat(paste("Total number of configurations:", object$total.configurations), "\n")
    cat(paste("p-value adjustment method:", object$p.adj.method))
    cat("\n")
    cat("\n")
	}
