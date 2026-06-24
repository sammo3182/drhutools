
#' Plot Results of fsQCA Permutation Test
#'
#' Plots distributions of consistencies and counterexamples from permutation tests 
#' of fsQCA data, including confidence intervals adjusted to account for multiple 
#' inference. Also prints observed consistency values and number of counterexamples 
#' as black dots along the x-axis, for comparison.
#' @param x Object returned by [fsQCApermTest()].
#' @param y A vector of configurations to examine. Default behavior is
#' to examine all configurations.
#' @param statistic The statistic to examine (`consistency`,
#' `counterexamples`, or `both`).
#' @param ... Additional parameters to pass on.
#' @return Plots of distributions of consistencies, counterexamples, or both.
#' @note Adopted from the archived CRAN package \pkg{QCAfalsePositive} by Bear
#'   Braumoeller. The original package has been removed from CRAN and is no
#'   longer maintained. It is included in \pkg{drhutools} for continued
#'   accessibility.
#' @concept fsQCA permutation test distribution
#' @method plot fsQCApt
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
#' plot(test)
#' plot(test, "bi", statistic = "consistency")
#' plot(test, c("BI", "Bi"), statistic = "both")
#' plot(test, statistic = "consistency")
#' plot(test, "BI")
#' }





plot.fsQCApt <- function(x, y=x$config.names, statistic="both", ...){
	if(!all(y %in% x$config.names)){
		stop("Unknown configuration of variables.")
		}
	if(!(statistic %in% c("consistency", "counterexamples", "both"))){
		stop("statistic must equal 'consistency', 'counterexamples', or 'both'.")
		}
	if(dev.cur()==1){
		dev.new(width=(5+(2*(statistic=="both"))), height=min(7, length(y)*3), units="in", res=360)
		}
	par(mar = c(4,2,2,1))
	config.num <- match(y, x$config.names)
	config.num <- config.num[!is.na(config.num)]
	par(mfrow = c(length(y), if (statistic == "both") 2 else 1))

	# Loop-invariant axis limits, computed once.
	con_min <- min(vapply(x$permutations.con, min, numeric(1)))
	cex_max <- max(vapply(x$permutations.cex, max, numeric(1)))

	for(m in config.num){
	if(m==max(config.num)){
		conlab <- "Consistency"
		cexlab <- "Counterexamples"
		} else{
		conlab <- "   "
		cexlab <- "   "
		}
	if((statistic=="consistency") | (statistic=="both")){
		con <- x$permutations.con[[m]]
		dens <- density(con, cut=0)
		pctile <- sort(con)[length(con)*(1-x$result.con[m,6])]
		upper <- dens$x > pctile
		plot(dens, axes=FALSE, col="white", xlab=conlab, ylab=" ", xlim=c(con_min, 1), main=x$config.names[m])
		axis(1)
		polygon(c(dens$x, rev(dens$x)), c(dens$y, rep(0, length(dens$y))), col="#6baed6", border="white", lwd=0.1)
		polygon(c(dens$x[upper], rev(dens$x[upper])), c(dens$y[upper], rep(0, sum(upper))), col="#08519c", border="white", lwd=0.1)
		points(x$result.con[m,1], par("usr")[3], pch=19, xpd=TRUE)
	}
	if((statistic=="counterexamples") | (statistic=="both")){
		cex <- x$permutations.cex[[m]]
		histpct <- sort(cex)[length(cex)*(x$result.cex[m,6])]
		hist(cex, breaks=seq(0,cex_max+1,1)-0.5, axes=FALSE, col=c(rep("#08519c", histpct), rep("#6baed6", max(cex)-histpct+1)), border="white", xlab=cexlab, ylab=" ", xlim=c(-1, cex_max+1), main=x$config.names[m])
		axis(1)
		points(x$result.cex[m,1], par("usr")[3], pch=19, xpd=TRUE)

	}
	}
}
