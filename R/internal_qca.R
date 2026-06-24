# Shared internals for the QCA false-positive tests adopted from Bear
# Braumoeller's archived `QCAfalsePositive` package. `csQCAbinTest()` and
# `mvQCAbinTest()` differ only in the class they tag onto the result, and their
# `summary()` methods are identical; both live here to avoid duplication.
#' @noRd

.qca_binom_test <- function(freq.y, configs, total.configs, adj.method, cl, result_class) {
  if (typeof(configs) != "list" || is.null(names(configs))) {
    stop("configs must be a list of the form list(name1=number.of.cases1, name2=number.of.cases2, ...)")
  }
  if (total.configs < length(configs)) {
    stop("Value of total.configs cannot be less than the length of configs.")
  }
  if (length(freq.y) > 1 | freq.y < 0 | freq.y > 1) {
    stop("y must be a single value between 0 and 1.")
  }
  if (sum(configs > (total.configs - (total.configs * freq.y))) > 0) {
    warning("Number of confirming cases not consistent with total number of configurations and frequency of outcome.", call. = FALSE)
  }

  cases <- as.vector(unlist(configs))
  p_raw <- (1 - freq.y)^cases
  p_adj <- p.adjust(
    c(p_raw, rep(1, total.configs - length(configs))),
    method = adj.method
  )[seq_along(configs)]

  result <- cbind("Number of cases" = cases, "p-raw" = p_raw, "p-adj" = p_adj)
  rownames(result) <- names(configs)

  structure(
    list(
      call = cl,
      total.configurations = total.configs,
      config.names = names(configs),
      p.adj.method = adj.method,
      result = result
    ),
    class = result_class
  )
}

.summary_qca_bt <- function(object) {
  cat("Call:\n")
  print(object$call)
  cat("\n")
  cat("Counterexamples\n")
  printCoefmat(object$result[, 1:3], P.values = TRUE, has.Pvalue = TRUE, tst.ind = 1, signif.legend = FALSE)
  cat(paste("Total number of configurations:", object$total.configurations), "\n")
  cat(paste("p-value adjustment method:", object$p.adj.method))
  cat("\n\n")
}
