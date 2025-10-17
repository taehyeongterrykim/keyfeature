#' Key Feature (KF) analysis
#'
#' Identify key lexico-grammatical features that distinguish target and reference corpora (Egbert & Biber, 2023). The function reads two files
#' (\code{.csv}, \code{.xlsx}, or \code{.xls}), each containing normalized rates
#' of feature occurrences (rows = texts; columns = features). For each feature, it
#' computes:
#' \itemize{
#'   \item Mean and SD for target and reference corpora
#'   \item Pooled SD = sqrt((sd_t^2 + sd_r^2)/2)
#'   \item Cohen's d = (mean_t - mean_r) / pooled SD
#' }
#'
#' @param target     1) File path to the target corpus or
#'                   2) File name in case located in the working directory (File should be .csv, .xlsx, or .xls).
#' @param reference  1) File path to the reference corpus or
#'                   2) File name in case located in the working directory (File should be .csv, .xlsx, or .xls).
#' @param features Optional. One or more column names to include in the key feature analysis.
#'   If not specified, the function automatically selects all numeric columns that appear in both files.
#' @param plot       If \code{TRUE} (default), draw a lollipop plot of Cohen's d (features on Y, D values on X).
#' @param xlim       Numeric vector giving X-axis limits for the plot
#'   (default \code{c(-2, 2)}).
#' @param xinterval  Numeric step for X-axis intervals
#'   (default \code{0.5}).
#'
#' @return A data frame (one row per feature) with:
#' \describe{
#'   \item{feature}{Feature name}
#'   \item{mean_target, mean_reference}{Means in target and reference corpora}
#'   \item{sd_target, sd_reference}{SDs in target and reference corpora}
#'   \item{sd_pooled}{Pooled SD}
#'   \item{cohens_d}{Cohen's \eqn{d} effect size}
#' }
#'
#' The returned table and plot are sorted in descending order of \code{cohens_d}.
#'
#' @examples
#' \dontrun{
#' # Select files (csv, xlsx, or xls) and features (numeric) to include in the analysis
#' kf <- keyfeature(
#'   target    = "korean_target.csv",
#'   reference = "chinese_reference.csv",
#'   features  = c("emphatic", "nn_all", "negation")
#' )
#'
#' # Select files (csv, xlsx, or xls) and include all features (numeric) that overlap between the two files.
#' kf <- keyfeature("target.xlsx", "reference.xlsx")
#' }
#'
#' @references
#' Egbert, J., & Biber, D. (2023). Key feature analysis: A simple, yet powerful method for comparing text varieties. Corpora, 18(1), 121–133. https://doi.org/10.3366/cor.2023.0275
#'
#' @importFrom graphics plot axis par abline segments points mtext
#' @export
keyfeature <- function(target, reference, features = NULL,
                       plot = TRUE, xlim = c(-2, 2), xinterval = 0.5) {
  # check paths
  is_path  <- function(x) is.character(x) && length(x) == 1 && file.exists(x)
  if (!is_path(target))    stop("'target' must be a valid file path.", call. = FALSE)
  if (!is_path(reference)) stop("'reference' must be a valid file path.", call. = FALSE)

  # read CSV or Excel files
  read_file <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") {
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("To read Excel files, please install the 'readxl' package.", call. = FALSE)
      }
      readxl::read_excel(path)
    } else {
      stop("Unsupported file type: ", ext, ". Use .csv, .xlsx, or .xls.", call. = FALSE)
    }
  }
  numeric_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

  df_t <- read_file(target)
  df_r <- read_file(reference)

  # choose features
  if (is.null(features)) {
    cand_t <- numeric_cols(df_t)
    cand_r <- numeric_cols(df_r)
    features <- intersect(cand_t, cand_r)
    if (!length(features)) {
      stop("No overlapping numeric columns were found between the two files.", call. = FALSE)
    }
  } else {
    miss_t <- setdiff(features, names(df_t))
    miss_r <- setdiff(features, names(df_r))
    if (length(miss_t)) stop("Missing in target: ", paste(miss_t, collapse = ", "), call. = FALSE)
    if (length(miss_r)) stop("Missing in reference: ", paste(miss_r, collapse = ", "), call. = FALSE)
  }

  # ensure numeric
  nonnum_t <- features[!vapply(df_t[features], is.numeric, logical(1))]
  nonnum_r <- features[!vapply(df_r[features], is.numeric, logical(1))]
  if (length(nonnum_t)) stop("Non-numeric feature(s) in target: ", paste(nonnum_t, collapse = ", "), call. = FALSE)
  if (length(nonnum_r)) stop("Non-numeric feature(s) in reference: ", paste(nonnum_r, collapse = ", "), call. = FALSE)

  # compute descriptive stats (always na.rm = TRUE)
  mt  <- sapply(df_t[features], function(x) mean(x, na.rm = TRUE))
  mr  <- sapply(df_r[features], function(x) mean(x, na.rm = TRUE))
  sdt <- sapply(df_t[features], function(x) sd(x, na.rm = TRUE))
  sdr <- sapply(df_r[features], function(x) sd(x, na.rm = TRUE))

  sd_pooled <- sqrt((sdt^2 + sdr^2) / 2)
  d <- (mt - mr) / sd_pooled
  d[!is.finite(d)] <- NA_real_

  # combine results
  result <- data.frame(
    feature        = names(mt),
    mean_target    = mt,
    mean_reference = mr,
    sd_target      = sdt,
    sd_reference   = sdr,
    sd_pooled      = sd_pooled,
    cohens_d       = d,
    check.names    = FALSE,
    row.names      = NULL
  )
  # sort so the returned table is ordered high to low D
  result <- result[order(-result$cohens_d, result$feature), ]

  # lollipop plot
  if (isTRUE(plot)) {
    # drop non-finite d and sort again
    plot_data <- result[is.finite(result$cohens_d), , drop = FALSE]
    ord <- order(-plot_data$cohens_d, plot_data$feature)
    plot_data <- plot_data[ord, , drop = FALSE]

    if (nrow(plot_data) == 0L) {
      warning("All Cohen's d values are NA/Inf; skipping plot.")
    } else {
      n <- nrow(plot_data)
      y <- seq_len(n)

      par_op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(par_op), add = TRUE)
      graphics::par(mar = c(4, 12, 3, 2))

      graphics::plot(plot_data$cohens_d, y,
                     type = "n",
                     xlim = xlim,
                     ylim = c(n, 1),
                     yaxt = "n",
                     xlab = "D",
                     ylab = "")

      vxs <- seq(from = ceiling(xlim[1] / xinterval) * xinterval,
                 to   = floor(xlim[2]  / xinterval) * xinterval,
                 by   = xinterval)
      graphics::abline(v = vxs, col = "grey85", lty = "solid")
      graphics::abline(v = 0,   col = "grey50", lty = 2)

      graphics::segments(x0 = 0, y0 = y, x1 = plot_data$cohens_d, y1 = y, lwd = 1.5)
      graphics::points(plot_data$cohens_d, y, pch = 16, cex = 1.1)

      graphics::axis(2, at = y, labels = plot_data$feature, las = 2, cex.axis = 0.9)
      graphics::mtext("Feature", side = 2, line = 10, cex = 1)
    }
  }

  return(result)
}
