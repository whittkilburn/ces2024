#' One-way weighted tabulation for CES 2024
#'
#' Produce a one-way tabulation (weighted percentages) for a CES variable.
#' The function filters out records with missing weights, constructs a
#' one-stage survey design with \code{commonpostweight}, and returns a
#' formatted \pkg{gt} table. No codebook lookups are performed.
#'
#' @param var A bare variable name from \code{ces24} (e.g., \code{educ}).
#' @param decimals Number of decimal places to display for percentages.
#'   Default is \code{0} (whole numbers).
#' @param drop_na Logical. Drop \code{NA} category from the tabulation?
#'   Default \code{TRUE}.
#'
#' @return A \pkg{gt} table with weighted counts and percentages.
#' @examples
#' \dontrun{
#' tab(educ)                 # whole-number percentages
#' tab(educ, decimals = 2)   # two-decimal percentages
#' }
#' @export
tab <- function(var, decimals = 0, drop_na = TRUE) {
  # dependencies check
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package 'survey' is required. Install it with install.packages('survey').")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it with install.packages('dplyr').")
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required. Install it with install.packages('gt').")
  }

  # capture variable name
  varname <- rlang::ensym(var) |> as.character()

  # --- subset to non-missing weights ---
  ces24_nomiss <- dplyr::filter(ces24, !is.na(commonpostweight))

  # --- survey design (one-stage, weights only) ---
  ces_design <- survey::svydesign(
    ids     = ~1,
    data    = ces24_nomiss,
    weights = ~commonpostweight
  )

  # --- weighted one-way table ---
  # use formula ~ varname
  fml <- stats::as.formula(paste0("~", varname))
  tab_w <- survey::svytable(fml, ces_design)

  # to data frame
  df <- as.data.frame(tab_w)
  names(df) <- c("category", "count")  # consistently name columns

  # optionally drop NA category
  if (drop_na) {
    df <- df[!is.na(df$category), , drop = FALSE]
  }

  # percentages
  total <- sum(df$count)
  df$percent <- (df$count / total) * 100

  # --- format with gt ---
  gt::gt(df) |>
    gt::fmt_number(columns = "percent", decimals = decimals) |>
    gt::cols_label(
      category = "Response",
      count    = "Weighted Count",
      percent  = "Percent"
    ) |>
    gt::tab_header(
      title = gt::md(paste0("**", varname, "**"))
    )
}
