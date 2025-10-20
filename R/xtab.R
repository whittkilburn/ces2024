#' Two-way weighted cross-tabulation for CES 2024
#'
#' Produce a weighted cross-tabulation for two CES variables, with frequencies
#' and optional row or column percentages, formatted as a \pkg{gt} table.
#' The function filters out missing weights and missing values on both variables,
#' constructs a one-stage survey design with \code{commonpostweight}, and
#' returns a table with per-column spanners (n and %).
#'
#' @param rowvar A bare variable name for the table rows (e.g., \code{pid7}).
#' @param colvar A bare variable name for the table columns (e.g., \code{educ}).
#' @param percent One of \code{none}, \code{row}, or \code{column}. You may pass
#'   it unquoted as \code{none}, \code{row}, or \code{column} (default \code{none}).
#' @param decimals Integer; number of decimal places to display for percentages
#'   (default \code{0}).
#' @param drop_na Logical; drop NA categories (default \code{TRUE}).
#'
#' @return A \pkg{gt} table with weighted frequencies (rounded up) and, if requested,
#'   row or column percentages.
#'
#' @examples
#' \dontrun{
#' # Frequencies only (default)
#' xtab(pid7, educ)
#'
#' # Column percentages (columns sum to 100)
#' xtab(pid7, educ, column)
#'
#' # Row percentages (rows sum to 100), show 1 decimal
#' xtab(pid7, educ, row, decimals = 1)
#' }
#' @export
xtab <- function(rowvar, colvar, percent = none, decimals = 0, drop_na = TRUE) {
  # deps
  if (!requireNamespace("survey", quietly = TRUE)) stop("Install 'survey'.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install 'dplyr'.")
  if (!requireNamespace("gt", quietly = TRUE)) stop("Install 'gt'.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Install 'rlang'.")

  # capture bare inputs
  row_name <- rlang::ensym(rowvar) |> as.character()
  col_name <- rlang::ensym(colvar) |> as.character()

  # percent can be passed bare (e.g., row) or as string
  pct_arg <- rlang::as_string(rlang::enexpr(percent))
  if (is.na(pct_arg) || pct_arg == "") pct_arg <- "none"
  pct_arg <- match.arg(pct_arg, choices = c("none", "row", "column"))

  # ---- subset to non-missing weights and non-missing vars ----
  df <- ces24
  df <- df[!is.na(df[["commonpostweight"]]), , drop = FALSE]
  # drop cases with NA on either variable (if drop_na = TRUE)
  if (drop_na) {
    df <- df[!is.na(df[[row_name]]) & !is.na(df[[col_name]]), , drop = FALSE]
  }

  # coerce to factor to get tidy labels if needed
  if (!is.factor(df[[row_name]])) df[[row_name]] <- as.factor(df[[row_name]])
  if (!is.factor(df[[col_name]])) df[[col_name]] <- as.factor(df[[col_name]])

  # ---- survey design (one-stage, weights only) ----
  ces_design <- survey::svydesign(
    ids     = ~1,
    data    = df,
    weights = ~commonpostweight
  )

  # ---- weighted two-way table ----
  fml <- stats::as.formula(paste0("~", row_name, " + ", col_name))
  tab_w <- survey::svytable(fml, ces_design)  # table with rows=rowvar, cols=colvar

  # base frequencies matrix
  freq_mat <- as.matrix(tab_w)
  # round UP to integers for display
  freq_mat_disp <- ceiling(freq_mat)

  # percentages (if requested)
  if (pct_arg == "row") {
    pct_mat <- prop.table(tab_w, margin = 1) * 100
  } else if (pct_arg == "column") {
    pct_mat <- prop.table(tab_w, margin = 2) * 100
  } else {
    pct_mat <- NULL
  }

  # ---- reshape to a data.frame with interleaved n and % per column ----
  rn <- rownames(freq_mat_disp)
  cn <- colnames(freq_mat_disp)

  out <- data.frame(Row = rn, stringsAsFactors = FALSE)

  for (j in seq_along(cn)) {
    this_col <- cn[j]
    # add frequency column
    out[[paste0(this_col, "_n")]] <- as.numeric(freq_mat_disp[, j])
    # add percent column (if requested)
    if (!is.null(pct_mat)) {
      out[[paste0(this_col, "_pct")]] <- as.numeric(pct_mat[, j])
    }
  }

  # ---- build gt table with spanners per column ----
  gt_tbl <- gt::gt(out)

  # add spanners so each original column gets "n" and "%"
  for (j in seq_along(cn)) {
    cols <- paste0(cn[j], c("_n", if (!is.null(pct_mat)) "_pct"))
    gt_tbl <- gt::tab_spanner(gt_tbl, label = cn[j], columns = cols)
  }

  # format numbers
  if (!is.null(pct_mat)) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(columns = grep("_pct$", names(out), value = TRUE), decimals = decimals)
  }
  gt_tbl <- gt_tbl |>
    gt::cols_label(Row = row_name) |>
    gt::cols_align(align = "right", columns = gt::everything()) |>
    gt::tab_header(
      title = gt::md(paste0("**", row_name, " × ", col_name, "**")),
      subtitle = switch(
        pct_arg,
        none   = "Weighted frequencies (counts rounded up)",
        row    = "Weighted frequencies and row percentages (rows sum to 100)",
        column = "Weighted frequencies and column percentages (columns sum to 100)"
      )
    )

  # rename subcolumns to n / %
  # (present nicer labels under each spanner)
  n_cols   <- grep("_n$",   names(out), value = TRUE)
  pct_cols <- grep("_pct$", names(out), value = TRUE)

  if (length(n_cols)) {
    names_map_n <- setNames(rep("n", length(n_cols)), n_cols)
    gt_tbl <- gt::cols_label(gt_tbl, .list = as.list(names_map_n))
  }
  if (length(pct_cols)) {
    names_map_p <- setNames(rep("%", length(pct_cols)), pct_cols)
    gt_tbl <- gt::cols_label(gt_tbl, .list = as.list(names_map_p))
  }

  gt_tbl
}
