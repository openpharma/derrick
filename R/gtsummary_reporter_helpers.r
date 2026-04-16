# =============================================================================
# gtsummary_reporter_helpers.r
#
# Internal helper functions for gtsummary_to_reporter_output().
# None of these are exported; they are documented with @noRd to suppress
# man-page generation.
# =============================================================================


# ---------------------------------------------------------------------------
# Text processing
# ---------------------------------------------------------------------------

#' Strip markdown bold markers from a character vector
#' @noRd
strip_md_bold <- function(x) {
  if (is.null(x)) return(x)
  x_chr <- as.character(x)
  x_chr <- gsub("\\*\\*(.*?)\\*\\*", "\\1", x_chr, perl = TRUE)
  x_chr <- gsub("__([^_]+)__",       "\\1", x_chr, perl = TRUE)
  x_chr
}

#' Format a POSIXct timestamp as ISO-8601 UTC string
#' @noRd
format_iso8601_utc <- function(x) {
  paste0(format(x, tz = "UTC", "%Y-%m-%dT%H:%M:%S"), " (UTC+0)")
}


# ---------------------------------------------------------------------------
# Environment helpers
# ---------------------------------------------------------------------------

#' Collect sequentially-numbered environment variables into a character vector
#'
#' Looks for variables named `<prefix>1`, `<prefix>2`, … `<prefix><max_n>` in
#' `env` (and its parents) and returns their non-missing, non-empty values.
#'
#' @param prefix Variable name prefix (e.g. `"title"`, `"footnote"`).
#' @param max_n  Maximum index to check (default `9L`).
#' @param env    Environment to start searching in (default `parent.frame()`).
#' @noRd
collect_env_lines <- function(prefix, max_n = 9L, env = parent.frame()) {
  vals <- character(0)
  for (i in seq_len(max_n)) {
    nm <- paste0(prefix, i)
    if (exists(nm, envir = env, inherits = TRUE)) {
      val <- get(nm, envir = env, inherits = TRUE)
      if (!is.null(val) && length(val) > 0) {
        vals <- c(vals, as.character(val))
      }
    }
  }
  vals[!is.na(vals) & nzchar(vals)]
}


# ---------------------------------------------------------------------------
# Row resolution helpers
# ---------------------------------------------------------------------------

#' Resolve a row specification to integer row indices
#'
#' Accepts NULL, logical vectors, numeric vectors, rlang quosures, or lists
#' thereof, and returns an integer vector of matching row positions within
#' `data`.
#'
#' @param row_spec A row specification (see above).
#' @param data     The data frame used for tidy-evaluation.
#' @noRd
resolve_rows <- function(row_spec, data) {
  if (is.null(row_spec)) return(integer(0))

  if (is.list(row_spec) && !rlang::is_quosure(row_spec)) {
    return(unique(unlist(lapply(row_spec, resolve_rows, data = data))))
  }

  if (is.logical(row_spec) && length(row_spec) == nrow(data)) {
    return(which(row_spec))
  }

  if (is.numeric(row_spec)) return(row_spec)

  if (rlang::is_quosure(row_spec) || rlang::is_expression(row_spec) ||
      rlang::is_call(row_spec)    || rlang::is_symbol(row_spec)) {
    res <- rlang::eval_tidy(row_spec, data = data)
    if (is.logical(res)) return(which(res))
    if (is.numeric(res)) return(res)
  }

  integer(0)
}

#' Convert a row specification to a human-readable string (debug helper)
#' @noRd
rows_to_text <- function(x) {
  if (is.list(x) && !rlang::is_quosure(x))  return("list")
  if (rlang::is_quosure(x))                 return(rlang::quo_text(x))
  if (rlang::is_expression(x) || rlang::is_call(x) ||
      rlang::is_symbol(x)     || is.language(x)) {
    return(rlang::expr_text(x))
  }
  if (is.logical(x))  return("logical")
  if (is.numeric(x))  return("numeric")
  class(x)[1]
}


# ---------------------------------------------------------------------------
# Column helpers
# ---------------------------------------------------------------------------

#' Extract the label attribute from a column, falling back to a default
#' @noRd
get_col_label <- function(x, default) {
  lbl <- attr(x, "label", exact = TRUE)
  if (is.null(lbl) || !nzchar(as.character(lbl))) return(default)
  as.character(lbl)
}

#' Estimate a column display width (in inches) from its content
#'
#' @param x           Column values (coerced to character for measurement).
#' @param min_width   Minimum returned width.
#' @param max_width   Maximum returned width.
#' @param chars_per_in Characters per inch assumption.
#' @noRd
calc_col_width <- function(x, min_width = 0.8, max_width = 3.5, chars_per_in = 8) {
  chars <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
  if (!is.finite(chars)) chars <- min_width * chars_per_in
  width <- chars / chars_per_in
  pmin(max_width, pmax(min_width, width))
}

#' Parse a column-widths specification into a numeric vector
#'
#' Accepts a `"|"`-delimited string (e.g. `"10|5|5"`) or a numeric vector.
#' Recycles or truncates to exactly `n_cols` elements.
#'
#' @param widths  Width specification or `NULL`.
#' @param n_cols  Target number of columns.
#' @noRd
parse_column_widths <- function(widths, n_cols) {
  if (is.null(widths)) return(NULL)

  if (is.character(widths) && length(widths) == 1L) {
    parts  <- unlist(strsplit(widths, "\\|", fixed = FALSE))
    widths <- suppressWarnings(as.numeric(trimws(parts)))
  }

  if (!is.numeric(widths)) return(NULL)
  widths <- widths[is.finite(widths)]
  if (length(widths) == 0) return(NULL)

  if (length(widths) < n_cols) {
    widths <- c(widths, rep(tail(widths, 1), n_cols - length(widths)))
  } else if (length(widths) > n_cols) {
    widths <- widths[seq_len(n_cols)]
  }

  widths
}

#' Scale column widths down so they fit within a maximum total width
#'
#' Enforces `min_width` on every column, then proportionally reduces widths
#' that exceed the budget.
#'
#' @param widths     Numeric vector of proposed widths.
#' @param max_total  Maximum allowed total width.
#' @param min_width  Per-column minimum.
#' @noRd
adjust_col_widths <- function(widths, max_total, min_width) {
  widths[!is.finite(widths)] <- min_width
  if (length(widths) == 0) return(widths)

  if (length(widths) * min_width > max_total) {
    min_width <- max_total / length(widths)
  }

  widths <- pmax(widths, min_width)
  total  <- sum(widths, na.rm = TRUE)
  if (total <= max_total) return(widths)

  extra       <- pmax(widths - min_width, 0)
  extra_total <- sum(extra, na.rm = TRUE)
  if (extra_total <= 0) {
    return(widths * (max_total / total))
  }

  scale  <- (max_total - length(widths) * min_width) / extra_total
  scale  <- max(0, min(1, scale))
  widths <- min_width + extra * scale

  if (sum(widths, na.rm = TRUE) > max_total) {
    widths <- widths * (max_total / sum(widths, na.rm = TRUE))
  }

  widths
}

#' Normalise a column-label override specification to a named list
#'
#' Accepts a named character/list vector or a data frame with `column` and
#' `label` columns. Returns `NULL` for unrecognised input.
#'
#' @noRd
normalize_column_labels <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) || is.list(x)) {
    if (is.null(names(x))) return(NULL)
    return(as.list(x))
  }
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    if (all(c("column", "label") %in% names(x))) {
      out        <- as.list(x$label)
      names(out) <- x$column
      return(out)
    }
  }
  NULL
}


# ---------------------------------------------------------------------------
# Page geometry helpers
# ---------------------------------------------------------------------------

#' Return the physical dimensions (width, height) of a named paper size
#'
#' @param paper_size A string (`"letter"`, `"legal"`, `"a4"`, `"rd4"`,
#'   `"none"`) or a numeric length-2 vector `c(width, height)`.
#' @param units      `"inches"` or `"cm"`.
#' @noRd
get_paper_dims <- function(paper_size, units) {
  if (is.numeric(paper_size) && length(paper_size) == 2) return(as.numeric(paper_size))
  size_key <- tolower(as.character(paper_size))
  dims <- switch(
    size_key,
    "letter" = c(8.5,  11),
    "legal"  = c(8.5,  14),
    "a4"     = c(8.27, 11.69),
    "rd4"    = c(8.27, 11.69),
    "none"   = c(Inf,  Inf),
    c(8.5, 11)
  )
  if (tolower(units) == "cm") dims <- dims * 2.54
  dims
}

#' Default page margins for a given unit system
#' @noRd
get_default_margins <- function(units) {
  if (tolower(units) == "cm") {
    return(c(top = 1.27, right = 2.54, bottom = 1.27, left = 2.54))
  }
  c(top = 0.5, right = 1, bottom = 0.5, left = 1)
}

#' Normalise a margin specification to a named numeric vector
#'
#' Accepts `NULL` (→ defaults), a named/unnamed length-4 numeric vector, or a
#' named list. Partial lists are merged with defaults.
#'
#' @noRd
normalize_margins <- function(margins, units) {
  if (is.null(margins)) return(get_default_margins(units))
  if (is.numeric(margins) && length(margins) == 4) {
    if (is.null(names(margins))) names(margins) <- c("top", "right", "bottom", "left")
    return(margins)
  }
  if (is.list(margins)) {
    out <- get_default_margins(units)
    nm  <- intersect(names(margins), names(out))
    out[nm] <- unlist(margins[nm])
    return(out)
  }
  get_default_margins(units)
}

#' Compute the maximum available table width after subtracting margins
#' @noRd
compute_max_table_width <- function(paper_size, orientation, units, margins) {
  dims    <- get_paper_dims(paper_size, units)
  if (tolower(orientation) == "landscape") dims <- rev(dims)
  margins <- normalize_margins(margins, units)
  max(0, dims[1] - margins["left"] - margins["right"])
}

#' Compute the maximum available table height after subtracting margins
#' @noRd
compute_max_table_height <- function(paper_size, orientation, units, margins) {
  dims    <- get_paper_dims(paper_size, units)
  if (tolower(orientation) == "landscape") dims <- rev(dims)
  margins <- normalize_margins(margins, units)
  max(0, dims[2] - margins["top"] - margins["bottom"])
}

#' Estimate the maximum number of data rows that fit on one page
#'
#' Returns `NULL` when the page is infinite or has no rows.
#'
#' @noRd
estimate_rows_per_page <- function(row_count, font_size, paper_size, orientation, units, margins) {
  height <- compute_max_table_height(paper_size, orientation, units, margins)
  if (!is.finite(height) || height <= 0 || row_count <= 0) return(NULL)

  line_height <- (as.numeric(font_size) * 1.2) / 72
  if (tolower(units) == "cm") line_height <- line_height * 2.54

  reserve_lines   <- 4
  header_lines    <- 2
  available_lines <- floor((height / line_height) - reserve_lines - header_lines)
  if (!is.finite(available_lines) || available_lines <= 0) return(NULL)
  min(row_count, available_lines)
}


# ---------------------------------------------------------------------------
# Text wrapping
# ---------------------------------------------------------------------------

#' Wrap a single label string to at most `max_chars` characters per line,
#' preserving any leading-space indentation on every continuation line
#'
#' @param txt       A single character value (may be `NA`).
#' @param max_chars Maximum characters per line (must be >= 1).
#' @noRd
wrap_with_indent <- function(txt, max_chars) {
  if (is.na(txt) || !nzchar(trimws(txt))) return(txt)
  if (nchar(txt) <= max_chars)            return(txt)

  indent_prefix <- regmatches(txt, regexpr("^\\s*", txt))
  body_text     <- substring(txt, nchar(indent_prefix) + 1L)
  words         <- strsplit(body_text, "\\s+")[[1L]]
  words         <- words[nzchar(words)]
  if (length(words) == 0L) return(txt)

  current   <- paste0(indent_prefix, words[1L])
  out_lines <- character(0L)
  for (w in words[-1L]) {
    candidate <- paste0(current, " ", w)
    if (nchar(candidate) <= max_chars) {
      current <- candidate
    } else {
      out_lines <- c(out_lines, current)
      current   <- paste0(indent_prefix, w)
    }
  }
  paste(c(out_lines, current), collapse = "\n")
}


# ---------------------------------------------------------------------------
# Spanning header helpers
# ---------------------------------------------------------------------------

#' Normalise a spanning-header specification to a data frame
#'
#' Accepts a data frame with `from`, `to`, `label` columns or an equivalent
#' named list / list-of-lists. Returns `NULL` for unrecognised input.
#'
#' @noRd
normalize_spanning_headers <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.data.frame(x)) {
    df        <- x
    names(df) <- tolower(names(df))
    if (all(c("from", "to", "label") %in% names(df))) {
      return(df[, c("from", "to", "label"), drop = FALSE])
    }
  }

  if (is.list(x)) {
    if (!is.null(names(x)) && all(c("from", "to", "label") %in% names(x))) {
      return(data.frame(from = x$from, to = x$to, label = x$label,
                        stringsAsFactors = FALSE))
    }

    if (length(x) > 0 && all(vapply(x, is.list, logical(1)))) {
      rows <- lapply(x, function(item) {
        if (all(c("from", "to", "label") %in% names(item))) {
          data.frame(from = item$from, to = item$to, label = item$label,
                     stringsAsFactors = FALSE)
        } else {
          NULL
        }
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) return(do.call(rbind, rows))
    }
  }

  NULL
}

#' Resolve a `from`/`to` spanning-header endpoint to a column name
#'
#' @param x    Numeric index or column-name string.
#' @param cols Character vector of ordered column names.
#' @noRd
resolve_span_col <- function(x, cols) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1) {
    idx <- as.integer(x)
    if (idx >= 1 && idx <= length(cols)) return(cols[idx])
    return(NULL)
  }
  if (is.character(x) && length(x) == 1 && x %in% cols) return(x)
  NULL
}

#' Resolve a `from`/`to` spanning-header endpoint to a column index
#'
#' @param x    Numeric index or column-name string.
#' @param cols Character vector of ordered column names.
#' @noRd
resolve_span_index <- function(x, cols) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1) {
    idx <- as.integer(x)
    if (idx >= 1 && idx <= length(cols)) return(idx)
    return(NULL)
  }
  if (is.character(x) && length(x) == 1) {
    idx <- match(x, cols)
    if (!is.na(idx)) return(idx)
  }
  NULL
}

#' Prepend fallback spanning-header rows when reporter::spanning_header is absent
#'
#' When the `reporter` package does not expose `spanning_header()`, this
#' function inserts a plain text row at the top of `df` with the label centred
#' between `from` and `to`.
#'
#' @param df               Data frame to prepend rows to.
#' @param span_use         Normalised spanning-header spec (data frame).
#' @param ordered_cols     Character vector of column names in display order.
#' @param spanning_header_fn  The `reporter::spanning_header` function or `NULL`.
#' @noRd
apply_spanning_rows <- function(df, span_use, ordered_cols, spanning_header_fn) {
  if (is.null(span_use) || nrow(span_use) == 0) return(df)
  if (!is.null(spanning_header_fn))              return(df)

  span_rows <- vector("list", nrow(span_use))
  for (i in seq_len(nrow(span_use))) {
    from_idx  <- resolve_span_index(span_use$from[i],  ordered_cols)
    to_idx    <- resolve_span_index(span_use$to[i],    ordered_cols)
    label_val <- strip_md_bold(span_use$label[i])
    if (is.null(from_idx) || is.null(to_idx) || is.null(label_val)) next
    if (from_idx > to_idx) { tmp <- from_idx; from_idx <- to_idx; to_idx <- tmp }
    mid_idx           <- floor((from_idx + to_idx) / 2)
    row_vals          <- rep("", length(ordered_cols))
    row_vals[mid_idx] <- as.character(label_val)
    span_rows[[i]]    <- as.list(row_vals)
  }

  span_rows <- span_rows[!vapply(span_rows, is.null, logical(1))]
  if (length(span_rows) == 0) return(df)
  span_df_rows        <- do.call(rbind.data.frame, c(span_rows, list(stringsAsFactors = FALSE)))
  names(span_df_rows) <- ordered_cols
  rbind(span_df_rows, df)
}

#' Apply spanning headers to a reporter table object via reporter::spanning_header
#'
#' @param tbl_obj          reporter table object.
#' @param span_use         Normalised spanning-header spec (data frame).
#' @param ordered_cols     Character vector of column names in display order.
#' @param spanning_header_fn  The `reporter::spanning_header` function or `NULL`.
#' @param debug_spanning   Logical; print debug messages when `TRUE`.
#' @noRd
apply_spanning_headers <- function(tbl_obj, span_use, ordered_cols,
                                    spanning_header_fn, debug_spanning = FALSE) {
  if (is.null(span_use) || nrow(span_use) == 0) return(tbl_obj)

  if (isTRUE(debug_spanning)) {
    message("SPANNING DEBUG: ordered_cols = ", paste(ordered_cols, collapse = ", "))
    message("SPANNING DEBUG: span_use = ")
    print(span_use)
    if (is.null(spanning_header_fn)) {
      message("SPANNING DEBUG: reporter::spanning_header not found in namespace. Using fallback rows.")
    }
  }

  if (is.null(spanning_header_fn)) return(tbl_obj)

  for (i in seq_len(nrow(span_use))) {
    from_col  <- resolve_span_col(span_use$from[i], ordered_cols)
    to_col    <- resolve_span_col(span_use$to[i],   ordered_cols)
    from_idx  <- resolve_span_index(span_use$from[i], ordered_cols)
    to_idx    <- resolve_span_index(span_use$to[i],   ordered_cols)
    label_val <- strip_md_bold(span_use$label[i])

    if (isTRUE(debug_spanning)) {
      message(sprintf(
        "SPANNING DEBUG: from=%s to=%s label=%s | idx=%s-%s | cols=%s-%s",
        as.character(span_use$from[i]), as.character(span_use$to[i]),
        as.character(label_val),
        ifelse(is.null(from_idx), "NA", from_idx),
        ifelse(is.null(to_idx),   "NA", to_idx),
        ifelse(is.null(from_col), "NA", from_col),
        ifelse(is.null(to_col),   "NA", to_col)
      ))
    }

    if (is.null(label_val))                          next
    if (is.null(from_col) && is.null(from_idx))      next
    if (is.null(to_col)   && is.null(to_idx))        next

    # Prefer index-based spanning (more reliable with reporter)
    if (!is.null(from_idx) && !is.null(to_idx)) {
      tbl_obj <- spanning_header_fn(
        x = tbl_obj, from = from_idx, to = to_idx,
        label = label_val, standard_eval = TRUE
      )
    } else if (!is.null(from_col) && !is.null(to_col)) {
      tbl_obj <- spanning_header_fn(
        x = tbl_obj, from = from_col, to = to_col,
        label = label_val, standard_eval = TRUE
      )
    }
  }

  tbl_obj
}


# ---------------------------------------------------------------------------
# Report building helpers
# ---------------------------------------------------------------------------

#' Build a reporter table specification from a processed data frame
#'
#' @param df               Processed data frame (one page chunk or full table).
#' @param col_widths       Named numeric vector of column widths.
#' @param col_map          Data frame with `column` and `label` (visible cols).
#' @param cols_to_define   Character vector: non-label, non-group columns.
#' @param center_cols      Columns to centre-align (subset of `cols_to_define`).
#' @param label_overrides  Named list of column-label overrides or `NULL`.
#' @param group_cols_to_hide  Columns to hide with `blank_after`.
#' @param span_use         Normalised spanning-header spec or `NULL`.
#' @param ordered_cols     Column names in display order.
#' @param spanning_header_fn  `reporter::spanning_header` or `NULL`.
#' @param debug_spanning   Logical; passed to [apply_spanning_headers()].
#' @noRd
build_table_spec <- function(df, col_widths, col_map, cols_to_define, center_cols,
                              label_overrides, group_cols_to_hide,
                              span_use, ordered_cols, spanning_header_fn,
                              debug_spanning = FALSE) {
  df <- apply_spanning_rows(df, span_use, ordered_cols, spanning_header_fn)

  label_header <- ""
  if (!is.null(label_overrides) && "label" %in% names(label_overrides)) {
    label_header <- label_overrides[["label"]]
  }

  tbl <- reporter::create_table(
    df,
    first_row_blank = TRUE,
    header_bold     = TRUE,
    borders         = c("top", "bottom")
  ) %>%
    reporter::column_defaults(
      width = min(1.2, max(0.8, min(col_widths, na.rm = TRUE))),
      align = "left"
    ) %>%
    reporter::define(label,
                     label = label_header,
                     width = col_widths[["label"]],
                     align = "left")

  if (length(group_cols_to_hide) > 0) {
    for (gcol in group_cols_to_hide) {
      if (!gcol %in% names(df)) next
      tbl <- do.call(
        reporter::define,
        list(x = tbl, var = as.name(gcol), visible = FALSE, blank_after = TRUE)
      )
    }
  }

  for (i in seq_len(nrow(col_map))) {
    this_col <- col_map$column[i]
    this_lab <- col_map$label[i]
    if (!is.null(label_overrides) && this_col %in% names(label_overrides)) {
      this_lab <- label_overrides[[this_col]]
    }
    if (!this_col %in% cols_to_define) next

    this_width <- if (this_col %in% names(col_widths)) col_widths[[this_col]] else NULL
    this_align <- if (this_col %in% center_cols) "center" else "left"

    tbl <- do.call(
      reporter::define,
      list(x   = tbl,
           var   = as.name(this_col),
           label = this_lab,
           width = this_width,
           align = this_align)
    )
  }

  tbl <- apply_spanning_headers(tbl, span_use, ordered_cols,
                                  spanning_header_fn, debug_spanning)
  tbl
}

#' Apply footnotes to a reporter table object
#'
#' @param tbl_obj      A reporter table object.
#' @param footnotes_vec  Character vector of footnote strings.
#'
#' @return A named list with elements:
#'   * `tbl`     – the (possibly modified) reporter table object.
#'   * `applied` – `TRUE` if footnotes were successfully applied.
#' @noRd
apply_tbl_footnotes <- function(tbl_obj, footnotes_vec) {
  if (length(footnotes_vec) == 0) return(list(tbl = tbl_obj, applied = FALSE))
  tryCatch(
    {
      for (note in footnotes_vec) {
        tbl_obj <- reporter::footnotes(tbl_obj, note, blank_row = "none")
      }
      list(tbl = tbl_obj, applied = TRUE)
    },
    error = function(e) {
      list(tbl = tbl_obj, applied = FALSE)
    }
  )
}

#' Safely call reporter::add_content, filtering unsupported arguments
#'
#' @param x      A reporter report object.
#' @param object Content object to add.
#' @param ...    Additional arguments forwarded if accepted by `add_content`.
#' @noRd
add_content_safe <- function(x, object, ...) {
  args <- list(...)
  keep <- intersect(names(args), names(formals(reporter::add_content)))
  args <- args[keep]
  do.call(reporter::add_content, c(list(x = x, object = object), args))
}
