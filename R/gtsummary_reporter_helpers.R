# =============================================================================
# gtsummary_reporter_helpers.r
#
# Internal helper functions for gtsummary_reporter().
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

# ---------------------------------------------------------------------------
# Environment helpers
# ---------------------------------------------------------------------------

#' Collect sequentially-numbered environment variables into a character vector
#'
#' Looks for variables named `<prefix>1`, `<prefix>2`, ..., `<prefix><max_n>` in
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
    widths <- c(widths, rep(utils::tail(widths, 1), n_cols - length(widths)))
  } else if (length(widths) > n_cols) {
    widths <- widths[seq_len(n_cols)]
  }

  widths
}

#' Scale column widths down so they fit within a maximum total width
#'
#' Enforces `min_width` on every column, then proportionally reduces widths
#' that exceed the budget. This is only used for user-supplied manual widths;
#' automatic widths are left to `reporter`.
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
  if (is.data.frame(x)) {
    names(x) <- tolower(names(x))
    if (all(c("column", "label") %in% names(x))) {
      out        <- as.list(x$label)
      names(out) <- x$column
      return(out)
    }
  }
  if (is.character(x) || is.list(x)) {
    if (is.null(names(x))) return(NULL)
    return(as.list(x))
  }
  NULL
}


# ---------------------------------------------------------------------------
# Page geometry helpers
# ---------------------------------------------------------------------------

#' Return output types supported by gtsummary_reporter()
#' @noRd
supported_output_types <- function() {
  c("RTF", "TXT", "DOCX", "PDF", "HTML")
}

#' Normalize and validate requested output types
#' @noRd
normalize_output_types <- function(output_types) {
  if (is.null(output_types) || length(output_types) == 0L) {
    stop("`output_types` must contain at least one supported output type.", call. = FALSE)
  }

  output_types <- trimws(toupper(as.character(output_types)))
  output_types <- output_types[!is.na(output_types) & nzchar(output_types)]
  if (length(output_types) == 0L) {
    stop("`output_types` must contain at least one supported output type.", call. = FALSE)
  }

  supported <- supported_output_types()
  unsupported <- setdiff(output_types, supported)
  if (length(unsupported) > 0L) {
    stop(
      paste0(
        "Unsupported `output_types`: ",
        paste(unsupported, collapse = ", "),
        ". Supported values are: ",
        paste(supported, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  unique(output_types)
}

#' Return the file extension for a supported output type
#' @noRd
output_file_extension <- function(output_type) {
  output_type <- normalize_output_types(output_type)
  if (length(output_type) != 1L) {
    stop("`output_type` must be a single output type.", call. = FALSE)
  }

  switch(
    output_type,
    RTF  = "rtf",
    TXT  = "txt",
    DOCX = "docx",
    PDF  = "pdf",
    HTML = "html"
  )
}

#' Normalize named paper sizes to the values accepted by reporter
#'
#' @param paper_size A string (`"letter"`, `"legal"`, `"A4"`, `"RD4"`,
#'   `"none"`) or a numeric length-2 vector `c(width, height)`.
#' @noRd
normalize_reporter_paper_size <- function(paper_size) {
  if (is.numeric(paper_size) && length(paper_size) == 2) return(as.numeric(paper_size))

  size_key <- tolower(as.character(paper_size))
  switch(
    size_key,
    "letter" = "letter",
    "legal"  = "legal",
    "a4"     = "A4",
    "rd4"    = "RD4",
    "none"   = "none",
    as.character(paper_size)
  )
}

#' Return the physical dimensions (width, height) of a named paper size
#'
#' @param paper_size A string (`"letter"`, `"legal"`, `"A4"`, `"RD4"`,
#'   `"none"`) or a numeric length-2 vector `c(width, height)`.
#' @param units      `"inches"` or `"cm"`.
#' @noRd
get_paper_dims <- function(paper_size, units) {
  if (is.numeric(paper_size) && length(paper_size) == 2) return(as.numeric(paper_size))
  size_key <- tolower(as.character(paper_size))
  if (tolower(units) == "cm") {
    switch(
      size_key,
      "letter" = c(21.59, 27.94),
      "legal"  = c(21.59, 35.56),
      "a4"     = c(21, 29.7),
      "rd4"    = c(19.6, 27.3),
      "none"   = c(Inf, Inf),
      c(21.59, 27.94)
    )
  } else {
    switch(
      size_key,
      "letter" = c(8.5, 11),
      "legal"  = c(8.5, 14),
      "a4"     = c(8.27, 11.69),
      "rd4"    = c(7.7, 10.7),
      "none"   = c(Inf, Inf),
      c(8.5, 11)
    )
  }
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
#' Accepts `NULL` (defaults), a named/unnamed length-4 numeric vector, or a
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

# ---------------------------------------------------------------------------
# Text wrapping
# ---------------------------------------------------------------------------

#' Wrap a single label string to at most `max_chars` characters per line,
#' preserving any leading-space indentation on every continuation line
#'
#' This helper is used only for TXT label wrapping. RTF, DOCX, PDF, and HTML
#' outputs rely on `reporter`'s format-specific physical indentation.
#'
#' @param txt       A single character value (may be `NA`).
#' @param max_chars Maximum characters per line (must be >= 1).
#' @noRd
wrap_with_indent <- function(txt, max_chars) {
  if (is.na(txt) || !nzchar(trimws(txt))) return(txt)
  if (nchar(txt) <= max_chars)            return(txt)

  indent_chars <- nchar(regmatches(txt, regexpr("^\\s*", txt)))
  body_text    <- substring(txt, indent_chars + 1L)
  if (!nzchar(body_text)) return(txt)

  lines <- stringi::stri_wrap(body_text,
                               width           = max_chars,
                               indent          = indent_chars,
                               exdent          = indent_chars,
                               whitespace_only = TRUE,
                               simplify        = TRUE)
  paste(lines, collapse = "\n")
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

#' Convert a gtsummary `table_styling$spanning_header` tibble to the
#' `from` / `to` / `label` data frame expected by `apply_spanning_headers()`
#'
#' gtsummary stores one row per column (fields: `column`, `spanning_header`,
#' `level`, `remove`).  This function collapses consecutive columns that share
#' the same header into a single `from`/`to`/`label` row.
#'
#' @param gts_span  The `table_styling$spanning_header` tibble from a gtsummary
#'   object, or `NULL`.
#' @param ordered_cols  Character vector of column names in display order
#'   (i.e. `names(processed_df)`).
#' @noRd
convert_gts_spanning_header <- function(gts_span, ordered_cols) {
  if (is.null(gts_span) || !is.data.frame(gts_span))              return(NULL)
  if (!all(c("column", "spanning_header") %in% names(gts_span)))  return(NULL)

  # When multiple modify_spanning_header() calls exist, keep only the
  # highest level (most recent) entry per column.
  if ("level" %in% names(gts_span) && nrow(gts_span) > 0) {
    gts_span <- gts_span[order(gts_span$level), , drop = FALSE]
    gts_span <- gts_span[!duplicated(gts_span$column, fromLast = TRUE), , drop = FALSE]
  }

  # Drop explicitly removed rows and blank / NA headers
  if ("remove" %in% names(gts_span)) {
    gts_span <- gts_span[!gts_span$remove, , drop = FALSE]
  }
  gts_span <- gts_span[
    !is.na(gts_span$spanning_header) & nzchar(gts_span$spanning_header),
    , drop = FALSE
  ]
  if (nrow(gts_span) == 0) return(NULL)

  # Map column names to positions in ordered_cols; drop unknowns
  positions <- match(gts_span$column, ordered_cols)
  valid     <- !is.na(positions)
  gts_span  <- gts_span[valid, , drop = FALSE]
  positions <- positions[valid]
  if (length(positions) == 0) return(NULL)

  # Sort by display position so from <= to is guaranteed
  ord       <- order(positions)
  gts_span  <- gts_span[ord, , drop = FALSE]
  positions <- positions[ord]

  # Collapse runs of consecutive columns sharing the same header into one span
  result <- list()
  i      <- 1L
  while (i <= nrow(gts_span)) {
    hdr <- gts_span$spanning_header[i]
    j   <- i
    while (j < nrow(gts_span) &&
           gts_span$spanning_header[j + 1L] == hdr &&
           positions[j + 1L] == positions[j] + 1L) {
      j <- j + 1L
    }
    result[[length(result) + 1L]] <- data.frame(
      from  = ordered_cols[positions[i]],
      to    = ordered_cols[positions[j]],
      label = strip_md_bold(hdr),
      stringsAsFactors = FALSE
    )
    i <- j + 1L
  }

  if (length(result) == 0) return(NULL)
  do.call(rbind, result)
}


#' Resolve a spanning-header endpoint to a column name
#'
#' Numeric endpoints are treated as display positions. Character endpoints must
#' match a value in `cols`.
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
  spanning_header_args <- tryCatch(names(formals(spanning_header_fn)), error = function(e) character(0))
  supports_spanning_bold <- "bold" %in% spanning_header_args

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
      call_args <- list(
        x = tbl_obj, from = from_idx, to = to_idx,
        label = label_val, standard_eval = TRUE
      )
      if (supports_spanning_bold) call_args$bold <- TRUE
      tbl_obj <- do.call(spanning_header_fn, call_args)
    } else if (!is.null(from_col) && !is.null(to_col)) {
      call_args <- list(
        x = tbl_obj, from = from_col, to = to_col,
        label = label_val, standard_eval = TRUE
      )
      if (supports_spanning_bold) call_args$bold <- TRUE
      tbl_obj <- do.call(spanning_header_fn, call_args)
    }
  }

  tbl_obj
}


# ---------------------------------------------------------------------------
# Report building helpers
# ---------------------------------------------------------------------------

#' Build a reporter table specification from a processed data frame
#'
#' `NULL` widths are passed through deliberately so `reporter` can calculate
#' output-specific widths from the target format, font, headers, and contents.
#' Non-NULL widths represent explicit user choices.
#'
#' @param df               Processed data frame (one page chunk or full table).
#' @param col_widths       Named numeric vector of manual column widths or `NULL`.
#' @param table_width      Optional total table width passed to reporter.
#' @param col_map          Data frame with `column` and `label` (visible cols).
#' @param cols_to_define   Character vector: non-label, non-group columns.
#' @param center_cols      Columns to centre-align (subset of `cols_to_define`).
#' @param label_overrides  Named list of column-label overrides or `NULL`.
#' @param group_cols_to_hide  Columns to hide with `blank_after`.
#' @param span_use         Normalised spanning-header spec or `NULL`.
#' @param ordered_cols     Column names in display order.
#' @param spanning_header_fn  `reporter::spanning_header` or `NULL`.
#' @param debug_spanning   Logical; passed to `apply_spanning_headers()`.
#' @noRd
build_table_spec <- function(df, col_widths = NULL, table_width = NULL,
                              col_map, cols_to_define, center_cols,
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
    width           = table_width,
    first_row_blank = TRUE,
    header_bold     = TRUE,
    borders         = c("top", "bottom")
  ) %>%
    reporter::column_defaults(
      align = "left"
    )

  if ("label" %in% names(df)) {
    label_width <- if (!is.null(col_widths) && "label" %in% names(col_widths)) {
      col_widths[["label"]]
    } else {
      NULL
    }
    tbl <- reporter::define(tbl,
                            label,
                            label = label_header,
                            width = label_width,
                            align = "left")
  }

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

    this_width <- if (!is.null(col_widths) && this_col %in% names(col_widths)) {
      col_widths[[this_col]]
    } else {
      NULL
    }
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
#'   * `tbl` - the (possibly modified) reporter table object.
#'   * `applied` - `TRUE` if footnotes were successfully applied.
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
#' This keeps the package compatible with reporter versions that do not accept
#' every optional argument used by newer releases.
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

#' Infer reporter's TXT column widths for an automatic-width table
#'
#' For TXT output, leading-space hierarchy labels need pre-wrapping so
#' continuation lines keep the same indentation. Rather than recreating
#' reporter's width algorithm, this helper asks reporter to paginate a temporary
#' table and reuses the column widths it calculated.
#'
#' @param df            Processed data frame.
#' @param tbl_spec_args Arguments forwarded to `build_table_spec()`.
#' @param rpt           A reporter report object configured for TXT output.
#'
#' @return Named numeric vector of column widths, or `NULL` when reporter
#'   internals are unavailable.
#' @noRd
infer_reporter_txt_col_widths <- function(df, tbl_spec_args, rpt) {
  page_setup_fn <- tryCatch(
    utils::getFromNamespace("page_setup", "reporter"),
    error = function(e) NULL
  )
  paginate_content_fn <- tryCatch(
    utils::getFromNamespace("paginate_content", "reporter"),
    error = function(e) NULL
  )
  if (is.null(page_setup_fn) || is.null(paginate_content_fn)) return(NULL)

  tryCatch(
    {
      tbl <- do.call(build_table_spec, c(list(df = df), tbl_spec_args))
      rpt_tmp <- add_content_safe(rpt, tbl, blank_row = "none", align = "left")
      rpt_tmp <- reporter::options_fixed(rpt_tmp, uchar = "_")
      rpt_tmp <- page_setup_fn(rpt_tmp)
      widths <- paginate_content_fn(rpt_tmp, rpt_tmp$content)[["widths"]]
      if (length(widths) == 0L) return(NULL)
      widths[[1L]]
    },
    error = function(e) NULL
  )
}
