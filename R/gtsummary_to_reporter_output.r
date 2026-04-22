# =============================================================================
# Script: gtsummary_to_reporter_output
#
# Packages referenced:
# - gtsummary: table building and styling
# - reporter: RTF/TXT/DOCX/HTML/PDF report generation
# - tidyverse: dplyr/tidyr helpers used in processing
#
# Function arguments (gtsummary_to_reporter_output):
# - gts_obj: gtsummary object or plain data.frame to export
# - file_path: output path (extension determines RTF/TXT names)
# - max_table_width: max width (inches/cm) for all columns combined
# - min_col_width: minimum width for any column
# - column_widths: manual widths (e.g., "10|5|5") or numeric vector
# - column_labels: override column header labels (named vector or data.frame)
# - spanning_headers: spanning header specs (data.frame or list)
# - report_orientation: "landscape" or "portrait"
# - report_paper_size: "letter", "legal", "a4", or numeric c(width, height)
# - report_units: "inches" or "cm"
# - report_margins: NULL or named vector/list (top/right/bottom/left)
# - report_font_size: base font size for table
# - indent_unit: spaces per indent unit for label column
# - output_types: c("RTF","TXT") output selection
# - save_rds: save processed data and ARD objects as RDS
# - rds_dir: output directory for RDS (uses file_path directory if empty)
# - rows_per_page: rows per page (auto if NULL)
# - debug_indent: print indent debug details
# - debug_spanning: print spanning header debug details
# - group_columns: optional grouping columns to hide and blank_after
# - group_blank_after: if TRUE, apply reporter blank_after by group column
#
# Environment variables (auto-used if defined):
# - title1-9: report title lines
# - footnote1-9: report footnotes
# - progname: footer program name (e.g., "ctr/t-bin-eff.r")
#
# Example:
# title1 <- "Table 14.1 Summary of Demographics"
# footnote1 <- "MMRM = Mixed Model for Repeated Measures"
# progname <- "ctr/t-bin-eff.r"
# gtsummary_to_reporter_output(
#   gts_obj = my_gts_table,
#   file_path = "Clinical_Trial_Output.rtf",
#   column_widths = "10|5|5",
#   column_labels = c(label = "Visit / Statistic", stat_1 = "PBO", stat_2 = "TRT"),
#   spanning_headers = data.frame(from = 2, to = 3, label = "Treatment Group")
# )
# =============================================================================

#' Export `gtsummary` tables to clinical-style `reporter` outputs
#'
#' Convert a `gtsummary` object (or a plain `data.frame`) into a `reporter`
#' table and write RTF and/or TXT outputs. The function is designed for
#' clinical reporting workflows and supports column labels, spanning headers,
#' pagination, indentation handling, and optional export of intermediate data.
#'
#' Environment variables are automatically consumed when available:
#' `title1`-`title9`, `footnote1`-`footnote9`, and `progname`.
#'
#' @param gts_obj A `gtsummary` object (with `table_body`/`table_styling`) or
#'   a plain `data.frame` to export.
#' @param file_path Output path. The extension is ignored and output files are
#'   written according to `output_types`.
#' @param max_table_width Maximum total table width in `report_units`. If
#'   `NULL`, derived from page geometry. Always capped at the page usable width
#'   (paper minus margins) to prevent table overflow.
#' @param min_col_width Minimum width allowed for any column, in `report_units`.
#' @param column_widths Optional manual column widths in `report_units`, either
#'   a numeric vector or a `"|"`-delimited string (e.g. `"10|5|5"`).
#' @param max_chars_per_line Optional integer. Constrains the total table width
#'   so that at most this many characters fit across one TXT line (Courier at
#'   12 CPI). Applied after `max_table_width`, so the stricter limit wins.
#'   Useful when the character budget is known (e.g. `max_chars_per_line = 132`
#'   for a 132-column terminal or legacy print file).
#' @param column_labels Optional column header overrides, as a named vector/list
#'   or a data frame with `column` and `label`.
#' @param spanning_headers Optional spanning header definitions, as a data frame
#'   or list with fields `from`, `to`, and `label`.
#' @param report_orientation Page orientation (`"landscape"` or `"portrait"`).
#' @param report_paper_size Paper size (`"letter"`, `"legal"`, `"a4"`, `"rd4"`,
#'   `"none"`, or numeric length-2 vector).
#' @param report_units Units for dimensions (`"inches"` or `"cm"`).
#' @param report_margins Optional margins as named vector/list (`top`, `right`,
#'   `bottom`, `left`) or numeric length-4 vector.
#' @param report_font_size Base report font size.
#' @param indent_unit Number of spaces per indent level in `label`.
#' @param output_types Output types to write; supported values are `"RTF"` and
#'   `"TXT"`.
#' @param save_rds Logical; when `TRUE`, save processed output data and ARD
#'   object (if available) as `.rds` files.
#' @param rds_dir Reserved argument for backward compatibility.
#' @param rows_per_page Optional maximum number of table rows per page. If
#'   `NULL`, estimated from page geometry and `report_font_size`.
#' @param debug_indent Logical; print indentation diagnostics.
#' @param debug_spanning Logical; print spanning-header diagnostics.
#' @param group_columns Optional grouping columns to hide and apply
#'   `blank_after`.
#' @param group_blank_after Logical; whether to apply `blank_after` based on
#'   grouping columns.
#'
#' @return A character vector containing generated output file paths.
#'
#' @examplesIf requireNamespace("reporter", quietly = TRUE)
#' gts_tbl <- gtsummary::trial |>
#'   dplyr::select(trt, age, grade) |>
#'   gtsummary::tbl_summary(by = trt) |>
#'   gtsummary::add_p()
#'
#' out <- gtsummary_to_reporter_output(
#'   gts_obj = gts_tbl,
#'   file_path = tempfile("clinical_report_", fileext = ".rtf"),
#'   output_types = "TXT",
#'   save_rds = FALSE
#' )
#'
#' out
#'
#' @export
gtsummary_to_reporter_output <- function(gts_obj, file_path = "Clinical_Report.rtf",
                                           max_table_width = NULL, min_col_width = 0.6,
                                           column_widths = NULL,
                                           column_labels = NULL,
                                           spanning_headers = NULL,
                                           report_orientation = "landscape",
                                           report_paper_size = "letter",
                                           report_units = "inches",
                                           report_margins = NULL,
                                           report_font_size = 9,
                                           indent_unit = 1,
                                           output_types = c("RTF", "TXT"),
                                           save_rds = TRUE,
                                           rds_dir = "rds",
                                           rows_per_page = NULL,
                                           max_chars_per_line = NULL,
                                           debug_indent = FALSE,
                                           debug_spanning = FALSE,
                                           group_columns = NULL,
                                           group_blank_after = TRUE) {

  # A. Extract data and styling metadata ----------------------------------------
  has_table_body <- is.data.frame(gts_obj) && "table_body" %in% names(gts_obj)
  is_plain_df    <- is.data.frame(gts_obj) && !has_table_body
  if (is_plain_df) {
    raw_body <- gts_obj
    styling  <- NULL
  } else {
    raw_body <- gts_obj$table_body
    styling  <- gts_obj$table_styling
  }

  # Collect title / footnote lines from the caller's environment
  title         <- collect_env_lines("title",    max_n = 9L, env = parent.frame())
  footnotes_vec <- collect_env_lines("footnote", max_n = 9L, env = parent.frame())
  title         <- strip_md_bold(title)
  footnotes_vec <- strip_md_bold(footnotes_vec)

  # B. Build indent map ----------------------------------------------------------
  indent_map  <- tibble::tibble(row_id = integer(), indent = integer())
  indent_tbl  <- if (!is.null(styling)) styling$indent else NULL
  indent_vals <- NULL

  if (!is.null(indent_tbl) && nrow(indent_tbl) > 0) {
    if ("n_spaces" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$n_spaces
    } else if ("indentation" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$indentation
    } else if ("indent" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$indent
    }
    if (is.null(indent_vals)) indent_vals <- rep(0L, nrow(indent_tbl))

    indent_vec <- rep(NA_integer_, nrow(raw_body))
    for (i in seq_len(nrow(indent_tbl))) {
      rows_spec  <- indent_tbl$rows[[i]]
      indent_val <- indent_vals[[i]]
      row_ids    <- resolve_rows(rows_spec, raw_body)
      if (is.null(indent_val) || is.na(indent_val)) indent_val <- 0L
      if (length(row_ids) > 0) indent_vec[row_ids] <- as.integer(indent_val)
    }

    indent_map <- tibble::tibble(
      row_id = which(!is.na(indent_vec)),
      indent = indent_vec[!is.na(indent_vec)]
    )
  }

  has_label_rows <- "row_type" %in% names(raw_body) &&
    any(raw_body$row_type == "label", na.rm = TRUE)

  # C. Build header data frame ---------------------------------------------------
  if (!is.null(styling) && !is.null(styling$header)) {
    header_df <- styling$header %>%
      mutate(
        column = as.character(column),
        label  = strip_md_bold(as.character(label)),
        hide   = as.logical(hide)
      )
  } else {
    col_labels <- vapply(raw_body, get_col_label, character(1), default = "")
    col_labels[col_labels == ""] <- names(raw_body)[col_labels == ""]
    header_df <- tibble::tibble(
      column = names(raw_body),
      label  = strip_md_bold(unname(col_labels)),
      hide   = FALSE
    )
  }

  visible_cols <- header_df %>%
    dplyr::filter(!hide) %>%
    dplyr::distinct(column) %>%
    dplyr::pull(column)
  visible_cols <- intersect(visible_cols, names(raw_body))

  pvalue_cols <- names(raw_body)[grepl("^p\\.value", names(raw_body))]

  # D. Build processed data frame ------------------------------------------------
  if (is_plain_df) {
    processed_df <- raw_body
  } else {
    group_cols_raw <- grep("^group\\d+_level$", names(raw_body), value = TRUE)
    selected_cols  <- unique(c(
      "label", "row_type", "variable",
      styling$header$column[!styling$header$hide],
      group_cols_raw
    ))
    processed_df <- raw_body %>%
      select(any_of(selected_cols)) %>%
      mutate(row_id = dplyr::row_number()) %>%
      dplyr::left_join(indent_map, by = "row_id") %>%
      mutate(
        label = if (nrow(indent_map) > 0) sub("^\\s+", "", label) else label,
        label = ifelse(!is.na(indent),
                       paste0(strrep(" ", indent * indent_unit), label),
                       label)
      )
  }

  if (isFALSE(group_blank_after)) {
    group_cols_to_hide <- character(0)
  } else if (!is.null(group_columns)) {
    group_cols_to_hide <- intersect(as.character(group_columns), names(processed_df))
  } else {
    group_cols_to_hide <- grep("^group\\d+_level$", names(processed_df), value = TRUE)
  }

  # D1. Optional indent diagnostics ---------------------------------------------
  if (isTRUE(debug_indent) && !is.null(indent_tbl) && nrow(indent_tbl) > 0) {
    debug_df <- raw_body %>%
      mutate(
        row_id = dplyr::row_number(),
        leading_spaces = ifelse(
          is.na(label), NA_integer_,
          nchar(label) - nchar(sub("^\\s+", "", label))
        )
      ) %>%
      select(row_id, row_type, variable, label, leading_spaces) %>%
      dplyr::left_join(indent_map, by = "row_id")

    message("---- INDENT DEBUG (first 20 rows) ----")
    print(utils::head(debug_df, 20))
    message("---- STYLING$INDENT ----")
    print(indent_tbl)

    if (nrow(indent_tbl) > 0) {
      indent_vals_dbg <- if (!is.null(indent_vals)) indent_vals else rep(0L, nrow(indent_tbl))

      rule_df <- purrr::map2_dfr(
        indent_tbl$rows,
        indent_vals_dbg,
        function(rows_spec, indent_val) {
          row_ids       <- resolve_rows(rows_spec, raw_body)
          sample_labels <- raw_body$label[row_ids]
          sample_labels <- sample_labels[!is.na(sample_labels)]
          sample_labels <- utils::head(sample_labels, 3)
          tibble::tibble(
            indent    = as.integer(indent_val),
            rows_expr = rows_to_text(rows_spec),
            match_n   = length(row_ids),
            example   = paste(sample_labels, collapse = " | ")
          )
        }
      )

      message("---- INDENT RULES (matches) ----")
      print(rule_df)
    }
  }

  # D2. P-value consolidation and formatting ------------------------------------
  if (!is_plain_df && length(pvalue_cols) > 0) {
    if ("variable" %in% names(processed_df)) {
      processed_df <- processed_df %>%
        dplyr::group_by(variable) %>%
        mutate(
          .has_p  = dplyr::if_any(any_of(pvalue_cols), ~ !is.na(.x) & .x != ""),
          .keep_p = if (any(.has_p)) dplyr::row_number() == min(which(.has_p)) else FALSE
        ) %>%
        dplyr::ungroup() %>%
        mutate(across(any_of(pvalue_cols), ~ ifelse(.keep_p, .x, NA)))
    } else {
      processed_df <- processed_df %>%
        mutate(
          .has_p  = dplyr::if_any(any_of(pvalue_cols), ~ !is.na(.x) & .x != ""),
          .keep_p = if (any(.has_p)) dplyr::row_number() == min(which(.has_p)) else FALSE
        ) %>%
        mutate(across(any_of(pvalue_cols), ~ ifelse(.keep_p, .x, NA)))
    }
  }

  if (!is_plain_df && length(pvalue_cols) > 0) {
    processed_df <- processed_df %>%
      mutate(
        across(
          any_of(pvalue_cols),
          ~ {
            x_num <- if (is.numeric(.x)) .x else suppressWarnings(as.numeric(.x))
            ifelse(is.na(x_num), "", formatC(x_num, format = "f", digits = 4))
          }
        )
      )
  }

  if (!is_plain_df) {
    processed_df <- processed_df %>%
      select(-row_id, -indent, -row_type, -variable,
             -any_of(c(".keep_p", ".has_p")))
  }

  # Ensure RDS directory exists early
  if (isTRUE(save_rds)) {
    rds_full_dir <- dirname(file_path)
    if (is.null(rds_full_dir) || rds_full_dir == "." || !nzchar(rds_full_dir)) {
      rds_full_dir <- getwd()
    }
    if (!dir.exists(rds_full_dir)) dir.create(rds_full_dir, recursive = TRUE)
  }

  missing_cols <- setdiff(
    header_df %>% dplyr::filter(!hide) %>% dplyr::pull(column),
    names(processed_df)
  )
  if (length(missing_cols) > 0) message(paste(missing_cols, collapse = ", "))

  # E. Column width calculation --------------------------------------------------

  # Resolve column-label overrides first (needed for header width contribution)
  label_overrides <- normalize_column_labels(column_labels)
  if (!is.null(label_overrides)) {
    label_overrides <- lapply(label_overrides, strip_md_bold)
  }

  # Build definitive header label map (visible cols + "label" pseudo-column)
  hdr_map <- header_df %>%
    dplyr::filter(!hide) %>%
    select(column, label)
  if (!is.null(label_overrides)) {
    hdr_map$label <- ifelse(
      hdr_map$column %in% names(label_overrides),
      unlist(label_overrides[hdr_map$column]),
      hdr_map$label
    )
  }
  hdr_map <- dplyr::bind_rows(
    tibble::tibble(column = "label", label = "Characteristic"),
    hdr_map
  )
  if (!is.null(label_overrides) && "label" %in% names(label_overrides)) {
    hdr_map$label[hdr_map$column == "label"] <- label_overrides[["label"]]
  }

  # Initial widths: max of data content width and header label width
  width_cols <- setdiff(names(processed_df), group_cols_to_hide)
  if (!"label" %in% width_cols && "label" %in% names(processed_df)) {
    width_cols <- c("label", width_cols)
  }
  col_widths   <- vapply(processed_df[width_cols], calc_col_width, numeric(1))
  label_widths <- setNames(
    vapply(hdr_map$label, calc_col_width, numeric(1)),
    hdr_map$column
  )
  shared <- intersect(names(col_widths), names(label_widths))
  col_widths[shared] <- pmax(col_widths[shared], label_widths[shared])

  # calc_col_width always works in inches; convert to report_units if needed
  if (tolower(report_units) == "cm") col_widths <- col_widths * 2.54

  # Track whether the user explicitly constrained the table width.  When TRUE,
  # the label column is expanded to fill any slack so the table reaches the
  # requested width (useful for giving footnotes more horizontal room).
  user_constrained_width <- !is.null(max_table_width) || !is.null(max_chars_per_line)

  # Resolve max_table_width: always cap at the page usable width so reporter
  # never receives columns that exceed the printable area.
  page_max <- compute_max_table_width(
    paper_size  = report_paper_size,
    orientation = report_orientation,
    units       = report_units,
    margins     = report_margins
  )
  if (is.null(max_table_width)) {
    max_table_width <- page_max
  } else {
    max_table_width <- min(max_table_width, page_max)
  }

  if (!is.null(max_chars_per_line)) {
    # TXT Courier uses exactly 12 CPI; convert character budget to physical width
    chars_width_in <- as.numeric(max_chars_per_line) / 12
    chars_width    <- if (tolower(report_units) == "cm") chars_width_in * 2.54 else chars_width_in
    max_table_width <- min(max_table_width, chars_width)
  }

  manual_widths <- parse_column_widths(column_widths, n_cols = length(col_widths))
  if (!is.null(manual_widths)) {
    col_widths        <- manual_widths
    names(col_widths) <- width_cols
    if (is.finite(max_table_width)) {
      col_widths <- adjust_col_widths(col_widths,
                                       max_total = max_table_width,
                                       min_width = min_col_width)
    }
  } else {
    if ("label" %in% names(col_widths) && is.finite(max_table_width)) {
      label_target    <- col_widths[["label"]]
      other_cols      <- setdiff(names(col_widths), "label")
      min_other_total <- length(other_cols) * min_col_width
      max_label_width <- max_table_width - min_other_total
      label_width     <- max(min(label_target, max_label_width), min_col_width)

      other_widths <- col_widths[other_cols]
      other_widths[!is.finite(other_widths)] <- min_col_width
      other_widths <- pmax(other_widths, min_col_width)

      total_other <- sum(other_widths)
      remaining   <- max_table_width - label_width
      if (total_other > remaining && total_other > 0) {
        other_widths <- other_widths * (remaining / total_other)
      } else if (user_constrained_width) {
        # User explicitly asked for a specific width: expand label to fill slack
        slack       <- max_table_width - label_width - sum(other_widths)
        label_width <- label_width + max(0, slack)
      }

      col_widths <- c(label = label_width, other_widths)
      col_widths <- col_widths[names(processed_df)]
    } else {
      col_widths <- adjust_col_widths(col_widths,
                                       max_total = max_table_width,
                                       min_width = min_col_width)
    }
  }

  # E1. Pre-wrap label column ---------------------------------------------------
  # reporter's TXT renderer uses exactly 12 CPI for Courier regardless of
  # font_size, and reserves 1 character per column for the inter-column
  # separator. The effective content width is therefore:
  #   floor(col_width_in * 12) - 1
  # col_widths are in report_units; convert to inches first for CPI arithmetic.
  # Pre-wrapping to this limit prevents reporter from re-wrapping our lines,
  # which would strip the leading-space indentation from continuation lines.
  if ("label" %in% names(processed_df) &&
      "label" %in% names(col_widths) &&
      is.finite(col_widths[["label"]])) {
    cpi             <- 12L
    label_width_in  <- if (tolower(report_units) == "cm") col_widths[["label"]] / 2.54
                       else col_widths[["label"]]
    max_label_chars <- max(10L, floor(label_width_in * cpi) - 1L)

    processed_df$label <- vapply(
      as.character(processed_df$label),
      wrap_with_indent,
      character(1L),
      max_chars = max_label_chars
    )
  }

  # E2. Column map and spanning header setup ------------------------------------
  col_map        <- header_df %>% dplyr::filter(!hide)
  cols_to_define <- setdiff(names(processed_df), c("label", group_cols_to_hide))
  ordered_cols   <- names(processed_df)
  center_cols    <- setdiff(cols_to_define, "label")

  span_use <- if (!is.null(spanning_headers)) {
    normalize_spanning_headers(spanning_headers)
  } else if (!is.null(styling)) {
    convert_gts_spanning_header(styling$spanning_header, ordered_cols)
  } else {
    NULL
  }
  spanning_header_fn <- tryCatch(
    getFromNamespace("spanning_header", "reporter"),
    error = function(e) NULL
  )

  # F. Assemble report -----------------------------------------------------------
  rpt <- reporter::create_report(
    font        = "Courier",
    orientation = report_orientation,
    paper_size  = report_paper_size,
    units       = report_units,
    font_size   = report_font_size
  ) %>%
    {
      if (length(title) > 0) {
        reporter::titles(., title, bold = TRUE,
                          font_size = report_font_size, align = "left")
      } else {
        .
      }
    }

  if (!is.null(report_margins)) {
    report_margins <- normalize_margins(report_margins, report_units)
    rpt <- do.call(reporter::set_margins,
                   c(list(x = rpt), as.list(report_margins)))
  }

  if (is.null(rows_per_page)) {
    geom_args <- list(
      row_count   = nrow(processed_df),
      paper_size  = report_paper_size,
      orientation = report_orientation,
      units       = report_units,
      margins     = report_margins
    )
    out_upper     <- toupper(output_types)
    rpp_candidates <- c(
      if ("RTF" %in% out_upper)
        do.call(estimate_rows_per_page, c(geom_args, list(font_size = report_font_size))),
      if ("TXT" %in% out_upper)
        # TXT renderer ignores font_size and uses fixed 6 LPI (Courier 12 CPI)
        do.call(estimate_rows_per_page, c(geom_args, list(lpi = 6L)))
    )
    rows_per_page <- if (length(rpp_candidates) > 0) min(rpp_candidates) else NULL
  }

  # Package all build_table_spec context into one list for DRY call sites
  tbl_spec_args <- list(
    col_widths         = col_widths,
    col_map            = col_map,
    cols_to_define     = cols_to_define,
    center_cols        = center_cols,
    label_overrides    = label_overrides,
    group_cols_to_hide = group_cols_to_hide,
    span_use           = span_use,
    ordered_cols       = ordered_cols,
    spanning_header_fn = spanning_header_fn,
    debug_spanning     = debug_spanning
  )

  footnote_applied <- FALSE

  if (!is.null(rows_per_page) && is.finite(rows_per_page) && rows_per_page > 0) {
    row_ids   <- seq_len(nrow(processed_df))
    chunk_ids <- split(row_ids, ceiling(row_ids / rows_per_page))

    for (i in seq_along(chunk_ids)) {
      chunk_df  <- processed_df[chunk_ids[[i]], , drop = FALSE]
      tbl_chunk <- do.call(build_table_spec, c(list(df = chunk_df), tbl_spec_args))

      fn_result        <- apply_tbl_footnotes(tbl_chunk, footnotes_vec)
      tbl_chunk        <- fn_result$tbl
      footnote_applied <- fn_result$applied

      rpt <- add_content_safe(
        rpt,
        tbl_chunk,
        blank_row  = "none",
        align      = "left",
        page_break = i < length(chunk_ids)
      )
    }
  } else {
    tbl      <- do.call(build_table_spec, c(list(df = processed_df), tbl_spec_args))
    fn_result        <- apply_tbl_footnotes(tbl, footnotes_vec)
    tbl              <- fn_result$tbl
    footnote_applied <- fn_result$applied

    rpt <- add_content_safe(rpt, tbl, blank_row = "none", align = "left")
  }

  # Fallback: attach footnotes to report when table-level application failed
  if (!isTRUE(footnote_applied) && length(footnotes_vec) > 0) {
    for (note in footnotes_vec) {
      rpt <- rpt %>% reporter::footnotes(note, blank_row = "none")
    }
  }

  # G. Footer and output --------------------------------------------------------
  output_types <- toupper(output_types)
  base_path    <- sub("\\.[^.]+$", "", file_path)
  output_paths <- character(0)

  progname_val <- ""
  if (exists("progname", envir = parent.frame(), inherits = TRUE)) {
    progname_val <- as.character(get("progname", envir = parent.frame(), inherits = TRUE))
  }
  today        <- Sys.Date()
  date9        <- toupper(sprintf(
    "%02d%s%04d",
    as.integer(format(today, "%d")),
    month.abb[as.integer(format(today, "%m"))],
    as.integer(format(today, "%Y"))
  ))
  hm_time      <- sub("^0", "", format(Sys.time(), "%H:%M"))
  footer_right <- trimws(paste(progname_val, date9, hm_time))
  base_name    <- tools::file_path_sans_ext(basename(file_path))

  # G1. RDS export --------------------------------------------------------------
  if (isTRUE(save_rds)) {
    rds_full_dir <- dirname(file_path)
    if (is.null(rds_full_dir) || rds_full_dir == "." || !nzchar(rds_full_dir)) {
      rds_full_dir <- getwd()
    }
    if (!is.null(rds_full_dir)) {
      saveRDS(processed_df, file.path(rds_full_dir, paste0(base_name, "_output_data.rds")))

      if (!is_plain_df) {
        ard_obj <- NULL
        if (exists("gather_ard", where = asNamespace("gtsummary"), inherits = FALSE)) {
          ard_obj <- tryCatch(gtsummary::gather_ard(gts_obj), error = function(e) NULL)
        } else if (exists("as_ard", where = asNamespace("cards"), inherits = FALSE)) {
          ard_obj <- tryCatch(cards::as_ard(gts_obj), error = function(e) NULL)
        }
        if (!is.null(ard_obj)) {
          saveRDS(ard_obj, file.path(rds_full_dir, paste0(base_name, "_ard.rds")))
        } else {
          message("ARD not saved: as_ard not available or failed.")
        }
      }
    }
  }

  # G2. Write RTF / TXT ---------------------------------------------------------
  if ("RTF" %in% output_types) {
    rtf_path <- paste0(base_path, ".rtf")
    rpt %>%
      reporter::page_footer(right = footer_right) %>%
      reporter::write_report(rtf_path, output_type = "RTF")
    output_paths <- c(output_paths, rtf_path)
  }

  if ("TXT" %in% output_types) {
    txt_path <- paste0(base_path, ".txt")
    rpt %>%
      reporter::page_footer(right = footer_right) %>%
      reporter::options_fixed(uchar = "_") %>%
      reporter::write_report(txt_path, output_type = "TXT")
    output_paths <- c(output_paths, txt_path)
  }

  message(paste("Report generated at:", paste(output_paths, collapse = ", ")))
  return(output_paths)
}


# =============================================================================
# Usage examples (commented out)
# =============================================================================
# 1. Build gtsummary object
# my_gts_table <- trial %>%
#   select(trt, age, grade, response) %>%
#   tbl_summary(by = trt) %>%
#   add_p() %>%
#   modify_spanning_header(all_stat_cols() ~ "**Treatment Group**") %>%
#   bold_labels()
#
# title1 <- "Table 14.1 Summary of Demographic and Baseline Characteristics"
# footnote1 <- "aa"
# progname <- "ctr/t-bin-eff.r"
#
# final_path <- gtsummary_to_reporter_output(
#   gts_obj = my_gts_table,
#   file_path = "Clinical_Trial_Output.rtf"
# )
#
# 2. Spanning header example
# span_tbl <- trial %>%
#   select(trt, age, grade, response) %>%
#   tbl_summary(by = trt) %>%
#   modify_spanning_header(all_stat_cols() ~ "**Treatment Group**")
#
# span_path <- gtsummary_to_reporter_output(
#   gts_obj = span_tbl,
#   file_path = "Clinical_Trial_Spanning_Header_Output.rtf",
#   spanning_headers = data.frame(
#     from = "stat_1", to = "stat_2", label = "Treatment Group",
#     stringsAsFactors = FALSE
#   ),
#   column_labels = c(label = "Visit / Statistic", stat_1 = "Placebo", stat_2 = "Active"),
#   debug_spanning = TRUE
# )
#
# 3. Plain tibble input
# simple_tbl <- tibble::tibble(
#   label = c("Row 1", "  Row 2 (pre-indented)", "Row 3"),
#   stat_1 = c("10 (50%)", "5 (25%)", "5 (25%)"),
#   stat_2 = c("12 (60%)", "4 (20%)", "4 (20%)")
# )
# title1 <- "Table 14.3 Simple Tibble Input"
# simple_path <- gtsummary_to_reporter_output(
#   gts_obj = simple_tbl,
#   file_path = "Clinical_Trial_Simple_Tibble_Output.rtf",
#   output_types = c("RTF", "TXT"),
#   column_widths = "10|5|5"
# )
