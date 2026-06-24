test_that("gtsummary_reporter is the exported entry point", {
  expect_true(is.function(derrick::gtsummary_reporter))
})

test_that("strip_md_bold removes ** markers", {
  expect_equal(derrick:::strip_md_bold("**bold**"),     "bold")
  expect_equal(derrick:::strip_md_bold("__bold__"),     "bold")
  expect_equal(derrick:::strip_md_bold("normal"),       "normal")
  expect_null(derrick:::strip_md_bold(NULL))
  expect_equal(derrick:::strip_md_bold(c("**a**", "**b**")), c("a", "b"))
  # Nested bold stays stripped (outer markers only)
  expect_equal(derrick:::strip_md_bold("**a** and **b**"), "a and b")
})

test_that("normalize_report_header_label applies clinical header conventions", {
  expect_equal(derrick:::normalize_report_header_label("Overall"), "Total")
  expect_equal(derrick:::normalize_report_header_label("Overall\nN (%)"), "Total\nN (%)")
  expect_equal(derrick:::normalize_report_header_label("Active N(%)"), "Active N (%)")
  expect_equal(derrick:::normalize_report_header_label("N(% of treated)"), "N (% of treated)")
  expect_equal(derrick:::normalize_report_header_label("**N (%)**"), "N (%)")
})

test_that("column_has_n_percent_values detects count-percent cells", {
  expect_true(derrick:::column_has_n_percent_values(c("", "12 (34.5)", NA)))
  expect_true(derrick:::column_has_n_percent_values("12 (34.5%)"))
  expect_false(derrick:::column_has_n_percent_values("12.3 (4.5)"))
  expect_false(derrick:::column_has_n_percent_values(c("", NA)))
})

test_that("add_n_percent_to_stat_headers appends second-line N percent labels", {
  header_df <- tibble::tibble(
    column = c("label", "stat_1", "stat_2", "stat_3", "stat_4", "stat_5", "stat_6"),
    label = c("", "Placebo", "Study Drug\nN (%)", "Mean", "Mean (SD)", "Value", "Flag\nN(% of treated)"),
    hide = FALSE
  )
  data <- tibble::tibble(
    label = c("A", "B"),
    stat_1 = c("12 (34.5)", ""),
    stat_2 = c("13 (40%)", ""),
    stat_3 = c("12.3", "4.5"),
    stat_4 = c("24 (9)", "22 (9)"),
    stat_5 = c("24 (9)", "22 (9)"),
    stat_6 = c("13 (40%)", "")
  )

  res <- derrick:::add_n_percent_to_stat_headers(header_df, data)

  expect_equal(res$label[res$column == "stat_1"], "Placebo\nN (%)")
  expect_equal(res$label[res$column == "stat_2"], "Study Drug\nN (%)")
  expect_equal(res$label[res$column == "stat_3"], "Mean")
  expect_equal(res$label[res$column == "stat_4"], "Mean (SD)")
  expect_equal(res$label[res$column == "stat_5"], "Value")
  expect_equal(res$label[res$column == "stat_6"], "Flag\nN (% of treated)")
})

test_that("collect_env_lines picks up numbered variables", {
  e <- new.env(parent = emptyenv())
  e$title1 <- "First"
  e$title2 <- "Second"
  e$title4 <- "Fourth"          # gap: title3 missing, stops at 4

  res <- derrick:::collect_env_lines("title", max_n = 9L, env = e)
  expect_equal(res, c("First", "Second", "Fourth"))
})

test_that("collect_env_lines ignores NA and empty strings", {
  e <- new.env(parent = emptyenv())
  e$footnote1 <- NA_character_
  e$footnote2 <- ""
  e$footnote3 <- "Valid"

  res <- derrick:::collect_env_lines("footnote", max_n = 5L, env = e)
  expect_equal(res, "Valid")
})

test_that("collect_env_lines returns character(0) when nothing found", {
  e   <- new.env(parent = emptyenv())
  res <- derrick:::collect_env_lines("title", max_n = 3L, env = e)
  expect_equal(res, character(0))
})

# ---------------------------------------------------------------------------
test_that("resolve_rows handles NULL", {
  expect_equal(derrick:::resolve_rows(NULL, data.frame()), integer(0))
})

test_that("resolve_rows handles logical vector", {
  df  <- data.frame(x = 1:5)
  idx <- derrick:::resolve_rows(c(TRUE, FALSE, TRUE, FALSE, TRUE), df)
  expect_equal(idx, c(1L, 3L, 5L))
})

test_that("resolve_rows handles numeric vector", {
  df  <- data.frame(x = 1:5)
  idx <- derrick:::resolve_rows(c(2, 4), df)
  expect_equal(idx, c(2, 4))
})

test_that("resolve_rows handles a list of specs", {
  df  <- data.frame(x = 1:5)
  idx <- derrick:::resolve_rows(list(c(1L, 2L), c(3L, 4L)), df)
  expect_equal(sort(idx), 1:4)
})

# ---------------------------------------------------------------------------
test_that("get_col_label returns attr label when set", {
  x <- structure(1:3, label = "My Column")
  expect_equal(derrick:::get_col_label(x, "default"), "My Column")
})

test_that("get_col_label returns default when label attr is missing", {
  x <- 1:3
  expect_equal(derrick:::get_col_label(x, "fallback"), "fallback")
})

test_that("get_col_label returns default for empty label", {
  x <- structure(1:3, label = "")
  expect_equal(derrick:::get_col_label(x, "fallback"), "fallback")
})

# ---------------------------------------------------------------------------
test_that("parse_column_widths returns NULL for NULL input", {
  expect_null(derrick:::parse_column_widths(NULL, n_cols = 3))
})

test_that("parse_column_widths parses pipe-delimited string", {
  res <- derrick:::parse_column_widths("10|5|5", n_cols = 3)
  expect_equal(res, c(10, 5, 5))
})

test_that("parse_column_widths extends short vector with last value", {
  res <- derrick:::parse_column_widths(c(2, 1), n_cols = 4)
  expect_equal(res, c(2, 1, 1, 1))
})

test_that("parse_column_widths truncates long vector", {
  res <- derrick:::parse_column_widths(c(1, 2, 3, 4, 5), n_cols = 3)
  expect_equal(res, c(1, 2, 3))
})

test_that("parse_column_widths returns NULL for non-numeric string", {
  expect_null(derrick:::parse_column_widths("a|b|c", n_cols = 3))
})

# ---------------------------------------------------------------------------
test_that("adjust_col_widths keeps widths that fit", {
  w   <- c(a = 2, b = 2, c = 2)
  res <- derrick:::adjust_col_widths(w, max_total = 10, min_width = 0.5)
  expect_equal(res, w)
})

test_that("adjust_col_widths scales down over-budget widths", {
  w   <- c(a = 5, b = 5, c = 5)
  res <- derrick:::adjust_col_widths(w, max_total = 9, min_width = 0.5)
  expect_lte(sum(res), 9 + 1e-9)
  expect_true(all(res >= 0.5))
})

test_that("adjust_col_widths handles non-finite values", {
  w   <- c(a = Inf, b = 2)
  res <- derrick:::adjust_col_widths(w, max_total = 5, min_width = 1)
  expect_true(all(is.finite(res)))
})

test_that("adjust_col_widths relaxes min_width when the page is too narrow", {
  w   <- c(a = 1, b = 1, c = 1, d = 1)
  res <- derrick:::adjust_col_widths(w, max_total = 2, min_width = 0.6)
  expect_equal(sum(res), 2, tolerance = 1e-9)
  expect_equal(unname(res), rep(0.5, 4), tolerance = 1e-9)
})

test_that("manual column widths are scaled to the effective page width", {
  manual   <- derrick:::parse_column_widths("10|5|5", n_cols = 3)
  page_max <- derrick:::compute_max_table_width(
    "letter", "landscape", "inches", NULL
  )
  res <- derrick:::adjust_col_widths(
    manual, max_total = page_max, min_width = 0.6
  )
  expect_equal(page_max, 9)
  expect_lte(sum(res), page_max + 1e-9)
  expect_true(all(res >= 0.6))
  expect_true(all(res < manual))
})

test_that("normalize_reporter_paper_size accepts reporter aliases", {
  expect_equal(derrick:::normalize_reporter_paper_size("letter"), "letter")
  expect_equal(derrick:::normalize_reporter_paper_size("legal"), "legal")
  expect_equal(derrick:::normalize_reporter_paper_size("a4"), "A4")
  expect_equal(derrick:::normalize_reporter_paper_size("A4"), "A4")
  expect_equal(derrick:::normalize_reporter_paper_size("rd4"), "RD4")
  expect_equal(derrick:::normalize_reporter_paper_size("RD4"), "RD4")
  expect_equal(derrick:::normalize_reporter_paper_size("none"), "none")
  expect_equal(derrick:::normalize_reporter_paper_size(c(7, 10)), c(7, 10))
})

test_that("supported_output_types lists all reporter output formats", {
  expect_equal(
    derrick:::supported_output_types(),
    c("RTF", "TXT", "DOCX", "PDF", "HTML")
  )
})

test_that("normalize_output_types normalizes case, whitespace, and duplicates", {
  res <- derrick:::normalize_output_types(c(" rtf ", "txt", "DOCX", "pdf", "html", "RTF"))
  expect_equal(res, c("RTF", "TXT", "DOCX", "PDF", "HTML"))
})

test_that("normalize_output_types rejects empty and unsupported values", {
  expect_error(
    derrick:::normalize_output_types(character(0)),
    "at least one supported output type"
  )
  expect_error(
    derrick:::normalize_output_types(c("RTF", "CSV")),
    "Unsupported `output_types`: CSV"
  )
})

test_that("output_file_extension maps output types to generated extensions", {
  expect_equal(derrick:::output_file_extension("RTF"),  "rtf")
  expect_equal(derrick:::output_file_extension("TXT"),  "txt")
  expect_equal(derrick:::output_file_extension("DOCX"), "docx")
  expect_equal(derrick:::output_file_extension("PDF"),  "pdf")
  expect_equal(derrick:::output_file_extension("HTML"), "html")
})

test_that("get_paper_dims returns named paper dimensions in inches", {
  dims <- derrick:::get_paper_dims("letter", "inches")
  expect_equal(dims, c(8.5, 11))
  expect_equal(derrick:::get_paper_dims("legal", "inches"), c(8.5, 14))
  expect_equal(derrick:::get_paper_dims("a4", "inches"), c(8.27, 11.69))
  expect_equal(derrick:::get_paper_dims("rd4", "inches"), c(7.7, 10.7))
})

test_that("get_paper_dims converts to cm", {
  dims_in <- derrick:::get_paper_dims("letter", "inches")
  dims_cm <- derrick:::get_paper_dims("letter", "cm")
  expect_equal(dims_cm, dims_in * 2.54)
  expect_equal(derrick:::get_paper_dims("A4", "cm"), c(21, 29.7))
  expect_equal(derrick:::get_paper_dims("RD4", "cm"), c(19.6, 27.3))
})

test_that("get_paper_dims accepts numeric vector", {
  dims <- derrick:::get_paper_dims(c(7, 10), "inches")
  expect_equal(dims, c(7, 10))
})

test_that("get_paper_dims returns Inf for 'none'", {
  dims <- derrick:::get_paper_dims("none", "inches")
  expect_true(all(is.infinite(dims)))
})

# ---------------------------------------------------------------------------
test_that("get_default_margins returns named vector in inches", {
  m <- derrick:::get_default_margins("inches")
  expect_named(m, c("top", "right", "bottom", "left"))
  expect_true(all(m > 0))
})

test_that("get_default_margins returns larger values in cm", {
  m_in <- derrick:::get_default_margins("inches")
  m_cm <- derrick:::get_default_margins("cm")
  expect_true(all(m_cm > m_in))
})

# ---------------------------------------------------------------------------
test_that("normalize_margins returns defaults for NULL", {
  m <- derrick:::normalize_margins(NULL, "inches")
  expect_named(m, c("top", "right", "bottom", "left"))
})

test_that("normalize_margins names a length-4 numeric", {
  m <- derrick:::normalize_margins(c(0.5, 1, 0.5, 1), "inches")
  expect_named(m, c("top", "right", "bottom", "left"))
})

test_that("normalize_margins merges a partial list with defaults", {
  m_default <- derrick:::get_default_margins("inches")
  m         <- derrick:::normalize_margins(list(top = 2), "inches")
  expect_equal(m[["top"]], 2)
  expect_equal(m[["left"]], m_default[["left"]])
})

# ---------------------------------------------------------------------------
test_that("compute_max_table_width covers default page width ranges in inches", {
  cases <- data.frame(
    paper = c("letter", "letter", "legal", "legal", "A4", "A4", "RD4", "RD4"),
    orientation = c(
      "landscape", "portrait", "landscape", "portrait",
      "landscape", "portrait", "landscape", "portrait"
    ),
    expected = c(9, 6.5, 12, 6.5, 9.69, 6.27, 8.7, 5.7),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(cases))) {
    w <- derrick:::compute_max_table_width(
      cases$paper[[i]], cases$orientation[[i]], "inches", NULL
    )
    expect_equal(
      w,
      cases$expected[[i]],
      tolerance = 1e-9,
      info = paste(cases$paper[[i]], cases$orientation[[i]])
    )
  }
})

test_that("compute_max_table_width returns Inf for paper_size='none'", {
  w <- derrick:::compute_max_table_width("none", "landscape", "inches", NULL)
  expect_true(is.infinite(w))
})

test_that("compute_max_table_width covers default page width ranges in cm", {
  cases <- data.frame(
    paper = c("letter", "letter", "legal", "legal", "A4", "A4", "RD4", "RD4"),
    orientation = c(
      "landscape", "portrait", "landscape", "portrait",
      "landscape", "portrait", "landscape", "portrait"
    ),
    expected = c(22.86, 16.51, 30.48, 16.51, 24.62, 15.92, 22.22, 14.52),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(cases))) {
    w_cm <- derrick:::compute_max_table_width(
      cases$paper[[i]], cases$orientation[[i]], "cm", NULL
    )
    expect_equal(
      w_cm,
      cases$expected[[i]],
      tolerance = 1e-9,
      info = paste(cases$paper[[i]], cases$orientation[[i]])
    )
  }
})

test_that("compute_max_table_width respects custom margins and numeric pages", {
  w <- derrick:::compute_max_table_width(
    "letter", "landscape", "inches",
    c(top = 0.5, right = 0.75, bottom = 0.5, left = 0.75)
  )
  expect_equal(w, 9.5)

  portrait <- derrick:::compute_max_table_width(
    c(8, 10), "portrait", "inches",
    list(left = 0.25, right = 0.75)
  )
  landscape <- derrick:::compute_max_table_width(
    c(8, 10), "landscape", "inches",
    list(left = 0.25, right = 0.75)
  )
  expect_equal(portrait, 7)
  expect_equal(landscape, 9)
})

test_that("compute_max_table_width bottoms out at zero when margins exceed page width", {
  w <- derrick:::compute_max_table_width(
    "letter", "portrait", "inches",
    c(top = 0.5, right = 5, bottom = 0.5, left = 5)
  )
  expect_equal(w, 0)
})

# max_chars_per_line conversion (inline logic, no package needed)
# ---------------------------------------------------------------------------
test_that("max_chars_per_line converts to inches via 12 CPI", {
  chars  <- 120L
  width  <- chars / 12   # = 10 inches
  expect_equal(width, 10)
})

test_that("max_chars_per_line in cm is 2.54x the inch equivalent", {
  chars     <- 120L
  width_in  <- chars / 12
  width_cm  <- width_in * 2.54
  expect_equal(width_cm, 25.4)
})

test_that("max_chars_per_line tighter than page_max wins", {
  page_max     <- 9     # inches, letter landscape minus margins
  chars_width  <- 120 / 12  # = 10 inches (wider than page_max)
  result       <- min(page_max, chars_width)
  expect_equal(result, 9)   # page_max wins

  chars_width2 <- 84 / 12   # = 7 inches (narrower than page_max)
  result2      <- min(page_max, chars_width2)
  expect_equal(result2, 7)  # chars_width wins
})


test_that("normalize_column_labels handles named vector input", {
  res <- derrick:::normalize_column_labels(c(label = "Var", stat_1 = "PBO"))
  expect_equal(res, list(label = "Var", stat_1 = "PBO"))
})

test_that("normalize_column_labels handles data frame input", {
  df  <- data.frame(column = c("a", "b"), label = c("Col A", "Col B"),
                    stringsAsFactors = FALSE)
  res <- derrick:::normalize_column_labels(df)
  expect_equal(res, list(a = "Col A", b = "Col B"))
})

test_that("normalize_column_labels returns NULL for unnamed vector", {
  expect_null(derrick:::normalize_column_labels(c("A", "B")))
})

test_that("normalize_column_labels returns NULL for NULL", {
  expect_null(derrick:::normalize_column_labels(NULL))
})

# ---------------------------------------------------------------------------
test_that("normalize_spanning_headers handles data frame input", {
  df  <- data.frame(from = 2, to = 3, label = "Treat",
                    stringsAsFactors = FALSE)
  res <- derrick:::normalize_spanning_headers(df)
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("from", "to", "label"))
})

test_that("normalize_spanning_headers handles named list input", {
  lst <- list(from = 2, to = 3, label = "Treat")
  res <- derrick:::normalize_spanning_headers(lst)
  expect_s3_class(res, "data.frame")
  expect_equal(res$label, "Treat")
})

test_that("normalize_spanning_headers handles list-of-lists", {
  lst <- list(
    list(from = 1, to = 2, label = "A"),
    list(from = 3, to = 4, label = "B")
  )
  res <- derrick:::normalize_spanning_headers(lst)
  expect_equal(nrow(res), 2)
})

test_that("normalize_spanning_headers returns NULL for NULL", {
  expect_null(derrick:::normalize_spanning_headers(NULL))
})

# ---------------------------------------------------------------------------
# convert_gts_spanning_header: gtsummary format to from/to/label
# ---------------------------------------------------------------------------
make_gts_span <- function(columns, headers, level = 1L, remove = FALSE) {
  data.frame(
    level           = level,
    column          = columns,
    spanning_header = headers,
    text_interpret  = "gt::md",
    remove          = remove,
    stringsAsFactors = FALSE
  )
}

test_that("convert_gts_spanning_header: two consecutive columns produce one span", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("Treatment", "Treatment"))
  cols <- c("label", "stat_1", "stat_2")
  res  <- derrick:::convert_gts_spanning_header(gts, cols)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$from,  "stat_1")
  expect_equal(res$to,    "stat_2")
  expect_equal(res$label, "Treatment")
})

test_that("convert_gts_spanning_header: strips markdown bold from label", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("**Treatment**", "**Treatment**"))
  cols <- c("label", "stat_1", "stat_2")
  res  <- derrick:::convert_gts_spanning_header(gts, cols)
  expect_equal(res$label, "Treatment")
})

test_that("convert_gts_spanning_header: two separate spans in one table", {
  gts  <- make_gts_span(
    c("stat_1", "stat_2", "stat_3", "stat_4"),
    c("Drug A", "Drug A", "Drug B", "Drug B")
  )
  cols <- c("label", "stat_1", "stat_2", "stat_3", "stat_4")
  res  <- derrick:::convert_gts_spanning_header(gts, cols)
  expect_equal(nrow(res), 2L)
  expect_equal(res$from,  c("stat_1", "stat_3"))
  expect_equal(res$to,    c("stat_2", "stat_4"))
})

test_that("convert_gts_spanning_header: remove=TRUE rows are dropped", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("Treatment", "Treatment"),
                         remove = TRUE)
  cols <- c("label", "stat_1", "stat_2")
  expect_null(derrick:::convert_gts_spanning_header(gts, cols))
})

test_that("convert_gts_spanning_header: highest level wins when column appears twice", {
  gts <- rbind(
    make_gts_span(c("stat_1", "stat_2"), c("Old",     "Old"),     level = 1L),
    make_gts_span(c("stat_1", "stat_2"), c("**New**", "**New**"), level = 2L)
  )
  cols <- c("label", "stat_1", "stat_2")
  res  <- derrick:::convert_gts_spanning_header(gts, cols)
  expect_equal(nrow(res), 1L)
  expect_equal(res$label, "New")
})

test_that("convert_gts_spanning_header: NULL / wrong format returns NULL", {
  expect_null(derrick:::convert_gts_spanning_header(NULL, c("a", "b")))
  expect_null(derrick:::convert_gts_spanning_header(
    data.frame(x = 1), c("a", "b")
  ))
})


test_that("resolve_span_col resolves numeric index to column name", {
  cols <- c("a", "b", "c")
  expect_equal(derrick:::resolve_span_col(2, cols), "b")
})

test_that("resolve_span_col resolves character name", {
  cols <- c("a", "b", "c")
  expect_equal(derrick:::resolve_span_col("c", cols), "c")
})

test_that("resolve_span_col returns NULL for out-of-bounds index", {
  expect_null(derrick:::resolve_span_col(10, c("a", "b")))
})

test_that("resolve_span_col returns NULL for missing name", {
  expect_null(derrick:::resolve_span_col("z", c("a", "b")))
})

test_that("resolve_span_index resolves numeric to integer", {
  expect_equal(derrick:::resolve_span_index(2, c("a", "b", "c")), 2L)
})

test_that("resolve_span_index resolves character to position", {
  expect_equal(derrick:::resolve_span_index("b", c("a", "b", "c")), 2L)
})

test_that("resolve_span_index returns NULL for out-of-bounds", {
  expect_null(derrick:::resolve_span_index(5, c("a", "b")))
})

test_that("apply_spanning_headers bolds reporter spanning headers by default", {
  skip_if_not_installed("reporter")

  df <- data.frame(label = "Row", stat_1 = "1", stat_2 = "2")
  tbl <- reporter::create_table(df)
  span_use <- data.frame(
    from = "stat_1",
    to = "stat_2",
    label = "Treatment",
    stringsAsFactors = FALSE
  )

  out <- derrick:::apply_spanning_headers(
    tbl_obj = tbl,
    span_use = span_use,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_length(out$col_spans, 1)
  expect_equal(out$col_spans[[1]]$label, "Treatment")
  expect_true(out$col_spans[[1]]$bold)
})

test_that("build_table_spec creates bold column headers by default", {
  skip_if_not_installed("reporter")

  df <- data.frame(label = "Row", stat_1 = "1")
  col_map <- data.frame(
    column = "stat_1",
    label = "Drug A",
    stringsAsFactors = FALSE
  )

  out <- derrick:::build_table_spec(
    df = df,
    col_widths = c(label = 2, stat_1 = 1),
    col_map = col_map,
    cols_to_define = "stat_1",
    center_cols = "stat_1",
    label_overrides = list(label = "Characteristic"),
    group_cols_to_hide = character(0),
    span_use = NULL,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_true(out$header_bold)
})

test_that("build_table_spec uses gtsummary label header without override", {
  skip_if_not_installed("reporter")

  df <- data.frame(label = "Row", stat_1 = "1")
  col_map <- data.frame(
    column = c("label", "stat_1"),
    label = c("System Organ Class /\n   Preferred term", "Drug A"),
    stringsAsFactors = FALSE
  )

  out <- derrick:::build_table_spec(
    df = df,
    col_widths = c(label = 2, stat_1 = 1),
    col_map = col_map,
    cols_to_define = "stat_1",
    center_cols = "stat_1",
    label_overrides = NULL,
    group_cols_to_hide = character(0),
    span_use = NULL,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_equal(out[["col_defs"]][["label"]][["label"]], "System Organ Class /\n   Preferred term")
})

test_that("build_table_spec leaves automatic widths to reporter", {
  skip_if_not_installed("reporter")

  df <- data.frame(label = "Row", stat_1 = "1")
  col_map <- data.frame(
    column = "stat_1",
    label = "Drug A",
    stringsAsFactors = FALSE
  )

  out <- derrick:::build_table_spec(
    df = df,
    col_widths = NULL,
    table_width = NULL,
    col_map = col_map,
    cols_to_define = "stat_1",
    center_cols = "stat_1",
    label_overrides = list(label = "Characteristic"),
    group_cols_to_hide = character(0),
    span_use = NULL,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_null(out[["width"]])
  expect_null(out[["col_defs"]][["label"]][["width"]])
  expect_null(out[["col_defs"]][["stat_1"]][["width"]])
})

test_that("build_table_spec forwards manual widths to reporter", {
  skip_if_not_installed("reporter")

  df <- data.frame(label = "Row", stat_1 = "1")
  col_map <- data.frame(
    column = "stat_1",
    label = "Drug A",
    stringsAsFactors = FALSE
  )

  out <- derrick:::build_table_spec(
    df = df,
    col_widths = c(label = 2, stat_1 = 1),
    table_width = 3,
    col_map = col_map,
    cols_to_define = "stat_1",
    center_cols = "stat_1",
    label_overrides = list(label = "Characteristic"),
    group_cols_to_hide = character(0),
    span_use = NULL,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_equal(out[["width"]], 3)
  expect_equal(out[["col_defs"]][["label"]][["width"]], 2)
  expect_equal(out[["col_defs"]][["stat_1"]][["width"]], 1)
})

test_that("build_table_spec applies reporter page_by and hides page-by columns", {
  skip_if_not_installed("reporter")

  df <- data.frame(
    section = c("Chemistry", "Chemistry", "Hematology"),
    label = c("ALT", "Albumin", "Hemoglobin"),
    stat_1 = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  col_map <- data.frame(
    column = "stat_1",
    label = "Drug A",
    stringsAsFactors = FALSE
  )

  out <- derrick:::build_table_spec(
    df = df,
    col_widths = NULL,
    table_width = NULL,
    col_map = col_map,
    cols_to_define = "stat_1",
    center_cols = "stat_1",
    label_overrides = list(label = "Parameter"),
    group_cols_to_hide = character(0),
    page_by_cols_to_hide = "section",
    page_by_label = "Category: ",
    span_use = NULL,
    ordered_cols = names(df),
    spanning_header_fn = reporter::spanning_header
  )

  expect_equal(out[["page_by"]][["var"]], "section")
  expect_equal(out[["page_by"]][["label"]], "Category: ")
  expect_true(out[["page_by"]][["bold"]])
  expect_false(out[["col_defs"]][["section"]][["visible"]])
})

# ---------------------------------------------------------------------------
test_that("wrap_with_indent returns short text unchanged", {
  expect_equal(derrick:::wrap_with_indent("Short", 40), "Short")
})

test_that("wrap_with_indent returns NA unchanged", {
  expect_equal(derrick:::wrap_with_indent(NA_character_, 10), NA_character_)
})

test_that("wrap_with_indent wraps long text at word boundary", {
  txt <- "This is a sentence that is definitely longer than twenty characters"
  res <- derrick:::wrap_with_indent(txt, 20)
  lines <- strsplit(res, "\n")[[1]]
  expect_true(length(lines) > 1)
  expect_true(all(nchar(lines) <= 20))
})

test_that("wrap_with_indent preserves indentation on continuation lines", {
  txt <- "    An indented sentence that needs wrapping because it is too long"
  res <- derrick:::wrap_with_indent(txt, 25)
  lines <- strsplit(res, "\n")[[1]]
  # All continuation lines should start with the same indentation
  if (length(lines) > 1) {
    prefixes <- regmatches(lines, regexpr("^\\s*", lines))
    expect_true(all(prefixes[-1] == prefixes[1]))
  }
})

test_that("wrap_with_indent handles empty/whitespace-only strings", {
  expect_equal(derrick:::wrap_with_indent("", 10),   "")
  expect_equal(derrick:::wrap_with_indent("   ", 10), "   ")
})

# ---------------------------------------------------------------------------
# SOC / PT wrapping: dummy AE table data
# ---------------------------------------------------------------------------
# Simulates a typical adverse-event table:
#   SOC rows have no leading indent
#   PT  rows have 2-space indent
# When the label column is narrow (~24 chars at 12 CPI), long PT labels must
# wrap onto continuation lines that preserve the 2-space indent.
test_that("wrap_with_indent SOC/PT: SOC wraps without indent", {
  soc <- "Nervous system disorders and related events"
  res <- derrick:::wrap_with_indent(soc, 24)
  lines <- strsplit(res, "\n")[[1]]
  prefixes <- regmatches(lines, regexpr("^\\s*", lines))
  expect_true(all(prefixes == ""))
  expect_true(all(nchar(lines) <= 24))
})

test_that("wrap_with_indent SOC/PT: PT continuation lines keep 2-space indent", {
  pt <- "  Dizziness postural with additional clinical detail"
  res <- derrick:::wrap_with_indent(pt, 24)
  lines <- strsplit(res, "\n")[[1]]
  expect_true(length(lines) > 1)
  prefixes <- regmatches(lines, regexpr("^\\s*", lines))
  expect_true(all(prefixes == "  "))
  expect_true(all(nchar(lines) <= 24))
})

test_that("wrap_with_indent SOC/PT: deeply-indented PT keeps 4-space indent", {
  pt <- "    Preferred term with a very long label that must be wrapped"
  res <- derrick:::wrap_with_indent(pt, 24)
  lines <- strsplit(res, "\n")[[1]]
  expect_true(length(lines) > 1)
  prefixes <- regmatches(lines, regexpr("^\\s*", lines))
  expect_true(all(prefixes == "    "))
  expect_true(all(nchar(lines) <= 24))
})

test_that("wrap_with_indent SOC/PT: full dummy AE table column is consistent", {
  # Mimic what processed_df$label looks like for a 2-SOC, 3-PT table
  labels <- c(
    "Nervous system disorders",
    "  Headache",
    "  Dizziness postural with additional clinical detail",
    "Gastrointestinal disorders",
    "  Nausea",
    "  Abdominal pain upper and lower region combined"
  )
  max_chars <- 24L
  wrapped   <- vapply(labels, derrick:::wrap_with_indent,
                      character(1L), max_chars = max_chars)

  for (i in seq_along(labels)) {
    original_indent <- nchar(regmatches(labels[i], regexpr("^\\s*", labels[i])))
    expected_prefix <- strrep(" ", original_indent)
    lines <- strsplit(wrapped[i], "\n")[[1]]
    prefixes <- regmatches(lines, regexpr("^\\s*", lines))
    expect_true(all(prefixes == expected_prefix),
                info = paste("indent mismatch for:", labels[i]))
    expect_true(all(nchar(lines) <= max_chars),
                info = paste("width exceeded for:", labels[i]))
  }
})

test_that("TXT automatic widths preserve indentation on wrapped label rows", {
  skip_if_not_installed("reporter")

  df <- data.frame(
    label = c(
      "Nervous system disorders",
      "  Dizziness postural with additional clinical detail that should wrap onto a continuation sentinel",
      "  Headache"
    ),
    stat_1 = c("", "1 (2.0%)", "2 (4.0%)"),
    stringsAsFactors = FALSE
  )

  out <- derrick::gtsummary_reporter(
    df,
    file.path(tempdir(), paste0("txt-indent-", Sys.getpid(), ".rtf")),
    output_types = "TXT",
    max_chars_per_line = 48,
    save_rds = FALSE
  )

  lines <- readLines(out, warn = FALSE)
  sentinel_lines <- lines[grepl("sentinel", lines)]
  expect_true(length(sentinel_lines) > 0L)
  expect_true(any(grepl("^  .*sentinel", sentinel_lines)))
})


test_that("rows_to_text returns 'list' for a plain list", {
  expect_equal(derrick:::rows_to_text(list(a = 1)), "list")
})

test_that("rows_to_text returns expr_text for atomic values (via rlang::is_expression)", {
  # rlang::is_expression() is TRUE for atomic R values, so expr_text is used
  expect_equal(derrick:::rows_to_text(TRUE), "TRUE")
  expect_equal(derrick:::rows_to_text(1L),   "1L")
})

test_that("apply_tbl_footnotes returns applied=FALSE for empty footnotes", {
  dummy_tbl <- list(content = "x")   # minimal placeholder
  res <- derrick:::apply_tbl_footnotes(dummy_tbl, character(0))
  expect_equal(res$tbl,     dummy_tbl)
  expect_false(res$applied)
})
