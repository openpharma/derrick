test_that("strip_md_bold removes ** markers", {
  expect_equal(crane.reporter:::strip_md_bold("**bold**"),     "bold")
  expect_equal(crane.reporter:::strip_md_bold("__bold__"),     "bold")
  expect_equal(crane.reporter:::strip_md_bold("normal"),       "normal")
  expect_null(crane.reporter:::strip_md_bold(NULL))
  expect_equal(crane.reporter:::strip_md_bold(c("**a**", "**b**")), c("a", "b"))
  # Nested bold stays stripped (outer markers only)
  expect_equal(crane.reporter:::strip_md_bold("**a** and **b**"), "a and b")
})

test_that("collect_env_lines picks up numbered variables", {
  e <- new.env(parent = emptyenv())
  e$title1 <- "First"
  e$title2 <- "Second"
  e$title4 <- "Fourth"          # gap: title3 missing → stops at 4

  res <- crane.reporter:::collect_env_lines("title", max_n = 9L, env = e)
  expect_equal(res, c("First", "Second", "Fourth"))
})

test_that("collect_env_lines ignores NA and empty strings", {
  e <- new.env(parent = emptyenv())
  e$footnote1 <- NA_character_
  e$footnote2 <- ""
  e$footnote3 <- "Valid"

  res <- crane.reporter:::collect_env_lines("footnote", max_n = 5L, env = e)
  expect_equal(res, "Valid")
})

test_that("collect_env_lines returns character(0) when nothing found", {
  e   <- new.env(parent = emptyenv())
  res <- crane.reporter:::collect_env_lines("title", max_n = 3L, env = e)
  expect_equal(res, character(0))
})

# ---------------------------------------------------------------------------
test_that("resolve_rows handles NULL", {
  expect_equal(crane.reporter:::resolve_rows(NULL, data.frame()), integer(0))
})

test_that("resolve_rows handles logical vector", {
  df  <- data.frame(x = 1:5)
  idx <- crane.reporter:::resolve_rows(c(TRUE, FALSE, TRUE, FALSE, TRUE), df)
  expect_equal(idx, c(1L, 3L, 5L))
})

test_that("resolve_rows handles numeric vector", {
  df  <- data.frame(x = 1:5)
  idx <- crane.reporter:::resolve_rows(c(2, 4), df)
  expect_equal(idx, c(2, 4))
})

test_that("resolve_rows handles a list of specs", {
  df  <- data.frame(x = 1:5)
  idx <- crane.reporter:::resolve_rows(list(c(1L, 2L), c(3L, 4L)), df)
  expect_equal(sort(idx), 1:4)
})

# ---------------------------------------------------------------------------
test_that("get_col_label returns attr label when set", {
  x <- structure(1:3, label = "My Column")
  expect_equal(crane.reporter:::get_col_label(x, "default"), "My Column")
})

test_that("get_col_label returns default when label attr is missing", {
  x <- 1:3
  expect_equal(crane.reporter:::get_col_label(x, "fallback"), "fallback")
})

test_that("get_col_label returns default for empty label", {
  x <- structure(1:3, label = "")
  expect_equal(crane.reporter:::get_col_label(x, "fallback"), "fallback")
})

# ---------------------------------------------------------------------------
test_that("calc_col_width respects min and max", {
  # Very short content → min_width
  expect_equal(crane.reporter:::calc_col_width("a", min_width = 0.8, max_width = 3.5), 0.8)
  # Very long content → max_width
  long <- paste(rep("x", 200), collapse = "")
  expect_equal(crane.reporter:::calc_col_width(long, min_width = 0.8, max_width = 3.5), 3.5)
})

test_that("calc_col_width returns finite value for NA input", {
  w <- crane.reporter:::calc_col_width(NA_character_, min_width = 0.5)
  expect_true(is.finite(w))
  expect_gte(w, 0.5)
})

# ---------------------------------------------------------------------------
test_that("parse_column_widths returns NULL for NULL input", {
  expect_null(crane.reporter:::parse_column_widths(NULL, n_cols = 3))
})

test_that("parse_column_widths parses pipe-delimited string", {
  res <- crane.reporter:::parse_column_widths("10|5|5", n_cols = 3)
  expect_equal(res, c(10, 5, 5))
})

test_that("parse_column_widths extends short vector with last value", {
  res <- crane.reporter:::parse_column_widths(c(2, 1), n_cols = 4)
  expect_equal(res, c(2, 1, 1, 1))
})

test_that("parse_column_widths truncates long vector", {
  res <- crane.reporter:::parse_column_widths(c(1, 2, 3, 4, 5), n_cols = 3)
  expect_equal(res, c(1, 2, 3))
})

test_that("parse_column_widths returns NULL for non-numeric string", {
  expect_null(crane.reporter:::parse_column_widths("a|b|c", n_cols = 3))
})

# ---------------------------------------------------------------------------
test_that("adjust_col_widths keeps widths that fit", {
  w   <- c(a = 2, b = 2, c = 2)
  res <- crane.reporter:::adjust_col_widths(w, max_total = 10, min_width = 0.5)
  expect_equal(res, w)
})

test_that("adjust_col_widths scales down over-budget widths", {
  w   <- c(a = 5, b = 5, c = 5)
  res <- crane.reporter:::adjust_col_widths(w, max_total = 9, min_width = 0.5)
  expect_lte(sum(res), 9 + 1e-9)
  expect_true(all(res >= 0.5))
})

test_that("adjust_col_widths handles non-finite values", {
  w   <- c(a = Inf, b = 2)
  res <- crane.reporter:::adjust_col_widths(w, max_total = 5, min_width = 1)
  expect_true(all(is.finite(res)))
})

# ---------------------------------------------------------------------------
# label-first width logic (inline in gtsummary_to_reporter_output) —
# tested here via the helper primitives it composes
# ---------------------------------------------------------------------------

# Shared helper: runs the label-first logic as it appears in the main function
label_first <- function(col_widths, max_table_width,
                         user_constrained, min_col_width = 0.6) {
  label_target    <- col_widths[["label"]]
  other_cols      <- setdiff(names(col_widths), "label")
  min_other_total <- length(other_cols) * min_col_width
  max_label_width <- max_table_width - min_other_total
  label_width     <- max(min(label_target, max_label_width), min_col_width)
  other_widths    <- col_widths[other_cols]
  other_widths[!is.finite(other_widths)] <- min_col_width
  other_widths    <- pmax(other_widths, min_col_width)
  total_other     <- sum(other_widths)
  remaining       <- max_table_width - label_width
  if (total_other > remaining && total_other > 0) {
    other_widths <- other_widths * (remaining / total_other)
  } else if (user_constrained) {
    slack       <- max_table_width - label_width - sum(other_widths)
    label_width <- label_width + max(0, slack)
  }
  c(label = label_width, other_widths)
}

# auto-sum (5.75) < mtw (9): no constraint → no expansion
test_that("label-first: no expansion when width not user-constrained", {
  cw  <- c(label = 0.875, s1 = 1.375, s2 = 1.375, s3 = 2.125)
  res <- label_first(cw, max_table_width = 9, user_constrained = FALSE)
  expect_equal(sum(res), sum(cw))           # unchanged
  expect_equal(res[["label"]], cw[["label"]])
})

# user sets max_table_width = 9 → label expands to fill the 3.25-inch slack
test_that("label-first: label expands to fill user-constrained width", {
  cw  <- c(label = 0.875, s1 = 1.375, s2 = 1.375, s3 = 2.125)
  res <- label_first(cw, max_table_width = 9, user_constrained = TRUE)
  expect_equal(sum(res), 9, tolerance = 1e-9)
  expect_gt(res[["label"]], cw[["label"]])  # label got wider
})

# user sets max_table_width = 7 (between auto-sum and page)
test_that("label-first: label expands to exactly max_table_width = 7", {
  cw  <- c(label = 0.875, s1 = 1.375, s2 = 1.375, s3 = 2.125)
  res <- label_first(cw, max_table_width = 7, user_constrained = TRUE)
  expect_equal(sum(res), 7, tolerance = 1e-9)
})

# user sets max_table_width < auto-sum → shrink path, user_constrained has no effect
test_that("label-first: shrinks when auto-sum exceeds max_table_width", {
  cw  <- c(label = 3.5, s1 = 3.5, s2 = 3.5)
  res <- label_first(cw, max_table_width = 7, user_constrained = TRUE)
  expect_lte(sum(res), 7 + 1e-9)
  expect_equal(res[["label"]], 3.5)  # label not touched when shrinking stats
})


  dims <- crane.reporter:::get_paper_dims("letter", "inches")
  expect_equal(dims, c(8.5, 11))
})

test_that("get_paper_dims converts to cm", {
  dims_in <- crane.reporter:::get_paper_dims("letter", "inches")
  dims_cm <- crane.reporter:::get_paper_dims("letter", "cm")
  expect_equal(dims_cm, dims_in * 2.54)
})

test_that("get_paper_dims accepts numeric vector", {
  dims <- crane.reporter:::get_paper_dims(c(7, 10), "inches")
  expect_equal(dims, c(7, 10))
})

test_that("get_paper_dims returns Inf for 'none'", {
  dims <- crane.reporter:::get_paper_dims("none", "inches")
  expect_true(all(is.infinite(dims)))
})

# ---------------------------------------------------------------------------
test_that("get_default_margins returns named vector in inches", {
  m <- crane.reporter:::get_default_margins("inches")
  expect_named(m, c("top", "right", "bottom", "left"))
  expect_true(all(m > 0))
})

test_that("get_default_margins returns larger values in cm", {
  m_in <- crane.reporter:::get_default_margins("inches")
  m_cm <- crane.reporter:::get_default_margins("cm")
  expect_true(all(m_cm > m_in))
})

# ---------------------------------------------------------------------------
test_that("normalize_margins returns defaults for NULL", {
  m <- crane.reporter:::normalize_margins(NULL, "inches")
  expect_named(m, c("top", "right", "bottom", "left"))
})

test_that("normalize_margins names a length-4 numeric", {
  m <- crane.reporter:::normalize_margins(c(0.5, 1, 0.5, 1), "inches")
  expect_named(m, c("top", "right", "bottom", "left"))
})

test_that("normalize_margins merges a partial list with defaults", {
  m_default <- crane.reporter:::get_default_margins("inches")
  m         <- crane.reporter:::normalize_margins(list(top = 2), "inches")
  expect_equal(m[["top"]], 2)
  expect_equal(m[["left"]], m_default[["left"]])
})

# ---------------------------------------------------------------------------
test_that("compute_max_table_width is positive for letter landscape", {
  w <- crane.reporter:::compute_max_table_width("letter", "landscape", "inches", NULL)
  # letter landscape: 11 inches wide, minus default margins 1+1 = 9
  expect_equal(w, 9)
})

test_that("compute_max_table_height is positive for letter portrait", {
  h <- crane.reporter:::compute_max_table_height("letter", "portrait", "inches", NULL)
  # letter portrait: 11 inches tall, minus default margins 0.5+0.5 = 10
  expect_equal(h, 10)
})

test_that("compute_max_table_width returns Inf for paper_size='none'", {
  w <- crane.reporter:::compute_max_table_width("none", "landscape", "inches", NULL)
  expect_true(is.infinite(w))
})

test_that("compute_max_table_width returns cm value 2.54x inches value", {
  w_in <- crane.reporter:::compute_max_table_width("letter", "landscape", "inches", NULL)
  w_cm <- crane.reporter:::compute_max_table_width("letter", "landscape", "cm", NULL)
  expect_equal(w_cm, w_in * 2.54)
})

# ---------------------------------------------------------------------------
test_that("estimate_rows_per_page returns NULL for zero rows", {
  expect_null(crane.reporter:::estimate_rows_per_page(
    0, 9, "letter", "landscape", "inches", NULL
  ))
})

test_that("estimate_rows_per_page returns a positive integer", {
  rpp <- crane.reporter:::estimate_rows_per_page(
    100, 9, "letter", "landscape", "inches", NULL
  )
  expect_true(!is.null(rpp))
  expect_true(rpp > 0)
  expect_true(rpp <= 100)
})

test_that("estimate_rows_per_page returns NULL for infinite paper", {
  rpp <- crane.reporter:::estimate_rows_per_page(
    50, 9, "none", "landscape", "inches", NULL
  )
  expect_null(rpp)
})

test_that("estimate_rows_per_page: lpi overrides font_size", {
  rpp_lpi  <- crane.reporter:::estimate_rows_per_page(100, lpi = 6L,
                 paper_size = "letter", orientation = "landscape",
                 units = "inches", margins = NULL)
  rpp_same <- crane.reporter:::estimate_rows_per_page(100, font_size = 99,
                 lpi = 6L,
                 paper_size = "letter", orientation = "landscape",
                 units = "inches", margins = NULL)
  expect_equal(rpp_lpi, rpp_same)
})

test_that("estimate_rows_per_page: larger font_size yields fewer rows", {
  rpp_small <- crane.reporter:::estimate_rows_per_page(500, font_size = 8,
                 paper_size = "letter", orientation = "landscape",
                 units = "inches", margins = NULL)
  rpp_large <- crane.reporter:::estimate_rows_per_page(500, font_size = 14,
                 paper_size = "letter", orientation = "landscape",
                 units = "inches", margins = NULL)
  expect_true(rpp_small > rpp_large)
})

test_that("estimate_rows_per_page: TXT at 6 LPI is stable regardless of font_size", {
  rpp_9  <- crane.reporter:::estimate_rows_per_page(500, font_size = 9,
               lpi = 6L, paper_size = "letter", orientation = "landscape",
               units = "inches", margins = NULL)
  rpp_14 <- crane.reporter:::estimate_rows_per_page(500, font_size = 14,
               lpi = 6L, paper_size = "letter", orientation = "landscape",
               units = "inches", margins = NULL)
  expect_equal(rpp_9, rpp_14)
})

# ---------------------------------------------------------------------------
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
  page_max     <- 9     # inches — letter landscape minus margins
  chars_width  <- 120 / 12  # = 10 inches (wider than page_max)
  result       <- min(page_max, chars_width)
  expect_equal(result, 9)   # page_max wins

  chars_width2 <- 84 / 12   # = 7 inches (narrower than page_max)
  result2      <- min(page_max, chars_width2)
  expect_equal(result2, 7)  # chars_width wins
})


  res <- crane.reporter:::normalize_column_labels(c(label = "Var", stat_1 = "PBO"))
  expect_equal(res, list(label = "Var", stat_1 = "PBO"))
})

test_that("normalize_column_labels handles data frame input", {
  df  <- data.frame(column = c("a", "b"), label = c("Col A", "Col B"),
                    stringsAsFactors = FALSE)
  res <- crane.reporter:::normalize_column_labels(df)
  expect_equal(res, list(a = "Col A", b = "Col B"))
})

test_that("normalize_column_labels returns NULL for unnamed vector", {
  expect_null(crane.reporter:::normalize_column_labels(c("A", "B")))
})

test_that("normalize_column_labels returns NULL for NULL", {
  expect_null(crane.reporter:::normalize_column_labels(NULL))
})

# ---------------------------------------------------------------------------
test_that("normalize_spanning_headers handles data frame input", {
  df  <- data.frame(from = 2, to = 3, label = "Treat",
                    stringsAsFactors = FALSE)
  res <- crane.reporter:::normalize_spanning_headers(df)
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("from", "to", "label"))
})

test_that("normalize_spanning_headers handles named list input", {
  lst <- list(from = 2, to = 3, label = "Treat")
  res <- crane.reporter:::normalize_spanning_headers(lst)
  expect_s3_class(res, "data.frame")
  expect_equal(res$label, "Treat")
})

test_that("normalize_spanning_headers handles list-of-lists", {
  lst <- list(
    list(from = 1, to = 2, label = "A"),
    list(from = 3, to = 4, label = "B")
  )
  res <- crane.reporter:::normalize_spanning_headers(lst)
  expect_equal(nrow(res), 2)
})

test_that("normalize_spanning_headers returns NULL for NULL", {
  expect_null(crane.reporter:::normalize_spanning_headers(NULL))
})

# ---------------------------------------------------------------------------
# convert_gts_spanning_header — gtsummary format → from/to/label
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

test_that("convert_gts_spanning_header: two consecutive columns → one span", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("Treatment", "Treatment"))
  cols <- c("label", "stat_1", "stat_2")
  res  <- crane.reporter:::convert_gts_spanning_header(gts, cols)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$from,  "stat_1")
  expect_equal(res$to,    "stat_2")
  expect_equal(res$label, "Treatment")
})

test_that("convert_gts_spanning_header: strips markdown bold from label", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("**Treatment**", "**Treatment**"))
  cols <- c("label", "stat_1", "stat_2")
  res  <- crane.reporter:::convert_gts_spanning_header(gts, cols)
  expect_equal(res$label, "Treatment")
})

test_that("convert_gts_spanning_header: two separate spans in one table", {
  gts  <- make_gts_span(
    c("stat_1", "stat_2", "stat_3", "stat_4"),
    c("Drug A", "Drug A", "Drug B", "Drug B")
  )
  cols <- c("label", "stat_1", "stat_2", "stat_3", "stat_4")
  res  <- crane.reporter:::convert_gts_spanning_header(gts, cols)
  expect_equal(nrow(res), 2L)
  expect_equal(res$from,  c("stat_1", "stat_3"))
  expect_equal(res$to,    c("stat_2", "stat_4"))
})

test_that("convert_gts_spanning_header: remove=TRUE rows are dropped", {
  gts  <- make_gts_span(c("stat_1", "stat_2"), c("Treatment", "Treatment"),
                         remove = TRUE)
  cols <- c("label", "stat_1", "stat_2")
  expect_null(crane.reporter:::convert_gts_spanning_header(gts, cols))
})

test_that("convert_gts_spanning_header: highest level wins when column appears twice", {
  gts <- rbind(
    make_gts_span(c("stat_1", "stat_2"), c("Old",     "Old"),     level = 1L),
    make_gts_span(c("stat_1", "stat_2"), c("**New**", "**New**"), level = 2L)
  )
  cols <- c("label", "stat_1", "stat_2")
  res  <- crane.reporter:::convert_gts_spanning_header(gts, cols)
  expect_equal(nrow(res), 1L)
  expect_equal(res$label, "New")
})

test_that("convert_gts_spanning_header: NULL / wrong format returns NULL", {
  expect_null(crane.reporter:::convert_gts_spanning_header(NULL, c("a", "b")))
  expect_null(crane.reporter:::convert_gts_spanning_header(
    data.frame(x = 1), c("a", "b")
  ))
})


test_that("resolve_span_col resolves numeric index to column name", {
  cols <- c("a", "b", "c")
  expect_equal(crane.reporter:::resolve_span_col(2, cols), "b")
})

test_that("resolve_span_col resolves character name", {
  cols <- c("a", "b", "c")
  expect_equal(crane.reporter:::resolve_span_col("c", cols), "c")
})

test_that("resolve_span_col returns NULL for out-of-bounds index", {
  expect_null(crane.reporter:::resolve_span_col(10, c("a", "b")))
})

test_that("resolve_span_col returns NULL for missing name", {
  expect_null(crane.reporter:::resolve_span_col("z", c("a", "b")))
})

test_that("resolve_span_index resolves numeric to integer", {
  expect_equal(crane.reporter:::resolve_span_index(2, c("a", "b", "c")), 2L)
})

test_that("resolve_span_index resolves character to position", {
  expect_equal(crane.reporter:::resolve_span_index("b", c("a", "b", "c")), 2L)
})

test_that("resolve_span_index returns NULL for out-of-bounds", {
  expect_null(crane.reporter:::resolve_span_index(5, c("a", "b")))
})

# ---------------------------------------------------------------------------
test_that("wrap_with_indent returns short text unchanged", {
  expect_equal(crane.reporter:::wrap_with_indent("Short", 40), "Short")
})

test_that("wrap_with_indent returns NA unchanged", {
  expect_equal(crane.reporter:::wrap_with_indent(NA_character_, 10), NA_character_)
})

test_that("wrap_with_indent wraps long text at word boundary", {
  txt <- "This is a sentence that is definitely longer than twenty characters"
  res <- crane.reporter:::wrap_with_indent(txt, 20)
  lines <- strsplit(res, "\n")[[1]]
  expect_true(length(lines) > 1)
  expect_true(all(nchar(lines) <= 20))
})

test_that("wrap_with_indent preserves indentation on continuation lines", {
  txt <- "    An indented sentence that needs wrapping because it is too long"
  res <- crane.reporter:::wrap_with_indent(txt, 25)
  lines <- strsplit(res, "\n")[[1]]
  # All continuation lines should start with the same indentation
  if (length(lines) > 1) {
    prefixes <- regmatches(lines, regexpr("^\\s*", lines))
    expect_true(all(prefixes[-1] == prefixes[1]))
  }
})

test_that("wrap_with_indent handles empty/whitespace-only strings", {
  expect_equal(crane.reporter:::wrap_with_indent("", 10),   "")
  expect_equal(crane.reporter:::wrap_with_indent("   ", 10), "   ")
})

# ---------------------------------------------------------------------------
# SOC / PT wrapping — dummy AE table data
# ---------------------------------------------------------------------------
# Simulates a typical adverse-event table:
#   SOC rows have no leading indent
#   PT  rows have 2-space indent
# When the label column is narrow (~24 chars at 12 CPI), long PT labels must
# wrap onto continuation lines that preserve the 2-space indent.
test_that("wrap_with_indent SOC/PT: SOC wraps without indent", {
  soc <- "Nervous system disorders and related events"
  res <- crane.reporter:::wrap_with_indent(soc, 24)
  lines <- strsplit(res, "\n")[[1]]
  prefixes <- regmatches(lines, regexpr("^\\s*", lines))
  expect_true(all(prefixes == ""))
  expect_true(all(nchar(lines) <= 24))
})

test_that("wrap_with_indent SOC/PT: PT continuation lines keep 2-space indent", {
  pt <- "  Dizziness postural with additional clinical detail"
  res <- crane.reporter:::wrap_with_indent(pt, 24)
  lines <- strsplit(res, "\n")[[1]]
  expect_true(length(lines) > 1)
  prefixes <- regmatches(lines, regexpr("^\\s*", lines))
  expect_true(all(prefixes == "  "))
  expect_true(all(nchar(lines) <= 24))
})

test_that("wrap_with_indent SOC/PT: deeply-indented PT keeps 4-space indent", {
  pt <- "    Preferred term with a very long label that must be wrapped"
  res <- crane.reporter:::wrap_with_indent(pt, 24)
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
  wrapped   <- vapply(labels, crane.reporter:::wrap_with_indent,
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


test_that("rows_to_text returns 'list' for a plain list", {
  expect_equal(crane.reporter:::rows_to_text(list(a = 1)), "list")
})

test_that("rows_to_text returns expr_text for atomic values (via rlang::is_expression)", {
  # rlang::is_expression() is TRUE for atomic R values, so expr_text is used
  expect_equal(crane.reporter:::rows_to_text(TRUE), "TRUE")
  expect_equal(crane.reporter:::rows_to_text(1L),   "1L")
})

test_that("apply_tbl_footnotes returns applied=FALSE for empty footnotes", {
  dummy_tbl <- list(content = "x")   # minimal placeholder
  res <- crane.reporter:::apply_tbl_footnotes(dummy_tbl, character(0))
  expect_equal(res$tbl,     dummy_tbl)
  expect_false(res$applied)
})

test_that("format_iso8601_utc includes UTC+0 suffix", {
  ts  <- as.POSIXct("2024-01-15 08:30:00", tz = "UTC")
  out <- crane.reporter:::format_iso8601_utc(ts)
  expect_true(grepl("UTC\\+0", out))
  expect_true(grepl("2024-01-15", out))
})
