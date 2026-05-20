base_path <- testthat::test_path()
manual_output_dir <- file.path(base_path, "outputs")
dev <- FALSE

make_reporter_test_dir <- function(name) {
  output_dir <- file.path(
    tempdir(),
    paste0("derrick-gtsummary-reporter-", name, "-", Sys.getpid())
  )
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir
}

ard_extractor_available <- function() {
  if (requireNamespace("gtsummary", quietly = TRUE) &&
      exists("gather_ard", where = asNamespace("gtsummary"), inherits = FALSE)) {
    return(TRUE)
  }

  requireNamespace("cards", quietly = TRUE) &&
    exists("as_ard", where = asNamespace("cards"), inherits = FALSE)
}

make_ae_table <- function() {
  data.frame(
    label = c(
      "Serious adverse events",
      "  Subjects with at least one event",
      "  Events leading to study drug interruption",
      "Treatment-emergent adverse events",
      "  Headache",
      "  Nausea",
      "  Alanine aminotransferase increased"
    ),
    placebo = c("", "2 (10.0%)", "1 (5.0%)", "", "6 (30.0%)", "4 (20.0%)", "1 (5.0%)"),
    active_low = c("", "1 (5.0%)", "0", "", "5 (25.0%)", "3 (15.0%)", "2 (10.0%)"),
    active_high = c("", "3 (15.0%)", "2 (10.0%)", "", "7 (35.0%)", "5 (25.0%)", "3 (15.0%)"),
    total = c("", "6 (10.0%)", "3 (5.0%)", "", "18 (30.0%)", "12 (20.0%)", "6 (10.0%)"),
    stringsAsFactors = FALSE
  )
}

collapse_report_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

normalize_html_text <- function(x) {
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("<br>", " ", x, fixed = TRUE)
  x
}

ae_column_labels <- c(
  label = "System Organ Class / Preferred Term",
  placebo = "Placebo",
  active_low = "Active Low",
  active_high = "Active High",
  total = "Total"
)

ae_spanning_headers <- data.frame(
  from = "placebo",
  to = "total",
  label = "Treatment Group",
  stringsAsFactors = FALSE
)

test_that("gtsummary_reporter writes TXT output from a gtsummary summary", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")

  output_dir <- make_reporter_test_dir("demog")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  demog_tbl <- gtsummary::trial |>
    dplyr::select(trt, age, grade) |>
    gtsummary::tbl_summary(by = trt) |>
    gtsummary::add_p() |>
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ "**Treatment Group**"
    )

  title1 <- "Table 14.1.1 Summary of Demographics"
  title2 <- "Safety Population"
  footnote1 <- "Based on subjects in each treatment group."
  progname <- "programs/t-demog.R"

  txt_paths <- derrick::gtsummary_reporter(
    gts_obj = demog_tbl,
    file_path = file.path(output_dir, "t-demog.rtf"),
    output_types = "TXT",
    save_rds = TRUE
  )

  expect_length(txt_paths, 1L)
  expect_equal(tolower(tools::file_ext(txt_paths)), "txt")
  expect_true(file.exists(txt_paths))

  txt_output <- collapse_report_text(txt_paths)
  expect_match(txt_output, "Table 14\\.1\\.1 Summary of Demographics")
  expect_match(txt_output, "Safety Population")
  expect_match(txt_output, "Treatment Group")
  expect_match(txt_output, "Based on subjects in each treatment group\\.")
  expect_match(txt_output, "programs/t-demog\\.R")

  output_data_path <- file.path(output_dir, "t-demog_output_data.rds")
  expect_true(file.exists(output_data_path))

  output_data <- readRDS(output_data_path)
  expect_s3_class(output_data, "data.frame")
  expect_true(all(c("label", "stat_1", "stat_2", "p.value") %in% names(output_data)))
  expect_true(any(grepl("Age", output_data$label, fixed = TRUE)))
  expect_true(any(grepl("Grade", output_data$label, fixed = TRUE)))

  if (ard_extractor_available()) {
    expect_true(file.exists(file.path(output_dir, "t-demog_ard.rds")))
  }
})

test_that("gtsummary_reporter writes multiple outputs from a plain data frame", {
  skip_if_not_installed("reporter")

  output_dir <- make_reporter_test_dir("ae")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  title1 <- "Table 14.3.1 Overview of Adverse Events"
  title2 <- "Safety Population"
  footnote1 <- "Events are counted once per subject within each row."
  progname <- "programs/t-ae-overview.R"

  ae_paths <- derrick::gtsummary_reporter(
    gts_obj = make_ae_table(),
    file_path = file.path(output_dir, "t-ae-overview.rtf"),
    column_labels = ae_column_labels,
    spanning_headers = ae_spanning_headers,
    output_types = c("TXT", "HTML"),
    save_rds = FALSE
  )

  expect_length(ae_paths, 2L)
  expect_setequal(tolower(tools::file_ext(ae_paths)), c("txt", "html"))
  expect_true(all(file.exists(ae_paths)))
  expect_false(file.exists(file.path(output_dir, "t-ae-overview_output_data.rds")))

  txt_path <- ae_paths[tolower(tools::file_ext(ae_paths)) == "txt"]
  txt_output <- collapse_report_text(txt_path)

  expect_match(txt_output, "Table 14\\.3\\.1 Overview of Adverse Events")
  expect_match(txt_output, "Treatment Group")
  expect_match(txt_output, "System Organ Class / Preferred Term")
  expect_match(txt_output, "Serious adverse events")
  expect_match(txt_output, "Alanine aminotransferase increased")
  expect_match(txt_output, "programs/t-ae-overview\\.R")

  html_path <- ae_paths[tolower(tools::file_ext(ae_paths)) == "html"]
  html_output <- normalize_html_text(collapse_report_text(html_path))

  expect_match(html_output, "Overview of Adverse Events")
  expect_match(html_output, "Treatment Group")
  expect_match(html_output, "Alanine aminotransferase increased")
})

test_that("Output Test-01: Output a RTF file with gtsummary object as expected", {
  if (dev) {
    skip_if_not_installed("gtsummary")
    skip_if_not_installed("dplyr")

    dir.create(manual_output_dir, recursive = TRUE, showWarnings = FALSE)

    demog_tbl <- gtsummary::trial |>
      dplyr::select(trt, age, grade) |>
      gtsummary::tbl_summary(by = trt) |>
      gtsummary::add_p() |>
      gtsummary::modify_spanning_header(
        gtsummary::all_stat_cols() ~ "**Treatment Group**"
      )

    title1 <- "Table 14.1.1 Summary of Demographics"
    title2 <- "Safety Population"
    footnote1 <- "Percentages are based on subjects in each treatment group."
    progname <- "programs/t-demog.R"

    rtf_path <- file.path(manual_output_dir, "t-demog.rtf")
    demog_paths <- derrick::gtsummary_reporter(
      gts_obj = demog_tbl,
      file_path = rtf_path,
      output_types = "RTF",
      save_rds = TRUE
    )

    expect_equal(demog_paths, rtf_path)
    expect_true(file.exists(rtf_path))
    expect_true(file.exists(file.path(manual_output_dir, "t-demog_output_data.rds")))
    if (ard_extractor_available()) {
      expect_true(file.exists(file.path(manual_output_dir, "t-demog_ard.rds")))
    }
  } else {
    expect_true(TRUE)
  }
})
