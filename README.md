# derrick <img src="man/figures/logo.png" align="right" height="139" alt="derrick hex logo" />

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![BiocCheck](https://github.com/openpharma/derrick/actions/workflows/bioccheck.yaml/badge.svg)](https://github.com/openpharma/derrick/actions/workflows/bioccheck.yaml)
[![Current Version](https://img.shields.io/badge/package%20version-v0.1.1-purple)](https://github.com/openpharma/derrick/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/openpharma/derrick?color=red&label=open%20issues)](https://github.com/openpharma/derrick/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)

`derrick` is a focused export layer for clinical tables. Build an analysis
table with `gtsummary` or a plain data frame, then use `gtsummary_reporter()`
to write consistent `reporter` RTF, TXT, DOCX, PDF, and HTML outputs.

The package is designed for the output details that usually slow down clinical
reporting work:

- one table specification can produce RTF, TXT, DOCX, PDF, and HTML files;
- `gtsummary` labels, p-values, indentation, and spanning headers are carried
  into the report output;
- page geometry, margins, manual column widths, and TXT character budgets can
  be controlled explicitly;
- titles, footnotes, program names, timestamps, and optional RDS exports are
  handled in one call.

## Installation

```r
install.packages("remotes")
remotes::install_github("openpharma/derrick")
```

```r
library(derrick)
```

## Quick Start

The main workflow is:

1. create or receive a `gtsummary` table;
2. set optional report metadata in the caller environment;
3. call `gtsummary_reporter()` with the required output formats.

```r
library(dplyr)
library(gtsummary)
library(derrick)

dir.create("outputs", showWarnings = FALSE)

title1 <- "Table 14.1 Summary of Demographics"
footnote1 <- "Percentages are based on the number of subjects in each treatment group."
progname <- "programs/t-demog.R"

tbl <- gtsummary::trial |>
  dplyr::select(trt, age, grade) |>
  gtsummary::tbl_summary(by = trt) |>
  gtsummary::add_p() |>
  gtsummary::modify_spanning_header(
    gtsummary::all_stat_cols() ~ "**Treatment Group**"
  )

paths <- gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/t-demog.rtf",
  output_types = c("RTF", "TXT", "DOCX", "PDF", "HTML"),
  save_rds = TRUE
)

paths
```

This writes `outputs/t-demog.rtf`, `outputs/t-demog.txt`,
`outputs/t-demog.docx`, `outputs/t-demog.pdf`, and `outputs/t-demog.html`.
With `save_rds = TRUE`, the processed output data is also saved beside the
report, and ARD data is saved when it can be extracted from the input table.

## What derrick Preserves

For `gtsummary` inputs, `derrick` reads both `table_body` and `table_styling`.
That means the exported report can use the visible column order, display
labels, row indentation rules, p-value columns, and spanning header metadata
already defined by the analysis table.

Plain data frames are also supported when the table has already been assembled:

```r
tbl_df <- data.frame(
  label = c("Age", "  Mean (SD)", "Sex", "  Female"),
  stat_1 = c("", "42.1 (12.3)", "", "10 (50.0%)"),
  stat_2 = c("", "43.8 (11.9)", "", "12 (60.0%)"),
  stringsAsFactors = FALSE
)

gtsummary_reporter(
  gts_obj = tbl_df,
  file_path = "outputs/simple-table.rtf",
  output_types = c("RTF", "TXT", "HTML"),
  column_labels = c(label = "Characteristic", stat_1 = "Placebo", stat_2 = "Active"),
  spanning_headers = data.frame(
    from = "stat_1",
    to = "stat_2",
    label = "Treatment Group"
  )
)
```

## Layout Controls

Use defaults first. By default, `derrick` lets `reporter` calculate table and
column widths for the requested output format. Supply width arguments only when
the report shell requires a specific layout.

| Argument | Use when |
| --- | --- |
| `output_types` | You need one or more of `"RTF"`, `"TXT"`, `"DOCX"`, `"PDF"`, or `"HTML"`. |
| `max_table_width` | You need a table narrower than the printable page. |
| `column_widths` | You need exact column allocation, e.g. `"3|2|2|2"`. |
| `max_chars_per_line` | TXT output must fit a fixed character budget, such as 132 columns. |
| `report_orientation`, `report_paper_size`, `report_margins` | The table must match a specific page setup. |
| `rows_per_page` | You need manual row chunks before `reporter` writes the final files. |

Reporter-managed widths, wrapping, and pagination are used by default.
`max_chars_per_line` only constrains TXT output. Leave
`rows_per_page = NULL` unless a table needs fixed row chunks for a specific
deliverable.

## Metadata and Diagnostics

`gtsummary_reporter()` automatically looks for `title1` through `title9`,
`footnote1` through `footnote9`, and `progname` in the caller environment.
Title and footnote wrapping is handled by `reporter` when the report is
written.

For layout validation, use:

```r
gtsummary_reporter(tbl, "outputs/check.rtf", debug_indent = TRUE)
gtsummary_reporter(tbl, "outputs/check.rtf", debug_spanning = TRUE)
```

## Learn More

See `vignettes/clinical-table-workflow.Rmd` for the draft end-to-end workflow,
including column width strategy, plain data frame inputs, pagination choices,
and troubleshooting notes.
