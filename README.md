# derrick

![GitHub forks](https://img.shields.io/github/forks/openpharma/derrick?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/openpharma/derrick?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/openpharma/derrick)
![GitHub contributors](https://img.shields.io/github/contributors/openpharma/derrick)
![GitHub last commit](https://img.shields.io/github/last-commit/openpharma/derrick)
![GitHub pull requests](https://img.shields.io/github/issues-pr/openpharma/derrick)
![GitHub repo size](https://img.shields.io/github/repo-size/openpharma/derrick)
![GitHub language count](https://img.shields.io/github/languages/count/openpharma/derrick)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Downloads](https://img.shields.io/github/downloads/openpharma/derrick/latest/total)](https://tooomm.github.io/github-release-stats/?username=openpharma\&repository=derrick)
[![Current Version](https://img.shields.io/github/r-package/v/openpharma/derrick/main?color=purple\&label=package%20version)](https://github.com/openpharma/derrick/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/openpharma/derrick?color=red\&label=open%20issues)](https://github.com/openpharma/derrick/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)

[![BiocCheck](https://github.com/openpharma/derrick/actions/workflows/bioccheck.yaml/badge.svg)](https://github.com/openpharma/derrick/actions/workflows/bioccheck.yaml)

`derrick` converts `gtsummary` tables and plain data frames into
clinical-style `reporter` RTF and TXT outputs. The main entry point is
`gtsummary_reporter()`.

The package focuses on the output details that usually matter for clinical
tables:

- controlled page geometry and column widths
- `gtsummary` labels, indentation, p-values, and spanning headers
- RTF and fixed-width TXT outputs from the same table specification
- optional titles, footnotes, and footer program/date metadata
- optional processed data and ARD RDS exports
- reporter-managed pagination by default, with manual row chunks available
  when explicitly requested

## Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("openpharma/derrick")
```

Load the package:

```r
library(derrick)
```

## Quick Start

`gtsummary_reporter()` accepts either a `gtsummary` object or a plain
`data.frame`. The output file extension in `file_path` is used only to define
the base name; files are written according to `output_types`.

```r
library(derrick)

tbl <- data.frame(
  label = c("Age", "  Mean (SD)", "Sex", "  Female"),
  stat_1 = c("", "42.1 (12.3)", "", "10 (50.0%)"),
  stat_2 = c("", "43.8 (11.9)", "", "12 (60.0%)"),
  stringsAsFactors = FALSE
)

paths <- gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/demographics.rtf",
  output_types = c("RTF", "TXT"),
  save_rds = FALSE
)

paths
```

This writes:

- `outputs/demographics.rtf`
- `outputs/demographics.txt`

## gtsummary Example

The function reads `table_body` and `table_styling` from a `gtsummary` object,
including visible columns, labels, indentation rules, p-value columns, and
spanning header metadata.

```r
library(dplyr)
library(gtsummary)
library(derrick)

title1 <- "Table 14.1 Summary of Demographics"
footnote1 <- "Percentages are based on the number of subjects in each treatment group."
progname <- "programs/t-demog.R"

gts_tbl <- gtsummary::trial |>
  dplyr::select(trt, age, grade) |>
  gtsummary::tbl_summary(by = trt) |>
  gtsummary::add_p() |>
  gtsummary::modify_spanning_header(
    gtsummary::all_stat_cols() ~ "**Treatment Group**"
  )

paths <- gtsummary_reporter(
  gts_obj = gts_tbl,
  file_path = "outputs/t-demog.rtf",
  output_types = c("RTF", "TXT"),
  save_rds = TRUE
)
```

The following variables are read from the caller's environment when present:

- `title1` through `title9`
- `footnote1` through `footnote9`
- `progname`

Titles and footnotes are wrapped to the active page width before the report is
written. `progname` is included in the right side of the page footer with the
current date and time.

## Main Arguments

| Argument | Purpose |
| --- | --- |
| `gts_obj` | A `gtsummary` object or plain `data.frame`. |
| `file_path` | Output base path. `.rtf` and/or `.txt` are generated from this base. |
| `output_types` | Any combination of `"RTF"` and `"TXT"`. |
| `max_table_width` | Optional total table width cap in `report_units`. `NULL` uses the full printable page width. |
| `column_widths` | Optional manual widths in display-column order, as a numeric vector or pipe-delimited string. |
| `max_chars_per_line` | Optional TXT character budget. Converted to width using 12 characters per inch. |
| `rows_per_page` | Optional manual row chunk size. Leave `NULL` to use reporter's own pagination. |
| `column_labels` | Optional display labels, usually a named vector or a data frame with `column` and `label`. |
| `spanning_headers` | Optional manual spanning header definitions with `from`, `to`, and `label`. |
| `group_columns` | Optional grouping columns to hide and use for `blank_after`. |
| `save_rds` | Save processed table data and, when available, ARD data next to the output file. |

`rds_dir` is kept as a reserved compatibility argument. RDS files are currently
written beside `file_path` using the output base name.

## Width Parameters

`gtsummary_reporter()` interprets `max_table_width`, `min_col_width`,
`column_widths`, and `report_margins` in `report_units`. The effective page
width is:

```r
page_width - left_margin - right_margin
```

Width resolution follows this order:

1. Determine the printable page width from `report_paper_size`,
   `report_orientation`, `report_units`, and `report_margins`.
2. Use the full printable width when `max_table_width = NULL`; otherwise cap
   the requested value at the printable width.
3. If `max_chars_per_line` is supplied, convert it to physical width using
   12 characters per inch and apply the stricter limit.
4. Compute automatic widths from data and headers, or apply manual
   `column_widths`.
5. Scale columns down when their total is wider than the effective table width.

With default margins, the common effective page widths are:

| `report_paper_size` | `report_orientation` | inches | cm |
| --- | --- | ---: | ---: |
| `"letter"` | `"landscape"` | 9.00 | 22.86 |
| `"letter"` | `"portrait"` | 6.50 | 16.51 |
| `"legal"` | `"landscape"` | 12.00 | 30.48 |
| `"legal"` | `"portrait"` | 6.50 | 16.51 |
| `"A4"` | `"landscape"` | 9.69 | 24.62 |
| `"A4"` | `"portrait"` | 6.27 | 15.92 |
| `"RD4"` | `"landscape"` | 8.70 | 22.22 |
| `"RD4"` | `"portrait"` | 5.70 | 14.52 |

Use `max_table_width = NULL` unless you intentionally want a narrower table.
Values above the printable page width are capped automatically. If margins
exceed the physical page width, the effective width becomes `0`.

## Manual Column Widths

Manual `column_widths` are applied in display-column order. For most
`gtsummary` tables this means the `label` column first, followed by statistic
or data columns.

For the default 9-inch landscape letter page:

```r
gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  column_widths = "3|2|2|2",
  output_types = c("RTF", "TXT")
)
```

Common starting points:

| Columns | Example `column_widths` | Total |
| ---: | --- | ---: |
| 3 | `"3|3|3"` | 9.0 |
| 4 | `"3|2|2|2"` | 9.0 |
| 5 | `"3|1.5|1.5|1.5|1.5"` | 9.0 |
| 6 | `"2.5|1.3|1.3|1.3|1.3|1.3"` | 9.0 |

The practical total range is `n_cols * min_col_width` through the effective
`max_table_width`. If the supplied total is larger, widths are scaled down. If
`n_cols * min_col_width` is wider than the page, the per-column floor is
relaxed to `effective_width / n_cols`.

When fewer widths are supplied than display columns, the last value is repeated.
When more widths are supplied, extras are ignored.

## Column Labels and Spanning Headers

Use `column_labels` to override display headers:

```r
gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  column_labels = c(
    label = "Visit / Statistic",
    stat_1 = "Placebo",
    stat_2 = "Active"
  )
)
```

Use `spanning_headers` when you need explicit spans or when the input is a
plain data frame:

```r
gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  spanning_headers = data.frame(
    from = "stat_1",
    to = "stat_2",
    label = "Treatment Group",
    stringsAsFactors = FALSE
  )
)
```

For `gtsummary` inputs, existing `table_styling$spanning_header` metadata is
used when `spanning_headers = NULL`.

## Pagination

By default, `rows_per_page = NULL` leaves pagination to `reporter`.
`reporter::write_report()` computes page breaks from the selected output
format, fixed page metrics, and the actual wrapped title, header, row, and
footnote line counts.

Use `rows_per_page` only when you need manual pre-splitting:

```r
gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  rows_per_page = 24
)
```

Manual chunks are added to the same report with page breaks between chunks.
Reporter still writes the final RTF/TXT output after the chunks are assembled.

## Group Columns and Blank Lines

When `group_blank_after = TRUE`, columns named like `group1_level`,
`group2_level`, and so on are hidden and used with reporter's `blank_after`
behavior. You can override the grouping columns explicitly:

```r
gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  group_columns = c("group1_level"),
  group_blank_after = TRUE
)
```

Set `group_blank_after = FALSE` when grouping columns should not be used for
blank-line behavior.

## Output Files

The function returns a character vector of generated file paths.

```r
paths <- gtsummary_reporter(
  gts_obj = tbl,
  file_path = "outputs/table.rtf",
  output_types = c("RTF", "TXT"),
  save_rds = TRUE
)
```

With `save_rds = TRUE`, the function also writes:

- `<base>_output_data.rds`: the processed data sent to reporter
- `<base>_ard.rds`: ARD data when it can be gathered from the input object

If ARD extraction is unavailable, the report is still generated and a message
is printed.

## Diagnostics

Use these flags when validating table layout:

- `debug_indent = TRUE` prints how indentation rules matched table rows.
- `debug_spanning = TRUE` prints how spanning headers were resolved.

These diagnostics are intended for development and validation, not routine
production report generation.
