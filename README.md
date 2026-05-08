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

Convert `gtsummary` tables and plain data frames into clinical-style
`reporter` RTF and TXT outputs.

## Width parameters

`gtsummary_to_reporter_output()` interprets `max_table_width`,
`min_col_width`, and `column_widths` in `report_units`. The effective page width
is:

```r
page_width - left_margin - right_margin
```

`max_table_width = NULL` uses the full effective page width. A supplied
`max_table_width` is capped at that value, and `max_chars_per_line` can make it
narrower for TXT output (`max_chars_per_line / 12` inches).

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

Manual `column_widths` are applied in display-column order, usually `label`
first and then statistic columns. Their practical total range is
`n_cols * min_col_width` through the effective `max_table_width`; larger totals
are scaled down. If `n_cols * min_col_width` is wider than the page, the
per-column floor is relaxed to `effective_width / n_cols`.

For the default 9-inch landscape letter page, use values such as `"3|2|2|2"`
for a 4-column table or `"3|1.5|1.5|1.5|1.5"` for a 5-column table.

## Pagination

By default, `rows_per_page = NULL` leaves pagination to `reporter`.
`reporter::write_report()` computes page breaks from the selected output
format, fixed page metrics, and the actual wrapped title, header, row, and
footnote line counts. Supply `rows_per_page` only when you want to force manual
chunks before reporter's own pagination runs.
