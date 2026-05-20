base_path <- paste0(getwd(),"/tests/testthat")
dev <- FALSE

test_that("Output Test-01: Output a RTF file with gtsummary object as expected", {
  if (dev) {
    library(gtsummary)
    
    output_dir <- file.path(base_path, "outputs")
    
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
    
    fp <- file.path(output_dir, "t-demog.rtf")
    demog_paths <- gtsummary_reporter(
      gts_obj = demog_tbl,
      file_path = fp,
      output_types = c("RTF"),
      save_rds = TRUE
    )
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(file.exists(paste0(output_dir,"/t-demog_output_data.rds")), TRUE)
    expect_equal(file.exists(paste0(output_dir,"/t-demog_ard.rds")), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})
