source("functions/make_reports.R")

report("rmds/import_and_tidy.Rmd", n = 1)
report("rmds/pop_comparison.Rmd", 2)
report("rmds/missing_data.Rmd", 3)
report("rmds/import_and_tidy_dataMaid.Rmd", 4)
report("rmds/home_care_plot.Rmd", 5)
report("rmds/per_thousand_la_comparison.Rmd", 6)
