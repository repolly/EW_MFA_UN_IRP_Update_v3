
wd <- getwd()
rmarkdown::render("./02_scripts/05_data_check/01_other/ccc_comparison_now_former.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./02_scripts/05_data_check/01_other/",
                  intermediates_dir = "./02_scripts/05_data_check/01_other/",
                  output_file = paste0("ccc_comparison_now_former_", substr(Sys.time(), 1, 10),".html")
                  )


wd <- getwd()
rmarkdown::render("./02_scripts/05_data_check/01_other/bgs_vs_estimates.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./02_scripts/05_data_check/01_other/",
                  intermediates_dir = "./02_scripts/05_data_check/01_other/",
                  output_file = paste0("bgs_usgs_estimates_A.3.8.2_", substr(Sys.time(), 1, 10),".html")
)


wd <- getwd()
rmarkdown::render("./02_scripts/05_data_check/check_large_changes.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./02_scripts/05_data_check/",
                  intermediates_dir = "./02_scripts/05_data_check/",
                  output_file = paste0("check_large_changes_", substr(Sys.time(), 1, 10),".html")
                  )



