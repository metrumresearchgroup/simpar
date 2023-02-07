#### This script is written for mrgvalidate >= 2.0.0
#### It may be removed or modified as new releases of
#### mrgvalprep incorporate parts of this and validation-helpers.R

library(here)
devtools::load_all(here())

# set up directories and clear existing output dirs, if they exist
val_dir <- here("inst", "validation")
print(val_dir)
source(file.path(val_dir, "validation-helpers.R"))

test_dir <- file.path(val_dir, "test_results")
if (fs::dir_exists(test_dir)) fs::dir_delete(test_dir)
fs::dir_create(test_dir)

# get package info
args <- extract_r_package_info(here())
args["style_dir"] <- "~/Documents/docx-ref-header-image/" # need to put ref .docx files in here
args["auto_test_dir"] <- test_dir

# run tests and write res to disk
test_res <- mrgvalprep::parse_testthat_list_reporter(
  testthat::test_dir(here("tests", "testthat"), Reporter = testthat::ListReporter, stop_on_failure = FALSE),
  roll_up_ids = TRUE
)

write.csv(
  test_res,
  file.path(test_dir, paste0(args$product_name, "-tests.csv"))
)

# capture commit hash and other system info
git_hash <- system("git rev-parse HEAD", intern=TRUE)
Sys.setenv("COMMIT_HASH" = git_hash)

mrgvalprep::get_sys_info(
  out_path = file.path(test_dir, paste0(args$product_name, "-tests.json")),
  env_vars = c("METWORX_VERSION", "COMMIT_HASH")
)

# create docs
docs_dir <- file.path(val_dir, paste0(args$product_name, "-", args$version, "-validation-docs"))
if (fs::dir_exists(docs_dir)) fs::dir_delete(docs_dir)
fs::dir_create(docs_dir)

args["output_dir"] <- docs_dir


do.call(
  mrgvalidate::create_package_docs,
  args
)

