test_db <-   dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input_test_readonly.duckdb"),
  read_only = FALSE
)

test_that("continuous outcome ",{
  con = dbGetQuery(test_db, "SELECT * FROM continuous_outcome")
  expect_false(sum(grepl("Treatment",colnames(con)) ) > 0)
})
