source(file.path(here::here(),"utils/get_db.R"), encoding = 'UTF-8')
library(dplyr)
library(testthat)
library(DBI)

test_db <-   dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input_test.duckdb"),
  read_only = FALSE
)

test_that("get_trial_outcome_in - get trial's outcome list for each type",{
  t1 <- get_trial_outcome_in(test_db, "continuous")
  expect_equal(dim(t1),c(1, 2))
  expect_equal(t1$outcomes, "con")
  
  t2 <- get_trial_outcome_in(test_db, "categorical")
  expect_equal(dim(t2),c(2, 2))
  expect_equal(t2$outcomes, c("RECIST","RECIST"))
  
  t3 <- get_trial_outcome_in(test_db, "survival")
  expect_equal(dim(t3),c(3, 2))
  expect_equal(t3$outcomes, c("os","os","os"))
  
})


test_that("get_trial_outcome - get trial's outcomes",{
  t1 <- get_trial_outcome(test_db)
  expect_equal(dim(t1),c(5, 2))
  expect_equal(t1$outcomes, c("con", "RECIST,os","RECIST","os","os"))

})


#--------------------------------------------------------
trial_con_db <-   dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input2.duckdb"),
  read_only = FALSE
)


test_input_org <- readRDS(file.path(here::here(), "tests_data/test_input.RDS"))
show_col <- c('nct_id', 'official_title')
test_input <- test_input_org[,show_col]


test_that("get_outcome_df - get trial's outcomes",{
  t1 <- get_outcome_df(test_input,trial_con_db, "os")
  expect_equal(dim(t1),c(6, 22))
  expect_true(all(t1$outcome_names == "os"))
  expect_equal(class(t1$ipd), c("vctrs_list_of", "vctrs_vctr", "list"))
  expect_equal(class(t1$ipd[[4]]), c("tbl_df","tbl", "data.frame"))

  t2 <- get_outcome_df(test_input,trial_con_db, "RECIST")
  expect_equal(dim(t2),c(8, 16))

})


