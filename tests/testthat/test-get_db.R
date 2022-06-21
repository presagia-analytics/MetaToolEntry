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






