source(file.path(here::here(), "utils/entry_db.R"), encoding = 'UTF-8')
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source(file.path(here::here(),"metatool-utils/trial-input.r"), encoding = 'UTF-8')
source(file.path(here::here(),"metatool-utils/rawchar.r"), encoding = 'UTF-8')
library(dplyr)

combined_df <- readRDS(file.path(here::here(),"/tests_data/combined_df_all.RDS"))
combined_df_inc <- readRDS(file.path(here::here(),"/tests_data/combined_df_incomplete.RDS"))
input_info <- readRDS(file.path(here::here(),"/tests_data/input_info.RDS"))
input_info$pdf_input$datapath <- file.path(here::here(),"/example/1_Nivolumab versus Docetaxel.pdf")
# saveRDS(combined_df, file = file.path(here::here(),"/tests_data/combined_df_all.RDS"))

test_db <-   dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input_test.duckdb"),
  read_only = FALSE
)

test_that("make_doce_pub",{
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
  
  input_info$pdf_input <- NULL
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
})

test_that("extract_outcome - get columns of the outcome ",{
  tmp <- extract_outcome("os",combined_df[1,])
  expect_equal(dim(tmp),c(1, 18))
  
  tmp <- extract_outcome("con",combined_df[1,])
  expect_equal(dim(tmp),c(1, 9))

})


test_that("make_doce_surv_curve - take km from excel add to survival outcome object",{
  t1 <- make_doce_surv_curve(combined_df[1,]$`os-km_data`)
  
  expect_equal(class(t1),c("survival_curve", "data.frame"))
  expect_equal(dim(t1), c(106,2))
  
  expect_error(make_doce_surv_curve(combined_df[1,]$`os-km_data`[[1]]))
  expect_error(make_doce_surv_curve(NA))
  
  t2 <- make_doce_surv_curve(vector(mode = "list", length = 1))
  expect_equal(class(t2), "NULL")
  expect_equal(t2, NULL)
  
  t3 <- make_doce_surv_curve(combined_df_inc[1,]$`os-km_data`)
  expect_identical(t2, t3)
  
})


test_that("make_doce_ipd_data - tke ipd data and add to ipd_data object",{
  t1 <- make_doce_ipd_data(combined_df[1,]$`os-ipd`)
  
  expect_equal(class(t1),c("ipd_data", "data.frame"))
  expect_equal(dim(t1), c(45,5))
  
  expect_error(make_doce_ipd_data(combined_df[1,]$`os-ipd`[[1]]))
  expect_error(make_doce_ipd_data(NA))
  
  t2 <- make_doce_ipd_data(vector(mode = "list", length = 1))
  expect_equal(class(t2), "NULL")
  expect_equal(t2, NULL)
  
  t3 <- make_doce_ipd_data(combined_df_inc[1,]$`os-ipd`)
  expect_identical(t2, t3)
  
})



test_that("make_doce_surv_fig - test save survival fig to database",{
  
  t1 <- make_doce_surv_fig(combined_df[1,]$`os-fig_path`)
  expect_equal(class(t1),c("survival_figures", "tbl_df", "tbl" , "data.frame"))
  
  t2 <- make_doce_surv_fig(combined_df_inc[1,]$`os-fig_path`)
  expect_equal(class(t2), "NULL")
  expect_equal(t2, NULL)
  
})



test_that("test survival outcome",{
  tmp <- make_surv_outcome("os",combined_df[1,],input_info)
   
  expect_true("survival_outcome" %in% class(tmp))
  expect_equal(dim(tmp), c(1,20))
  expect_equal(class(tmp$survival_curve),"list")
  expect_equal(class(tmp$survival_curve[[1]]),c("survival_curve", "data.frame"))
  expect_equal(class(tmp$survival_figures),"list")
  expect_equal(class(tmp$survival_figures[[1]]),c("survival_figures", "tbl_df", "tbl" , "data.frame"))
  
  t2 <- make_surv_outcome("os",combined_df_inc[1,],input_info)
  expect_equal(dim(t2),dim(tmp))
  expect_equal(class(t2$survival_curve),"list")
  expect_equal(class(t2$survival_curve[[1]]),"NULL")
  expect_equal(class(t2$survival_figures),"list")
  expect_equal(class(t2$survival_figures[[1]]),"NULL")
})

test_that("test continous outcome",{
  tmp <- make_con_outcome("con",combined_df[1,],input_info)
  expect_true("continuous_outcome" %in% class(tmp))
})


test_that("test RECIST",{
  RECIST_tmp <- make_RECIST_outcome(combined_df[1,],input_info)
  expect_true("categorical_outcome" %in% class(RECIST_tmp))
  expect_equal(dim(RECIST_tmp),c(4, 11))
})

test_that("test resp",{
  resp_tmp <- make_resp_outcome(combined_df[1,],input_info)
  expect_true("categorical_outcome" %in% class(resp_tmp))
  expect_equal(dim(resp_tmp),c(2, 11))
})

test_that("test make_dose_outcome",{
  all_tmp <- make_doce_outcome(combined_df[1,],input_info)
  expect_equal(length(all_tmp),3)
})

test_that("make_trial - take all of the info and make a single trial object for duckdb", {
  outcome_list <- make_doce_outcome(combined_df[1,],input_info)
  doce_pub <- make_doce_pub(input_info)
  
  single_trial <- make_trial(input_info,outcome_list,doce_pub,"nct111")
  
  expect_equal(typeof(single_trial),"list")
  expect_equal(length(single_trial),7)
  
})
# 

test_that("write_trial - write tiral in to duckdb - perfect cases 1",{

  ## 
  outcome_list <- make_doce_outcome(combined_df[1,],input_info)
  doce_pub <- make_doce_pub(input_info)
  
  # test if single outcome - con works 
  outcome_list_con <- outcome_list[[2]]
  single_trial_con <- make_trial(input_info,outcome_list_con,doce_pub,"nct112")
  expect_silent(write_trial(single_trial_con, test_db))
  
  # test if single outcome - cat works 
  outcome_list_cat <- outcome_list[[3]]
  single_trial_cat <- make_trial(input_info,outcome_list_cat,doce_pub,"nct113")
  expect_silent(write_trial(single_trial_cat, test_db))
})

test_that("write_trial - write tiral in to duckdb - perfect cases 2",{

  ## survival outcome
  outcome_list <- make_doce_outcome(combined_df[1,],input_info)
  doce_pub <- make_doce_pub(input_info)

  outcome_list_surv <- outcome_list[[1]]
  single_trial_surv <- make_trial(input_info,outcome_list_surv,doce_pub,"nct111")
  expect_silent(write_trial(single_trial_surv, test_db))
})



# test_that("write_trial - write tiral in to duckdb - perfect cases 3",{
#   
#   ## all outcome
#   outcome_list <- make_doce_outcome(combined_df[1,],input_info)
#   doce_pub <- make_doce_pub(input_info)
#   
#   single_trial <- make_trial(input_info,outcome_list,doce_pub,"nct111")
#   write_trial(single_trial, trial_con_db)
# })





# 
# test_that("write_trial - write tiral in to duckdb (abnormal cases)",{
#   
#   ## perfect cases
#   
#   outcome_list <- make_doce_outcome(combined_df_inc[1,],input_info)
#   doce_pub <- make_doce_pub(input_info)
#   
#   single_trial <- make_trial(input_info,outcome_list,doce_pub,"nct111")
#   write_trial(single_trial, trial_con_db)
#   
# })



