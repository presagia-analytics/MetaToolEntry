source(file.path(here::here(), "utils/entry_db.R"), encoding = 'UTF-8')
source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
source(file.path(here::here(),"metatool-utils/trial-input.r"), encoding = 'UTF-8')
source(file.path(here::here(),"metatool-utils/rawchar.r"), encoding = 'UTF-8')
library(dplyr)

combined_df <- readRDS(file.path(here::here(),"/tests_data/combined_df_all.RDS"))
input_info <- readRDS(file.path(here::here(),"/tests_data/input_info.RDS"))
input_info$pdf_input$datapath <- file.path(here::here(),"/example/1_Nivolumab versus Docetaxel.pdf")


test_that("make_doce_pub",{
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
  
  input_info$pdf_input <- NULL
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
})

test_that("combine outcomes of the same times",{
  tmp <- extract_outcome("os",combined_df[1,])
  expect_equal(dim(tmp),c(1, 16))
  
  tmp <- extract_outcome("con",combined_df[1,])
  expect_equal(dim(tmp),c(1, 9))

})


test_that("test survival outcome",{
  tmp <- make_surv_outcome("os",combined_df[1,],input_info)
  expect_true("survival_outcome" %in% class(tmp))
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

# outcome_list <- make_doce_outcome(combined_df[1,],input_info)
# make_trial(input_info,outcome_list,doce_pub,"nct111")
# 
# 
# test_that("make_surv_outcome", {
#   doce_surv_outcome <- make_surv_outcome(ttf_table[1,],"os","month",fig_path)
#   expect_equal(dim(doce_surv_outcome),c(1, 16))
#   
#   expect_identical(c("survival_outcome") %in% class(doce_surv_outcome), TRUE )
#   
# })

