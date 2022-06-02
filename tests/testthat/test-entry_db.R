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


test_that("make_doce_surv_curve - take km from excel add to survival outcome object",{
  t1 <- make_doce_surv_curve(combined_df[1,]$`os-km_data`)

  expect_equal(class(t1),c("survival_curve", "data.frame"))
  expect_equal(nrow(t1), 106)
  expect_equal(ncol(t1), 2)
  
  expect_error(make_doce_surv_curve(combined_df[1,]$`os-km_data`[[1]]))
  expect_error(make_doce_surv_curve(NA))
  
  t2 <- make_doce_surv_curve(vector(mode = "list", length = 1))
  expect_equal(class(t2), "NULL")
  expect_equal(t2, NULL)
  
})


test_that("test survival outcome",{
  tmp <- make_surv_outcome("os",combined_df[1,],input_info)
  expect_true("survival_outcome" %in% class(tmp))
  expect_equal(nrow(tmp), 1)
  expect_equal(ncol(tmp), 18)
  expect_equal(class(tmp$survival_curve),"list")
  
  

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
  
  ## when each of the outcome is not excist
  
  
  # test special case of input_info
  
  
  
  # test special case of outcome_list 
  
  
  # test every NULL case of the input. 
  
})

