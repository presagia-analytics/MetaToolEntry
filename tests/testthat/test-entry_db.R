source(file.path(here::here(), "utils/entry_db.R"), encoding = 'UTF-8')
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/trial-input.r")
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/rawchar.r")
library(dplyr)

fig_path <- file.path(here::here(),"example/1_os.PNG")
ttf_table <- readRDS(file.path(here::here(),"/tests_data/ttf_table.RDS"))
input_info <- readRDS(file.path(here::here(),"/tests_data/input_info.RDS"))
input_info$pdf_input$datapath <- file.path(here::here(),"/example/1_Nivolumab versus Docetaxel.pdf")

test_that("make_surv_outcome", {
  doce_surv_outcome <- make_surv_outcome(ttf_table[1,],"os","month",fig_path)
  expect_equal(dim(doce_surv_outcome),c(1, 16))
  
  expect_identical(c("survival_outcome") %in% class(doce_surv_outcome), TRUE )

})

test_that("make_doce_pub",{
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
  
  input_info$pdf_input <- NULL
  doce_pub <- make_doce_pub(input_info)
  expect_equal(dim(doce_pub),c(1, 5))
})



