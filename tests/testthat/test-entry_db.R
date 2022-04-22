source(file.path(here::here(), "utils/entry_db.R"), encoding = 'UTF-8')
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/trial-input.r")
source("C:/Users/Preadmin/OneDrive - Telperian/Github/ctrialsgovshiny/metatool-utils/rawchar.r")
library(dplyr)

fig_path <- file.path(here::here(),"example/1_os.PNG")
ttf_table <- readRDS(file.path(here::here(),"/tests_data/ttf_table.RDS"))

test_that("test create table", {
  doce_surv_outcome <- make_surv_outcome(ttf_table[1,],"os","month",fig_path)
  expect_equal(dim(doce_surv_outcome),c(1, 16))
  
  expect_identical(c("survival_outcome") %in% class(doce_surv_outcome), TRUE )

})