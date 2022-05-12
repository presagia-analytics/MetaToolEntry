source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
library(testthat)
library(shiny)
library(ggplot2)

## test data 
SourceData <- readRDS("C:/Users/Preadmin/OneDrive - Telperian/Github/MetaToolApp/raw-data/FirstDataSource.RDS")
ns <- NS("example")

test_that("test create table", {
  test_ttf <- create_ttf_table(6)
  expect_equal(dim(test_ttf),c(6, 15))

  test_cat <- create_cat_table(6)
  expect_equal(dim(test_cat),c(6, 5))

  test_num <- create_num_table(6)
  expect_equal(dim(test_num),c(6, 9))

})

dfc <- create_cat_table(3)
dfn <- create_num_table(3)
dft <- create_ttf_table(3)

dft$Treatment <- c("Docetaxel","Nivolumab","Nivolumab")
dft$Subgroup  <- c("squamous","squamous","non-squamous")
dft$Pathology <- c("2","3","4")
dft$N <- c("135", "137","250")
dft$No.Event <- c("70","50","50")
dft$Est.Median <- c("5.99","11.15", "9.14")
dft$Fup.Median<- c("24", "24", "13")
dft$KM.Data <- c("NSCLC01642004_docetaxel.csv","NSCLC01642004_nivolumab.csv","NSCLC01673867_docetaxel.csv")
dft$IPD.Data = c("","ipd_example1.csv","ipd_example2.csv")

km_input_files <- data.frame(name = c("NSCLC01642004_docetaxel.csv","NSCLC01642004_nivolumab.csv"),
                             size = c("2619, 2573"),
                             type = c("application/vnd.ms-excel","application/vnd.ms-excel"),
                             datapath = c(file.path(here::here(), "example/NSCLC01642004_docetaxel.csv"),
                                          file.path(here::here(), "example/NSCLC01642004_nivolumab.csv")))

ipd_input_files <- data.frame(name = c("ipd_example1.csv","ipd_example2.csv"),
                             size = c("2619, 2573"),
                             type = c("text/csv","text/csv"),
                             datapath = c(file.path(here::here(), "tests_data/ipd_example1.csv"),
                                          file.path(here::here(), "tests_data/ipd_example2.csv")))


risk_table <- tibble::tibble(
  Treatment = c("Time (in original unit)","Docetaxel","Nivolumab"),
  Subgroup  = c("","squamous","squamous"),
  Pathology = c("","2","3"),
  "Value (Separate numbers by blank space)" = c("0 3 6 9 12 15 18 21 24","135 113 86 69 52 31 15 7 0","137 103 68 45 30 14 7 2 0"),
  
  )

test_that("make_final_table", {
  # this function will not be used for databd
  test_c <- make_final_table(dfc,ns)
  expect_identical(colnames(test_c),c( "ID","Treatment", "Subgroup","Pathology","example-N"))

  test_n <- make_final_table(dfn,ns)
  expect_identical(colnames(test_n),c( "ID","Treatment", "Subgroup","Pathology","example-N",
                                       "example-Mean","example-Sd", "example-Median", "example-Range"))
  test_t <- make_final_table(dft,ns)
  expect_identical(colnames(test_t),c( "ID","Treatment", "Subgroup","Pathology","example-N",
                                       "example-No.Event","example-Est.Median", "example-EM.95CIL", "example-EM.95CIU",
                                       "example-Fup.Median", "example-HR", "example-HR.95CIL","example-HR.95CIU",
                                       "example-KM.Data","example-IPD.Data"))

})

test_that("adjust_row", {
  expect_equal(nrow(adjust_row(dfc,6)),6)
  expect_equal(nrow(adjust_row(dfc,1)),1)
  expect_equal(nrow(adjust_row(dfc,NA)),3)
  expect_equal(nrow(adjust_row(dfc,3)),3)
})

test_that("adjust_col", {
  test_d1 <- adjust_col(dfc,c("A;B;C"))
  expect_equal(colnames(test_d1),c( "ID","Treatment", "Subgroup", "Pathology" ,"N","A","B","C"))

  test_d2 <- adjust_col(test_d1,c("A;B;C;D"))
  expect_equal(colnames(test_d2),c( "ID","Treatment", "Subgroup", "Pathology","N","A","B","C","D"))

  test_d3 <- adjust_col(test_d2,c("A;l"))
  expect_equal(colnames(test_d3),c( "ID","Treatment", "Subgroup", "Pathology","N","A","l"))

  expect_equal(colnames(adjust_col(dfc,NA)),c( "ID","Treatment", "Subgroup", "Pathology","N"))
  expect_equal(colnames(adjust_col(dfc,"")),c( "ID","Treatment", "Subgroup", "Pathology","N"))
  expect_equal(colnames(adjust_col(dfc,"A,B,C,")),c( "ID","Treatment", "Subgroup", "Pathology","N","A","B","C"))
  expect_equal(colnames(adjust_col(dfc,"A,B;C")),c( "ID","Treatment", "Subgroup", "Pathology","N","A","B","C"))
  expect_equal(colnames(adjust_col(dfc,"A B;C")),c( "ID","Treatment", "Subgroup", "Pathology","N","A B","C"))
  expect_equal(colnames(adjust_col(dfc,"A;;B;")),c( "ID","Treatment", "Subgroup", "Pathology","N","A","B"))
})

test_that("Test KMplot from median", {
  dft_median <- make_df_median(dft)
  expect_identical(colnames(dft_median),c( "treatment","timess", "valuess"))
  expect_equal(dim(dft_median),c(1353, 3))

  p <- make_survplot(dft_median)
  expect_error(print(p), NA)

  expect_identical(use_median(NULL,dft$KM.Data, dft$Est.Median), TRUE)
  expect_identical(use_median("A path",dft$KM.Data, dft$Est.Median), FALSE)
  expect_identical(use_median("A path",c("", "","b"), dft$Est.Median), FALSE)
  expect_identical(use_median(NULL,c("", "",""), dft$Est.Median), TRUE)
  expect_identical(use_median("A path",c("", "",""), dft$Est.Median), TRUE)
  expect_identical(use_median("A path",c("", "",""), c("","","")), FALSE)
  expect_identical(use_median("A path",c("", "",""), c("5","","")), TRUE)
})

test_that("Test KMplot from km data", {
  dft$km_data <- SourceData$os.data[1:3]
  dft_km <- make_df_kmdata(dft)
  expect_identical(colnames(dft_km),c( "treatment","timess", "valuess"))
  expect_equal(dim(dft_km),c(269, 3))

  p <- make_survplot(dft_km)
  expect_error(print(p), NA)

  expect_identical(use_km_data(NULL,dft$KM.Data), FALSE)
  expect_identical(use_km_data("A path",dft$KM.Data), TRUE)
  expect_identical(use_km_data("A path",c("", "","b")), TRUE)
  expect_identical(use_km_data(NULL,c("", "","")), FALSE)
  expect_identical(use_km_data("A path",c("", "","")), FALSE)
})

test_that("from table to plot",{
  dft$km_data <- SourceData$os.data[1:3]

  p <- table2survplot(dft,km_input_files)
  expect_error(print(p), NA)

  p <- table2survplot(dft,NULL)
  expect_error(print(p), NA)

  dft$KM.Data <- ""
  p <- table2survplot(dft,km_input_files)
  expect_error(print(p), NA)

})

test_that("convert and clean km time", {
  expect_equal(km_clean_month_vector(seq(1,100)),seq(1:100))
  expect_equal(km_clean_month_vector(seq(1,3),unit_time = "year"),c(12, 24, 36))
  expect_equal(round(km_clean_month_vector(seq(4.35,12,by = 4.33),unit_time = "week"),0),c(1,2))
  expect_equal(round(km_clean_month_vector(seq(30,360,by = 30),unit_time = "day"),0),c(1:12))

  test_data1 <- data.frame(time = c(0,1:10), surv = seq(1.1,0.1,by = -0.1))
  test_1 <- km_clean(test_data1)
  expect_equal(dim(test_1 ),c(10,2))

  test_data2 <- data.frame(time = c(-1,0,0.1,1,2,3,4:7), surv = seq(1,0.1,by = -0.1))
  test_2 <- km_clean(test_data2)
  expect_equal(dim(test_2 ),c(9,2))

  expect_equal(km_clean_month(test_data1,"year")$time, c(0,seq(24,120,12)))
})

test_that("convert and clean km time", {
  test_df <- add_km(dft,km_input_files)
  expect_equal(dim(test_df),c(3,16))
  expect_identical(test_df$km_data[[3]],NULL)

  test_df <- add_km(dft,km_input_files[1,])
  expect_equal(dim(test_df),c(3,16))
  expect_identical(test_df$km_data[[2]],NULL)
  expect_identical(colnames(test_df )[16], "km_data")

  dft$km_data <- ""
  test_df <- add_km(dft,NULL)
  expect_identical(test_df$km_data[[1]],"")

  dft$KM.Data <- ""
  test_df <- add_km(dft,km_input_files[1,])
  expect_identical(test_df$km_data[[1]],NULL)
})

test_that("get median survival", {
  dft$km_data <- SourceData$os.data[1:3]
  expect_equal(get_ttf_median(dft$km_data[[1]]),"5.99")
  expect_equal(get_ttf_median(NULL),"")

  dft2 <- dft
  dft2$Est.Median<- ""
  expect_identical(update_median(dft2)$Est.Median,c("5.99", "9.15", "9.14"))
})

test_that("get risk table from image",{
  img_input <- "C:/Users/Preadmin/OneDrive - Telperian/Github/MetaToolApp/raw-data/1_os.PNG"

  test_df <- get_risktable_from_fig(img_input)
  expect_equal(dim(test_df),c(3,1))
  expect_identical(test_df[[1]][1], c("time 0 3 6 9 12 15 18 21 24"))

  test_df <- make_risk_table(img_input)
  expect_equal(dim(test_df),c(3,5))

  test_df <- make_risk_table(NULL)
  expect_equal(dim(test_df),c(1,5))
  expect_identical(test_df$`Value (Separate numbers by blank space)`, c("Can't get the risk table"))

})

# test_that("clean risk table",{
#   risk_table_test <- clean_risktable(risk_table)
#   expect_equal(dim(risk_table_test),c(3,6))
#   expect_identical(class(risk_table_test$value), c("list"))
#   expect_identical(class(risk_table_test$value[[1]]), c("numeric"))
# })

test_that("test upload ipd",{
  ipd_table <- add_ipd_upload(dft, ipd_input_files)
  expect_equal(dim(ipd_table),c(3,16))  #note, 16 is without km_data
  expect_true(is.null(ipd_table$ipd[1][[1]]))
  expect_false(is.null(ipd_table$ipd[2][[1]]))

})

test_that("calcaute ipd from risk table",{
  risk_time <- c(0 , 3 , 6  ,9, 12, 15, 18 ,21, 24)
  risk_number <- c(135, 113 , 86,  69,  52 , 31,  15 ,  7 ,  0)
  dft$km_data <- SourceData$os.data[1:3]

  ipd_test <- get_ipd_risktable(risk_time, risk_number, dft$km_data[[1]])
  expect_equal(dim(ipd_test),c(135,5))

  ipd_test2 <- get_ipd_median(0.2,7,10)
  expect_equal(dim(ipd_test2),c(10,5))

  ipd_test3 <- get_ipd_median(0.4," ",4)
  expect_equal(dim(ipd_test3),c(4,5))
  expect_false(all(is.na(get_ipd_median(0.4," ",4)$time)))
  ##  parcial risk table

  ## null risk table

  ipd_table <- add_ipd_risktable(dft, risk_table,"month")
  expect_equal(dim(ipd_table),c(3,17))
  expect_identical(class(ipd_table$ipd), c("list"))
  expect_false(all(sapply(ipd_table$ipd, is.null)))

  ipd_table2 <- add_ipd_risktable(dft, NULL,"month")
  expect_equal(dim(ipd_table2),c(3,17))
  expect_identical(class(ipd_table2$ipd), c("list"))
  expect_false(all(sapply(ipd_table2$ipd, is.null)))

  ipd_plot_table <- do.call(rbind,ipd_table$ipd)
  p <-make_ipd_figure(ipd_plot_table)
  expect_error(print(p), NA)
})

# test_that("test get ipd from upload or from risktable",{
#   # from upload 
#   
#   ipd_table <- add_ipd(dft, risk_table,ipd_input_files, "month")
#   expect_equal(dim(ipd_table),c(3,16))  #note, 16 is without km_data
#   expect_true(is.null(ipd_table$ipd[3][[1]]))
#   expect_false(is.null(ipd_table$ipd[2][[1]]))
#   
#   #from risk table
#   risk_table$Ipd.Name <- ""
#   dft$km_data <- SourceData$os.data[1:3]
# 
#   ipd_table2 <- add_ipd(dft, risk_table,ipd_input_files,"month")
#   expect_equal(dim(ipd_table2),c(3,17))
#   expect_identical(class(ipd_table2$ipd), c("list"))
#   expect_false(all(sapply(ipd_table2$ipd, is.null)))
#   
# })


test_that("merge outcome",{
  tab_df1 <- data.frame(ID = c(1,2,3),
                       Treatment = c("A","B",""),
                       Subgroup = c("C","D",""),
                       Pathology = c("C","D",""),
                       N = c(1,2,""),
                       out1  = c("oo11","oo12","")
                       )

  tab_df2 <- data.frame(ID = c(1,2,3),
                        Treatment = c("A","B","E"),
                        Subgroup = c("C","D","F"),
                        Pathology = c("C","D",""),
                        out2  = c("oo21","oo22","oo23")
  )

  test1 <- merge_outcome(NULL,tab_df1, ns)
  expect_equal(dim(test1),c(2,5))
  expect_identical(colnames(test1),c("Treatment", "Subgroup", "Pathology","example-N", "example-out1"))

  test2 <- merge_outcome(tab_df2,tab_df1, ns)
  expect_equal(dim(test2),c(3,7))
  expect_identical(colnames(test2),c("ID","Treatment", "Subgroup","Pathology","out2" ,"example-N", "example-out1"))
  expect_identical(is.na(test2[3,7]),TRUE)

  tab_df1$N = c(10,20,"")
  test3 <- merge_outcome(test2,tab_df1, ns)
  expect_identical(test3[2,6],"20")
})

test_that("treatment and subgroup dropdown",{
  tab_df1 <- data.frame(ID = c(1,2,3),
                        Treatment = c("A","B",""),
                        Subgroup = c("C","D",""),
                        out1  = c("oo11","oo12","")
  )

  tab_df2 <- data.frame(ID = c(1,2,3),
                        Treatment = c("A","B","E"),
                        Subgroup = c("C","D","F"),
                        out2  = c("oo21","oo22","oo23")
  )

  test1 <- make_trsub_list(tab_df2, tab_df1)
  expect_equal(length(test1[[1]]),4)

  test2 <- make_trsub_list(tab_df2, NULL)
  expect_equal(length(test2[[1]]),3)

  test3 <- make_trsub_list(NULL, tab_df1)
  expect_equal(length(test3[[1]]),3)

  test4 <- make_trsub_list(NULL, NULL)
  expect_equal(length(test4[[1]]),1)

})

test_that("merge_trial_info",{

  test_input <- list()
  test_input$PID = "12"
  test_input$PaperNickName = "name"
  test_input$Type = "CancerType"
  test_input$Phase = "2"
  test_input$TrLine = "2"
  test_input$NCT = "NCT1234567"
  test_input$Year = "1234"
  test_input$FirstAuthor = "Author"

  trial_outcome <- SourceData[1:3,]
  test_df <- merge_trial_info(trial_outcome, test_input)
  expect_equal(test_df$trial_info[1],"Y1234CancerTypeP2TL2Docetaxel-squamous")

})
