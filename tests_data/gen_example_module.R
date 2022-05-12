source(file.path(here::here(), "utils/functions_helper.R"), encoding = 'UTF-8')
library(testthat)
library(shiny)
library(ggplot2)

## test data 
SourceData <- readRDS("C:/Users/Preadmin/OneDrive - Telperian/Github/MetaToolApp/raw-data/FirstDataSource.RDS")
ns <- NS("example")

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

#saveRDS(dft,file.path(here::here(),"/tests_data/ttf_rhansome.RDS"))



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

dft_final_ep<- dft
dft_final_ep<- dft
dft_final_ep$km_data <- NA
dft_final_ep$ipd <- NA
dft_final_ep$ipd_type <- NA
dft_final_ep$fig_path <- NA


dft_final<- dft_final_ep
dft_final$km_data <- SourceData$os.data[1:3]

save(dft, km_input_files, ipd_input_files, risk_table,dft_final_ep,dft_final,file = file.path(here::here(),"/tests_data/ttf_module_data.RData"))
