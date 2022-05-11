fig_path <- file.path(here::here(),"example/1_os.PNG")
ttf_table <- readRDS(file.path(here::here(),"/tests_data/ttf_table.RDS"))
input_info <- readRDS(file.path(here::here(),"/tests_data/input_info.RDS"))
input_info$pdf_input$datapath <- file.path(here::here(),"/example/1_Nivolumab versus Docetaxel.pdf")

combined_df <- readRDS(file.path(here::here(),"/tests_data/combined_df.RDS"))
combined_df$Pathology <- "ddd"
combined_df$`os-fig_path` <- file.path(here::here(),"example/1_os.PNG")
os_table <- combined_df[,which(grepl("os",colnames(combined_df)))]
colnames(os_table) <- gsub("os", "pfs", colnames(os_table))
combined_df <- cbind(combined_df,os_table)

combined_df$`con-N` <- c(1,1)
combined_df$`con-Mean` <- c(1,1)
combined_df$`con-Sd` <- c(1,1)
combined_df$`con-Median` <- c(1,1)
combined_df$`con-Range` <- c(1,1)

combined_df$`con2-N` <- c(2,2)
combined_df$`con2-Mean` <- c(2,2)
combined_df$`con2-Sd` <- c(2,2)
combined_df$`con2-Median` <- c(21,12)
combined_df$`con2-Range` <- c(12,12)

combined_df$`Resp-OR` <- c(1,1)
combined_df$`Resp-OR95CI` <- c(2,1)
combined_df$`Resp-N` <- c(3,13)
combined_df$`Resp-Responce` <- c(13,13)

saveRDS(combined_df,file.path(here::here(),"/tests_data/combined_df_all.RDS"))