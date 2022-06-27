### databd and entry 

make_doce_pub <- function(input_info){
  pdf_article <- NA
  if(!is.null(input_info$pdf_input)){
    pdf_path <- input_info$pdf_input$datapath
    pdf_article <- read_rawchar_doc(pdf_path)
  }
  pub <- 
    publication(
      article = pdf_article,
      doi = "asdfasd1234",  # unknown
      year = input_info$Year,
      first_name = input_info$FirstAuthor,
      last_name = input_info$LastAuthor
    )
  pub
}

make_doce_outcome <- function(combined_df_row,input_info){

  outcome_names_all <- stringr::str_remove(colnames(combined_df_row)[grepl('\\b.N\\b',colnames(combined_df_row))], ".N")
  surv_names <- outcome_names_all[which(outcome_names_all %in% c("os","pfs"))]
  cat_names <- outcome_names_all[which(outcome_names_all %in% c("RECIST","Resp"))]
  con_names <- outcome_names_all[which(outcome_names_all %in% c("con","con2"))]  ## example
  
  doce_outcome <- list()
  doce_outcome[["doce_surv_outcome"]] <- do.call(rbind, lapply(surv_names, function(x) make_surv_outcome(x,combined_df_row,input_info)))
  doce_outcome[["doce_con_outcome"]] <- do.call(rbind, lapply(con_names , function(x) make_con_outcome(x,combined_df_row,input_info)))
  doce_outcome[["doce_cat_outcome"]] <- do.call(rbind, lapply(cat_names , function(x) make_cat_outcome(x,combined_df_row,input_info)))
  
  doce_outcome
}


# combind_same_outcome <- function(outcome_names,combined_df){
#   combine_table <- tibble()
#   col_id <- which(colnames(combined_df) %in% c("Treatment","Subgroup", "Pathology"))
# 
#   for(outcome_name in outcome_names){
#     tmp_table <- combined_df[,c(col_id,which(grepl(paste0("\\b",outcome_name,"\\b"),colnames(combined_df))))]
#     colnames(tmp_table) <- gsub(paste0(outcome_name,"-"), "", colnames(tmp_table))
#     tmp_table$outcome_names <- outcome_name
#     combine_table <- bind_rows(combine_table ,tmp_table)
#   }
# 
#   combine_table
# }

extract_outcome <- function(outcome_name,combined_df_row){
  col_id <- which(colnames(combined_df_row) %in% c("Treatment","Subgroup", "Pathology"))
  tmp_table <- combined_df_row[,c(col_id,which(grepl(paste0("\\b",outcome_name,"\\b"),colnames(combined_df_row))))]
  colnames(tmp_table) <- gsub(paste0(outcome_name,"-"), "", colnames(tmp_table))
  tmp_table$outcome_names <- outcome_name
  tmp_table
}

make_surv_outcome <- function(outcome_name,combined_df_row,input_info){
  ttf_table <- extract_outcome(outcome_name,combined_df_row)

  assert(
    nrow(ttf_table) == 1,
    combine = "and"
  )
  
  unit_time <- input_info[[paste0("os","-unit_time")]]
  
  
  km_data_list <- make_doce_surv_curve(ttf_table$km_data)
  
  surv_fig_list <- make_doce_surv_fig(ttf_table$fig_path) 
  
  ipd_list <- make_doce_ipd_data(ttf_table$ipd)
  
  tibble(
    outcome_names = ttf_table$outcome_names,           
    time_unit = unit_time,  
    treatment = ttf_table$Treatment,   
    subgroup = ttf_table$Subgroup,
    pathology = ttf_table$Pathology,
    n = ttf_table$N,
    num_events = ttf_table$No.Event,
    est_median = ttf_table$Est.Median,
    est_95_ci_upper = ttf_table$EM.95CIU,
    est_95_ci_lower = ttf_table$EM.95CIL,
    fup_median = ttf_table$Fup.Median,
    hazard_ratio = ttf_table$HR,
    hr_95_ci_upper = ttf_table$HR.95CIU,
    hr_95_ci_lower = ttf_table$HR.95CIL,
    survival_curve = list(km_data_list),
    survival_figures = list(surv_fig_list),
    survival_ipd = list(ipd_list),
    ipd_type = ttf_table$ipd_type
  ) |>       
    survival_outcome() 
  
}

make_doce_surv_curve <- function(km_dataframe){
  km_data <- km_dataframe[[1]]
  if(!is.null(km_data)){
    doce_surv <- km_data |>  
      survival_curve()
  }else{
    doce_surv <- NULL
  }
  doce_surv
}

make_doce_ipd_data <- function(ipd_dataframe){

  ipd_data <- ipd_dataframe[[1]][,c( "time" ,"status" )]
  if(!is.null(ipd_data)){
    doce_ipd <- ipd_data |>  
      ipd_data()
  }else{
    doce_ipd <- NULL
  }
  doce_ipd
}

make_doce_surv_fig <- function(fig_path){
  if(!is.na(fig_path)){
    doce_figs <-
      tibble(uploaded = read_rawchar_doc(fig_path),
             generated = read_rawchar_doc(fig_path),
             generated_ipd = read_rawchar_doc(fig_path)) |>
      survival_figures()
  }else{
    doce_figs <-NULL
  }
}

make_con_outcome <- function(outcome_name,combined_df_row,input_info){
  con_table <- extract_outcome(outcome_name,combined_df_row)
  colnames(con_table) <- tolower(colnames(con_table))
  
  assert(
    nrow(con_table) == 1,
    combine = "and"
  )
  
  tmp <- continuous_outcome(con_table)
  tmp
}

make_RECIST_outcome <- function(combined_df_row,input_info){

  cat_table <- extract_outcome("RECIST",combined_df_row)
  
  assert(
    nrow(cat_table) == 1,
    combine = "and"
  )
  
  if(cat_table$CR == ""){cat_table$CR = 0}
  if(cat_table$PR == ""){cat_table$PR = 0}
  if(cat_table$SD == ""){cat_table$SD = 0}
  if(cat_table$PD == ""){cat_table$PD = 0}
  
  responses <- c(rep("CR",cat_table$CR),rep("PR",cat_table$PR),rep("SD",cat_table$SD),rep("PD",cat_table$PD))
  
  doce_cat_outcome <- 
    tibble(
      val = factor(responses, levels = c("PD", "SD", "PR", "CR"),
                   ordered = TRUE)
    )
  
  doce_cat_outcome <- categorical_outcome(doce_cat_outcome, 
                                          outcome_names = cat_table$outcome_names,
                                          treatment = cat_table$Treatment, 
                                          subgroup = cat_table$Subgroup,
                                          pathology = cat_table$Pathology,
                                          odd_ratio = "",
                                          odd_ratio_95ci = "")
  
  doce_cat_outcome 
}

make_resp_outcome <- function(combined_df_row,input_info){
  cat_table <- extract_outcome("Resp",combined_df_row)
  assert(
    nrow(cat_table) == 1,
    combine = "and"
  )
  
  
  if(cat_table$Response == ""){cat_table$Response = 0}else{
    cat_table$Response <- as.numeric(cat_table$Response)
  }
  
  responses <- c(rep("Response",cat_table$Response),rep("No Response",(as.numeric(cat_table$N) - cat_table$Response)))
  
  doce_cat_outcome <- 
    tibble(
      val = factor(responses, levels = c("Response", "No Response"),
                   ordered = FALSE)
    )
  
  doce_cat_outcome <- categorical_outcome(doce_cat_outcome, 
                                          treatment = cat_table$Treatment,
                                          outcome_names = cat_table$outcome_names,
                                          subgroup = cat_table$Subgroup,
                                          pathology = cat_table$Pathology,
                                          odd_ratio = cat_table$OR,
                                          odd_ratio_95ci = cat_table$OR95CI)
  
  doce_cat_outcome 
}

make_cat_outcome <- function(outcome_name,combined_df_row,input_info){
  if(outcome_name == "RECIST"){
    output_df <- make_RECIST_outcome(combined_df_row,input_info)
  }
  if(outcome_name == "Resp"){
    output_df <- make_resp_outcome(combined_df_row,input_info)
  }
  output_df
}

make_trial <- function(input_info,doce_outcome_list,pub,nct_number){
  
  if(is.null(input_info$Type)){input_info$Type <- ""}
  if(is.null(input_info$TrLine)){input_info$TrLine <- ""}
  if(is.null(input_info$Phase)){input_info$Phase <- ""}
  
  s_trial <- trial(trial_id = nct_number,
                   disease = input_info$Type, 
                   line = input_info$TrLine, 
                   phase = input_info$Phase,
                   outcome = doce_outcome_list,
                   publication = pub,
                   nickname = "test trial")
  s_trial
}



