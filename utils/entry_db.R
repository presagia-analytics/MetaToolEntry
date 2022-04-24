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
  doce_outcome <- list()
  # for each of type of outcome;
  
  #input_info$`os-unit_time`
  
  for (outcome_i in outcome_names_all){
    df_name <- paste0("doce_",outcome_i,"_outcome")
    
    if(outcome_i %in% c("os","pfs")){
      doce_outcome[[df_name]] <- make_surv_outcome(outcome_i,combined_df_row,input_info)
    }
    
    # if(outcome_i %in% c("RECIST")){
    #   doce_outcome[[df_name]] <- make_RECIST_outcome(combined_df_row,input_info)  # column is not fixed. 
    # }
    # 
    # if(outcome_i %in% c("con")){
    #   doce_outcome[[df_name]] <- make_con_outcome(outcome_i,combined_df_row,input_info)
    # }
    
  }
  doce_outcome
}


make_surv_outcome <- function(outcome_name,combined_df_row,input_info){
  ttf_table <- combined_df_row[,c(1,2,which(grepl("os",colnames(combined_df_row))))]
  colnames(ttf_table) <- c(colnames(create_ttf_table(1))[-1],"km_data","fig_path")
  
  #browser()
  # # Grab the survival data.
  km_data <- ttf_table$km_data[[1]]
  
  if(!is.null(km_data )){
    doce_surv <- km_data |>
      rename(event = surv) |>
      survival_curve()
  }else{
    doce_surv <- NULL
  }
  
  # Grab the figures
  fig_path <- ttf_table$fig_path
  if(!is.null(fig_path)){
    doce_figs <-
      tibble(uploaded = read_rawchar_doc(fig_path),
             generated = read_rawchar_doc(fig_path),
             generated_ipd = read_rawchar_doc(fig_path)) |>
      survival_figures()
  }else{
    doce_figs <-NULL
  }
  
  unit_time <- input_info[[paste0("os","-unit_time")]]
  
  tibble(
    survival_type = outcome_name,           
    time_unit = unit_time,  
    treatment = ttf_table$Treatment,   
    subgroup = ttf_table$Subgroup,    
    n = ttf_table$N,
    num_events = ttf_table$No.Event,
    est_median = ttf_table$Est.Median,
    est_95_ci_upper = ttf_table$EM.95CIU,
    est_95_ci_lower = ttf_table$EM.95CIL,
    hazard_ratio = ttf_table$HR,
    hr_95_ci_upper = ttf_table$HR.95CIU,
    hr_95_ci_lower = ttf_table$HR.95CIL,
    survival_curve = list(doce_surv),
    survival_figures = list(doce_figs)
  ) |>       
    survival_outcome() 
  
}


make_trial <- function(input_info,doce_outcome_list,pub,nct_number){
  s_trial <- trial(nct_number,
                   disease = input_info$Type, 
                   line = input_info$TrLine, 
                   outcome = doce_outcome_list,
                   publication = pub,
                   nickname = "test trial")
  s_trial
}



