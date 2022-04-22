### databd and entry 

make_surv_outcome <- function(ttf_table,outcome_name, unit_time,fig_path = NULL){
  #browser()
  # # Grab the survival data.
  if(!is.null(ttf_table[1,]$km_data[[1]])){
    doce_surv <- ttf_table[1,]$km_data[[1]]|>
      rename(event = surv) |>
      survival_curve()
  }else{
    doce_surv <- NULL
  }
  
  # Grab the figures
  if(!is.null(fig_path)){
    doce_figs <-
      tibble(uploaded = read_rawchar_doc(fig_path),
             generated = read_rawchar_doc(fig_path),
             generated_ipd = read_rawchar_doc(fig_path)) |>
      survival_figures()
  }else{
    doce_figs <-NULL
  }
  
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


