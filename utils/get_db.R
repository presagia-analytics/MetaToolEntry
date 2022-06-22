## get duckdb info for apps 
library(dplyr)

get_trial_outcome_in <- function(trial_con_db, outcome_type){
  sql_rule <- paste0("SELECT trial_id, outcome_names FROM ",outcome_type,"_outcome")
  
  t_out <- dbGetQuery(trial_con_db, sql_rule ) %>% 
    distinct() %>%
    group_by(trial_id) %>% 
    mutate(outcomes = paste0(outcome_names, collapse = ",")) %>%
    select(c("trial_id", "outcomes")) %>%
    distinct()
  
  t_out
}


get_trial_outcome <- function(trial_con_db){
  out_db <- rbind(
    get_trial_outcome_in(trial_con_db, "continuous"),
    get_trial_outcome_in(trial_con_db, "categorical"),
    get_trial_outcome_in(trial_con_db, "survival")) %>%
    distinct() %>%
    group_by(trial_id) %>% 
    mutate(outcomes_all = paste0(outcomes, collapse = ",")) %>%
    select(c("trial_id", "outcomes_all")) %>%
    distinct()
  
  out_db$trial_id <- trimws(out_db$trial_id)
  colnames(out_db) <- c("nct_id","outcomes")
  out_db
}


get_outcome_df <- function(filter_trail, trial_con_db, outcome_name){
  
  if (outcome_name %in% c("os", "pfs")){
    outcome_type <- "survival"
  } else if (outcome_name %in% c("RECIST","Resp")){
    outcome_type <- "categorical"
  } else if (outcome_name %in% c("con","con2")){
    outcome_type <- "categorical"
  } else { stop()}
  
  IN <- filter_trail$nct_id
  sql_rule <- paste0("SELECT * FROM ",outcome_type,"_outcome WHERE outcome_names = '",outcome_name,"'AND trial_id IN ", paste0("('", paste(target_trail, collapse = "','"),"')"))
  out_df <-dbGetQuery(trial_con_db, sql_rule)
  out_df
}


## TODO, survival add ipd. 
## merge trial infomation to outcome database. 
