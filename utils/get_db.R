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
