
source(file.path(here::here(),"metatool-utils/get_db.R"), encoding = 'UTF-8')
library(dplyr)
library(testthat)


test_db <-   dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input_test_readonly.duckdb"),
  read_only = FALSE
)

con = dbGetQuery(test_db, "SELECT * FROM continuous_outcome")
cat = dbGetQuery(test_db, "SELECT * FROM categorical_outcome")
surv= dbGetQuery(test_db, "SELECT trial_id, survival_type FROM survival_outcome")
surv_uni <- surv[!duplicated(surv),]


dbGetQuery(test_db, "SELECT trial_id, survival_type FROM survival_outcome") %>% 
  distinct() %>%
  group_by(trial_id) %>% 
  mutate(outcomes = paste0(survival_type, collapse = ",")) %>%
  select(c("trial_id", "outcomes")) %>%
  distinct()


