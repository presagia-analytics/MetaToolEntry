library(readr)
library(dplyr)

source(file.path(here::here(),"metatool-utils/trial-input.r"))

# Functions for creating data base connections. 
trial_con <- function() {
  dbConnect(
    duckdb::duckdb(),
    dbdir = file.path(here::here(), "ctgov-snaps/trial-input.duckdb"),
    read_only = FALSE
  )
}

ctgov_con <- function() {
  dbConnect(
    duckdb(),
    dbdir = file.path(here::here(), "ctgov-snaps/ctgov.duckdb"),
    read_only = TRUE
  )
} 

# A trial is composed of a trial_id (nct_id), disease, line, outcome,
# publication, nickname.

# To build a trial, let's start by creating an outcome using the 
# nivolumab docetaxel data. 

# Grab the survival data.
doce_surv <- read_csv(file.path(here::here(), "example/NSCLC01642004_docetaxel.csv")) |>
  survival_curve()

# Grab the figures
doce_figs <- 
  tibble(uploaded = read_rawchar_doc("nivo-doce-example/1_os.PNG"),
         generated = read_rawchar_doc("nivo-doce-example/1_os.PNG"),
         generated_ipd = read_rawchar_doc("nivo-doce-example/1_os.PNG")) |> 
  survival_figures()

# Create a survival outcome and write it to the database.
doce_surv_outcome <- 
  tibble(
    survival_type = "os",
    time_unit = "month",
    treatment = "docetaxel",
    subgroup = "ITT",
    pathology = "pathology",
    n = 100,
    num_events = 55,
    est_median = 20,
    est_95_ci_upper = 35,
    est_95_ci_lower = 25,
    fup_median = 20,
    hazard_ratio = 1.1,
    hr_95_ci_upper = 1.34,
    hr_95_ci_lower = .99,
    survival_curve = list(doce_surv),
    survival_figures = list(doce_surv),
    survival_ipd = list(doce_surv)
  ) |> 
  survival_outcome() 
#  write_outcome(trial_con())

# Create a categorical outcome.
doce_cat_outcome <- 
  tibble(
    val = factor(c("CR", "PR", "PR", "PD"), levels = c("PD", "SD", "PR", "CR"),
                 ordered = TRUE)
  )|> 
  categorical_outcome2() 

# Write the categorical outcome to the db.
#write_outcome(doce_cat_outcome, trial_con())

# Create a continuous outcome.
doce_con_outcome <- tibble(val = iris$Sepal.Length)


doce_con_outcom <- 
  tibble(
    outcome_name = "XXX",
    treatment = "docetaxel",
    subgroup = "ITT",
    pathology = "pathology",
    n = 100,
    mean = 55,
    sd = 20,
    median = 35,
    range = 25
  ) |> 
  continuous_outcome() 

#write_outcome(doce_con_outcome)

# Create a publication
pub <- 
  publication(
    article = 
      read_rawchar_doc(file.path(here::here(), "example/1_Nivolumab versus Docetaxel.pdf")),
    doi = "asdfasd1234",
    year = 2022,
    first_name = "some",
    last_name = "dude"
  )

test_trial <- trial("NCT1234", disease = "None", line = "First", phase = "3",
                    outcome = list(doce_surv_outcome = doce_surv_outcome, 
                                   doce_cat_outcome = doce_cat_outcome,
                                   doce_con_outcome = doce_con_outcome),
                    publication = pub,
                    nickname = "test trial")

write_trial(test_trial, trial_con())
