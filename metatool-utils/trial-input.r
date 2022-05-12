library(whoami)
library(uuid)
library(checkmate)
library(DBI)
library(purrr)
library(dbplyr)

#source("rawchar.r")
source(file.path(here::here(),"metatool-utils/rawchar.r"))

user_name <- function() {
  username()
}

group_name <- function() {
  "everyone"
}

# A survival curve is a data.frame with a time and an survival rate column.
survival_curve <- function(x) {
  assert(
    check_data_frame(x),
    'time' %in% names(x),
    'surv' %in% names(x),
    combine = "and"
  )
  add_class <- "survival_curve"
  class(x) <- setdiff(class(x), add_class)
  class(x) <- c(add_class, class(x))
  x
}

make_outcome <- function(x, table_names, add_class) {
  # todo: Should types be checked?

  assert(
    check_data_frame(x),
    ncol(x) == length(table_names),
    all(sort(names(x)) == sort(table_names)),
    combine = "and"
  )
  class(x) <- setdiff(class(x), add_class)
  class(x) <- c(add_class, class(x))
  x
}

survival_figures <- function(x) {
  surv_fig_names <- c("uploaded", "generated", "generated_ipd")
  assert(
    check_data_frame(x),
    ncol(x) == length(surv_fig_names),
    all(sort(names(x)) == sort(surv_fig_names)),
    combine = "and"
  )
  add_class <- c("survival_figures")
  class(x) <- setdiff(class(x), add_class)
  class(x) <- c(add_class, class(x))
  x
}

survival_outcome <- function(x) {
  table_names <-colnames(x)


  add_class <- c("survival_outcome", "outcome")
  x <- make_outcome(x, table_names, add_class)

  assert(
    length(x$survival_type) == 1,
    x$survival_type %in% c("os", "pfs"),
    length(x$time_unit) == 1,
    x$time_unit %in% c('month', 'week', 'year', 'day'),
    combine = "and"
  )
  outcome_id <- UUIDgenerate()
  outcome_row_id <- UUIDgenerate()
  
  x$outcome_id <- outcome_id
  x$outcome_row_id <- outcome_row_id
  x$survival_curve[[1]]$outcome_row_id <- outcome_row_id
  x$survival_figures[[1]]$outcome_id <- outcome_id
  x
}


categorical_outcome <- function(
  x,
  outcome_names,
  treatment,
  subgroup,
  pathology,
  odd_ratio,
  odd_ratio_95ci) {

  assert(
    check_data_frame(x),
  )
  table_names <- c("n", "val")
  add_class <- c("categorical_outcome", "outcome")
  ordered <- is.ordered(x[["val"]])
  levels <- deparse(levels(x[["val"]]))
  x <- x |>
    group_by(val) |>
    summarize(n = n(), .groups = "drop") 
    
  x <- make_outcome(x, table_names, add_class)
  
  outcome_id <- UUIDgenerate()
  x$outcome_id <- outcome_id
  x$levels <- deparse(levels)
  x$ordered <- ordered
  x$outcome_names <- outcome_names
  x$treatment <- treatment
  x$subgroup <- subgroup
  x$pathology <- pathology
  x$odd_ratio <- odd_ratio
  x$odd_ratio_95ci <- odd_ratio_95ci
  x
}



# categorical_outcome2 <- function(x) {
#   assert(
#     check_data_frame(x),
#   )
#   table_names <- colnames(x)
#   add_class <- c("categorical_outcome", "outcome")
#     
#   x <- make_outcome(x, table_names, add_class)
#   
#   outcome_id <- UUIDgenerate()
#   x$outcome_id <- outcome_id
#   x
# }



# continuous_outcome <- function(x) {
# 
#   table_names <- c(
#     "outcome_name",
#     "treatment",
#     "subgroup",
#     "pathology",
#     "n",
#     "mean",
#     "sd",
#     "median",
#     "range")
# 
#   add_class <- c("continuous_outcome", "outcome")
#   x <- make_outcome(x, table_names, add_class)
# 
#   outcome_id <- UUIDgenerate()
#   x$outcome_id <- outcome_id
#   x
# }

continuous_outcome <- function(x) {
  assert(
    check_data_frame(x),
  )
  table_names <- colnames(x)
  add_class <- c("continuous_outcome", "outcome")
  x <- make_outcome(x, table_names, add_class)

  outcome_id <- UUIDgenerate()
  x$outcome_id <- outcome_id
  x
}



publication <- function(article, doi, year, first_name, last_name) {
  
  year <- as.integer(year)
  assert(
    length(article) == 1,
    length(doi) == 1,
    check_character(doi),
    length(year) == 1,
    check_integer(year),
    length(first_name) == 1,
    check_character(first_name),
    length(last_name) == 1,
    check_character(last_name),
    combine = "and"
  )

  assert(
    check_character(article),
    inherits(article, "connection"),
    combine = "or"
  )

  ret <- tibble(
    article = article,
    doi = doi,
    year = year,
    first_name = first_name,
    last_name = last_name
  )

  class(ret) <- c("publication", class(ret))
  ret
}

write_outcome <- function(x, con, verbose = FALSE, ...) {
  UseMethod("write_outcome", x)
}

write_outcome.default <- function(x, con, verbose = FALSE, ...) {
  stop(
    paste0("Don't know how to sync outcome for object of type:\n\t",
           paste(class(x), collapse = "\n\t"),
           "\n"
    )
  )
}

# Temporary database table from tibble.
tmp_tbl <- function(x, con, tbl_name = UUIDgenerate()) {
  uuid <- UUIDgenerate()
  db_copy_to(con, tbl_name, x, temporary = TRUE)
  ret <- tbl(con, tbl_name)
  ret$info <- new.env()
  ret$info$con <- con
  ret$info$name <- tbl_name
  reg.finalizer(ret$info,
    function(e)
      dbRemoveTable(
        e$con,
        e$name
      )
  )
  ret
}

tmp_tbl <- function(x, con, tbl_name = UUIDgenerate(), scope_temp = TRUE) {
  uuid <- UUIDgenerate()
  db_copy_to(con, tbl_name, as.data.frame(x), temporary = TRUE)
  ret <- tbl(con, tbl_name)
  ret$info <- new.env()
  ret$info$con <- con
  ret$info$name <- tbl_name
  if (scope_temp) {
    reg.finalizer(
      ret$info,
      function(e) dbRemoveTable(e$con, e$name)
    )
  }
  ret
}

sync_table <- 
  function(
    db_tbl, 
    add_tbl, 
    by = intersect(colnames(db_tbl), colnames(add_tbl)),
    execute_on_last = TRUE) {

  if (!inherits(db_tbl, "tbl_dbi") || !inherits(db_tbl$ops$x, "ident")) {
    stop("First parameter doesn't look like a tbl_dbi in the database")
  }

  # Get the rows that need updating.
  updated_rows <- inner_join(db_tbl, add_tbl |> select(all_of(by)), 
                             by = by, copy = TRUE)

  # Get the new rows.
  new_rows <- anti_join(tmp_tbl(add_tbl, db_tbl$src$con), 
                        db_tbl, by = by, copy = TRUE)

  # Get the unchanged rows.
  unchanged_rows <- anti_join(db_tbl, add_tbl, by = by, copy = TRUE)

  ret <- union(unchanged_rows, updated_rows ) |> union(new_rows)

  uuid <- paste(sample(letters, 50, replace = TRUE), collapse = "")

  # TODO: Fix this, 3 separate statements and it's basically a table 
  # overwrite.
  dbExecute(
    db_tbl$src$con, 
    paste("CREATE TABLE", uuid, "AS", remote_query(ret)),
    immediate = FALSE
  )
  dbExecute(
    db_tbl$src$con, 
    paste("DROP TABLE", as.character(db_tbl$ops$x)),
    immediate = FALSE
  )
  dbExecute(
    db_tbl$src$con, 
    paste("ALTER TABLE", uuid, "RENAME TO", as.character(db_tbl$ops$x)),
    immediate = execute_on_last
  )
  invisible(db_tbl)
}

write_outcome.survival_outcome <- function(x, con, verbose = FALSE, ...) {
  # Add the user and group.
  x$user_name <- user_name()
  x$group_name <- group_name()

  # Get the survival curve and sync with the database.
  surv_curve <- x$survival_curve[[1]]
  if ( !("survival_curve" %in% dbListTables(con))) {
    db_copy_to(con, "survival_curve", as_tibble(surv_curve[c(),]),
               temporary = FALSE)
  }
  db_surv_curve <- tbl(con, "survival_curve")
 
  sync_table(db_surv_curve, surv_curve, by = "outcome_row_id")

  # Get the survival figures and sync with the database.
  surv_figs <- x$survival_figures[[1]]
  
  if ( !("survival_figures" %in% dbListTables(con)) ) {
    db_copy_to(con, "survival_figures", as_tibble(surv_figs[c(),]),
               temporary = FALSE)
  }
  db_surv_figs <- tbl(con, "survival_figures")
  db_surv_figs <- sync_table(db_surv_figs, surv_figs, by = "outcome_id")

  # Remove the list columns and sync with the database.
  xs <- select(x, -survival_curve, -survival_figures)
  if ( !("survival_outcome" %in% dbListTables(con)) ) {
    db_copy_to(con, "survival_outcome", as_tibble(xs[c(),]),
               temporary = FALSE)
  }

  db_surv_out <- tbl(con, "survival_outcome")
  sync_table(db_surv_out, xs, 
             by = c("trial_id", "survival_type", "time_unit", 
                    "treatment", "subgroup","pathology"))
  x
}

write_outcome.categorical_outcome <- function(x, con, verbose = FALSE, ...) {
  x$user_name <- user_name()
  x$group_name <- group_name()
  if (!("categorical_outcome" %in% dbListTables(con)) ) {
    db_copy_to(con, "categorical_outcome", as_tibble(x[c(),]),
               temporary = FALSE)
  }
  db_cat_out <- tbl(con, "categorical_outcome")
  sync_table(db_cat_out, x, by = c("trial_id", "outcome_id"))
  x
}

write_outcome.continuous_outcome <- function(x, con, verbose = FALSE, ...) {
  x$user_name <- user_name()
  x$group_name <- group_name()
  
  if ( !("continuous_outcome") %in% dbListTables(con) ) {
    db_copy_to(con, "continuous_outcome", as_tibble(x[c(),]))  
  }
  db_con_out <- tbl(con, "continuous_outcome")
  sync_table(db_con_out, x, by = c("trial_id", "outcome_id"))
  x
}

write_publication <- function(x, con, verbose = FALSE, ...) {
  assert(inherits(x, "publication"))
  x$user_name <- user_name()
  x$group_name <- group_name()
  
  if ( !("publication" %in% dbListTables(con)) ) {
    db_copy_to(con, "publication", as_tibble(x[c(),]))
  }
  db_pub <- tbl(con, "publication")
  sync_table(db_pub, x, by = c("trial_id", "user_name", "doi"))
  x
}

trial <- function(trial_id, disease, line, phase, outcome, publication, 
                  nickname = NULL) {

  assert(
    check_character(trial_id),
    length(trial_id) == 1,
    check_character(disease),
    length(disease) == 1,
    check_character(line),
    length(line) == 1,
    check_character(phase),
    length(phase) == 1,
    combine = "and"    
  )

  if ( !("list" %in% class(outcome)) ) {
    outcome <- list(outcome)
  }
  assert(all(map_lgl(outcome, ~inherits(.x, "outcome"))))

  if ( !("list" %in% class(publication)) ) {
    publication <- list(publication)
  }
  
  assert(all(map_lgl(publication, ~inherits(.x, "publication"))))

  ret <- tibble(
    trial_id = trial_id,
    disease = disease,
    line = line,
    phase = phase,
    outcome = list(outcome),
    nickname = nickname,
    publication = list(publication)
  ) 
  class(ret) <- setdiff(class(ret), "trial")
  class(ret) <- c("trial", class(ret))
  ret
}

write_trial <- function(trial, con, verbose = FALSE, ...) {
  assert(inherits(trial, "trial"))

  trial$user_name <- user_name()
  trial$group_name <- group_name()
  
  assign_trial_id <- function(x, id) {
    x$trial_id <- id
    x
  }

  trial$outcome[[1]] <- 
    map(trial$outcome[[1]], partial(assign_trial_id, id = trial$trial_id))

  trial$publication[[1]] <-  
    map(trial$publication[[1]], partial(assign_trial_id, id = trial$trial_id))

  walk(trial$outcome[[1]], partial(write_outcome, con = con))
  walk(trial$publication[[1]], partial(write_publication, con = con))

  xs <- select(trial, -outcome, -publication)
  if ( !("trial" %in% dbListTables(con))) {
    db_copy_to(con, "trial", as_tibble(xs[c(),]), temporary = FALSE)
  }
  db_trial <- tbl(con, "trial")
  sync_table(db_trial, xs, 
             by = c("trial_id", "user_name", "disease", "nickname"))
  trial
}


sync_trial <- function(trial) {
}

list_trials <- function(id = NULL) {
}

get_trials_by_nct_id <- function(nct_ids) {
}

get_trials_by_set_name <- function(set_name) {
}

write_trial_set <- function() {
}
