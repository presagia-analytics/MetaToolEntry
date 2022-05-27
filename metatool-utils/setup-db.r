library(DBI)
library(duckdb)

con <- dbConnect(
  duckdb::duckdb(),
  dbdir = file.path(here::here(), "ctgov-snaps/trial-input3.duckdb"),
  read_only=FALSE
)

setup_db <- readLines(file.path(here::here(),'metatool-utils/setup-db.sql'))

sudi <- which(setup_db == "")

start <- 1

for (end in sudi) {
  query <- trimws(paste(setup_db[start:(end-1)], collapse = " "))
  if (query != "") {
    tryCatch(
      dbExecute(con, query),
      error = function(e) {
        print(e)
      }
    )
  }
  start <- end + 1
}

