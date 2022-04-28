library(tibble)
library(duckdb)
library(DBI)
library(blob)

con <- dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only=FALSE)

x <- tibble(name = "mtcars", dataset = blob(serialize(mtcars, NULL)))

dbWriteTable(con, "datasets", x)


library(tibble)
library(duckdb)
library(DBI)
library(blob)
library(dplyr)

raw_to_rawchar <- function(raw_vec) {
  paste(raw_vec, collapse = "")
}

rawchar_to_raw <- function(rawchar_vec) {
  ind <- matrix(seq_len(nchar(rawchar_vec)), ncol = 2, byrow = TRUE)
  ret <- vapply(
    seq_len(nrow(ind)),
    function(i) substr(rawchar_vec, ind[i,1], ind[i,2]),
    NA_character_
  )
  as.raw(as.hexmode(ret))
}


con <- dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only=FALSE)

x <- tibble(name = "mtcars", dataset = raw_to_rawchar(serialize(mtcars, NULL)))

dbWriteTable(con, "datasets", x, overwrite = TRUE)

datasets <- tbl(con, "datasets") |> as_tibble()

datasets$dataset[[1]] |>
  rawchar_to_raw() |>
  unserialize() |>
  head() 

library(arrow)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only=FALSE)
df <- data.frame(b=I(list(charToRaw('asdf'))))
at <- arrow::Table$create(df)
duckdb::duckdb_register_arrow(con, "at", at)

df <- data.frame(b=I(list(3)))
at <- arrow::Table$create(df)
duckdb::duckdb_register_arrow(con, "at", at)


