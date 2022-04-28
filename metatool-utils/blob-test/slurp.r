library(iotools)
library(tibble)
library(duckdb)
library(DBI)
library(dbplyr)
library(blob)

con <- dbConnect(duckdb::duckdb(), dbdir="test.duckdb", read_only=FALSE)

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

read_rawchar_doc <- function(con, n = 10e6, file_encoding = "") {
  readAsRaw(con, n, fileEncoding = file_encoding) |> 
    raw_to_rawchar()
}

write_rawchar_doc <- function(x, con) {
  x |> 
    rawchar_to_raw() |>
    writeBin(con)
}

rc <- read_rawchar_doc("listdown-jss.pdf") |>
  write_rawchar_doc("test.pdf")

rs <- paste(pdf_contents, collapse = "")
x <- tibble(fn = fn, content = read_rawchar_doc(fn))

db_copy_to(con, 'test', x, temporary = FALSE, overwrite = TRUE)
