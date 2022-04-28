library(iotools)

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
