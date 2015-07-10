
# Create word-level bigrams from a single character string

bigrams <- function(text) {
  wdVec <- strsplit(text, "\\s+")[[1]]
  w <- length(wdVec)
  ifelse(w < 2, "", paste0(wdVec[1:(w - 1)], wdVec[2:w], collapse = " "))
}


# Create character-level shingles (length 2) from a character string

shingle <- function(text) {
  chVec <- strsplit(gsub(" ", "_", text), "")[[1]]
  t <- length(chVec)
  ifelse(t < 3, "", paste0(chVec[1:(t -1)], chVec[2:(t)], collapse = " "))
}


# Minimal version of textir::tfidf, to create tfidf from count matrix

tfidf <- function(x) {
  idf <- log(nrow(x)) - log(colSums(x > 0) + 1)
  x <- x / rowSums(x)
  t( t(x) * idf )
}



