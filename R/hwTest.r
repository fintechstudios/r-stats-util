hwExpected <-
  function(timeSeries,
           seasonality = "mult",
           frequency) {
    tsData <- ts(timeSeries, frequency = frequency)
    hw <-
      HoltWinters(x = tsData,
                  seasonal = seasonality)
    hw$fitted[, 1]
  }

hwPredict <-
  function(timeSeries,
           seasonality = "mult",
           frequency,
           predictions) {
    tsData <- ts(timeSeries, frequency = frequency)
    hw <-
      HoltWinters(x = tsData,
                  seasonal = seasonality)
    predict(hw, n.ahead = predictions)
  }

compareSent <- function(a, b) {
  stringdist(a, b) / mean(nchar(a), nchar(b))
}

removeLong <- function(sents, thresh) {
  x <- mean(nchar(sents))
  sd <- sd(nchar(sents))
  sents[which(nchar(sents) < x + thresh * sd)]
}

removeSimilar <- function(sents) {
  remove = c()
  for (i in 1:(length(sents) - 1)) {
    comps = c()
    for (j in (i + 1):length(sents)) {
      comps <- c(comps, compareSent(sents[i], sents[j]))
    }
    if (min(comps) < 0.5) {
      remove <- c(remove, (which(comps < 0.5) + i))
    }
  }
  keep <- setdiff(1:length(sents), remove)
  sents[keep]
}

cleanDoc <- function(data) {
  data <- unique(data)
  data <-
    sapply(data, function(x)
      iconv(x, "UTF-8", "ASCII", sub = ""))
  data <- sapply(data, trimws)
  data <- sapply(data, function(x)
    gsub("\003", "", x))
  data <- sapply(data, function(x)
    gsub("\\s+", " ", x))
  data <- sapply(data, function(x)
    trimws(x))
  data <- sapply(data, function(x)
    gsub('\"', '"', x))
  data <- sapply(data, function(x)
    gsub("\r\n", ". ", x))
  data <- sapply(data, function(x)
    gsub("\r", ". ", x))
  data <- sapply(data, function(x)
    gsub("\n", ". ", x))
  data <- sapply(data, function(x)
    gsub("\t", " ", x))
  data <-
    sapply(data, function(x)
      stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", " "))
  data <- sapply(data, function(x)
    gsub(" +", " ", x))
  data <- unique(data)
  data
}

summarize <- function(docs, n, sample) {
  docs <- sample(docs, length(docs) * sample, replace = FALSE)
  
  docs <- unique(docs)
  docs <- data[which(nchar(docs) > 1)]
  docs <- cleanDoc(docs)
  docs <- unique(docs)
  
  doc_id = 1:length(docs)
  df <- data.frame(docs, doc_id, stringsAsFactors = FALSE)
  
  lex <- unnest_sentences(df, sents, data)
  lex$sents <- cleanDoc(lex$sents)
  
  ranked <- lexRank(data, n = 100)
  
  topSents <- ranked$sentence[which(!duplicated(ranked$sentence))]
  topSents <- removeLong(topSents, 0.75)
  topSents <- removeSimilar(topSents)
  topSents <- head(topSents, n)
  
  topSents
}
