# data/extract.R - Multi-source Data Extraction

extract_iv_multi <- function(sources, item, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  results <- lapply(sources, function(src) {
    tryCatch(extract_iv(src, item, schema), error = function(e) NULL)
  })
  results <- results[!sapply(results, is.null)]
  
  if (length(results) == 0) {
    return(data.frame(Participant = character(0), Min = numeric(0), Max = numeric(0)))
  }
  do.call(rbind, results)
}

extract_sv_multi <- function(sources, item, standardize = NULL, schema = NULL) {
  if (is.null(schema)) schema <- default_schema()
  
  results <- lapply(sources, function(src) {
    tryCatch(extract_sv(src, item, standardize = standardize, schema = schema), error = function(e) NULL)
  })
  results <- results[!sapply(results, is.null)]
  
  if (length(results) == 0) {
    return(data.frame(Participant = character(0), Value = numeric(0)))
  }
  do.call(rbind, results)
}
