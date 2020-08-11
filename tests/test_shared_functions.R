# This file stores common functions that are shared across multiple test scripts

test_for_error <- function(code, expected_regexp = ".+") {
  tryCatch({
    code
    return("No error returned")
  }, error = function(e) {
    if (grepl(expected_regexp, e$message)) return(TRUE)
    return(c(e$message, "Expected error did not match - ", expected_regexp))
  }
  )
}
