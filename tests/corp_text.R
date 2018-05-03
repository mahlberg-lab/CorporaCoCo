library(CorporaCoCo)
library(data.table)
library(unittest, quietly=TRUE)


test_for_error <- function(code, expected_regexp = '.+') {
    tryCatch({
            code
            return("No error returned")
        }, error = function(e) {
            if(grepl(expected_regexp, e$message)) return(TRUE)
            return(c(e$message, "Expected error did not match - ", expected_regexp))
        }
    )
}


# -----
# tests
# -----

ok_group("main", {
    x <- "A man, a plan, a canal -- Panama!"
    got <- corp_text(x)
    ok(is.corp_text(got), "is.corp_text")

    ok(corp_get_text(got) == x, "corp_get_text")

    tokens <- corp_get_tokens(got)
    ok(is.data.table(tokens), "corp_get_tokens returned a data.table")
    ok(all(c("idx", "type", "start", "end", "token") %in% names(tokens)) && length(names(tokens)) == 5, "corp_get_tokens - correct vars")

    metadata <- corp_get_metadata(got)
    ok(all(c("PACKAGE_VERSION", "DATE") %in% names(metadata)) && length(names(metadata)) == 2, "corp_get_metadata - correct vars") 

    lookup <- corp_type_lookup(got)
    ok(is.data.table(lookup), "corp_type_lookup returned a data.table")
    ok(nrow(lookup) == 5, "corp_type_lookup - correct number of rows")
})
