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

ok_group("corp_surface", {
    x <- "A man, a plan, a canal -- Panama!"
    got <- corp_surface(x, span = '2R')
    ok(is.corp_cooccurrence(got), "is.corp_cooccurrence")
    ok(is.corp_surface(got), "is.corp_surface")

    ok(corp_get_text(got) == x, "corp_get_text")

    counts <- corp_get_counts(got)
    ok(is.data.table(counts), "corp_get_counts returned a data.table")

    metadata <- corp_get_metadata(got)
    ok(all(c("PACKAGE_VERSION", "DATE", "span", "nodes", "collocates") %in% names(metadata)) && length(names(metadata)) == 5, "corp_get_metadata - correct vars") 
    ok(metadata$span == "2R" && is.null(metadata$nodes) && is.null(metadata$collocates), "corp_get_metadata - corect values")

    lookup <- corp_type_lookup(got)
    ok(is.data.table(lookup), "corp_type_lookup returned a data.table")
    ok(nrow(lookup) == 5, "corp_type_lookup - correct number of rows")

    tokens <- corp_get_tokens(got)
    ok(is.data.table(tokens), "corp_get_tokens returned a data.table")
    ok(all(c("idx", "type", "start", "end", "token") %in% names(tokens)) && length(names(tokens)) == 5, "corp_get_tokens - correct vars")
})
