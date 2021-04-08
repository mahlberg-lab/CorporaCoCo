library(CorporaCoCo)
library(data.table)
library(unittest, quietly = TRUE)

# Retrieve functions that are shared across multiple test files
source(file = "shared_functions.R", local = TRUE)

# -----
# tests
# -----

ok_group("corp_surface", {
    x <- "A man, a plan, a canal -- Panama!"
    y <- corp_text(x)
    got <- corp_surface(y, span = "2R")
    ok(is.corp_cooccurrence(got), "is.corp_cooccurrence")
    ok(is.corp_surface(got), "is.corp_surface")

    ok(corp_get_text(got) == x, "corp_get_text")
    ok(identical(corp_get_tokens(got), corp_get_tokens(y)), "corp_get_tokens")

    counts <- corp_get_counts(got)
    ok(is.data.table(counts), "corp_get_counts returned a data.table")

    metadata <- corp_get_metadata(got)
    ok(all(c("PACKAGE_VERSION", "DATE", "span", "nodes", "collocates") %in% names(metadata)) && length(names(metadata)) == 5, "corp_get_metadata - correct vars")
    ok(metadata$span == "2R" && is.null(metadata$nodes) && is.null(metadata$collocates), "corp_get_metadata - corect values")
})
