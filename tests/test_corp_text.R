library(CorporaCoCo)
library(data.table)
library(unittest, quietly = TRUE)

# Retrieve functions that are shared across multiple test files
source("tests/test_shared_functions.R")

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

ok_group("corp_text_rbindlist", {
    aa <- "The cat sat on the mat."
    bb <- "This dog ate this cat."
    cc <- "That badger has a funny face."
    aaa <- corp_text(aa)
    bbb <- corp_text(bb)
    ccc <- corp_text(cc)
    ddd <- list(aaa, bbb, ccc)
    got <- corp_text_rbindlist(ddd)
    ok(corp_get_text(got) == "The cat sat on the mat. This dog ate this cat. That badger has a funny face.", "text")
    tokens <- corp_get_tokens(got)
    ok(tokens[type == "badger", "start"] == 53 && tokens[type == "badger", "end"] == 58, "tokens")
})
