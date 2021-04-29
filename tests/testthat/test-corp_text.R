test_that("main", {
    x <- "A man, a plan, a canal -- Panama!"
    got <- corp_text(x)
    expect_equal(is.corp_text(got), TRUE)

    expect_equal(corp_get_text(got), x)

    tokens <- corp_get_tokens(got)
    expect_equal(is.data.table(tokens), TRUE)
    expect_equal(all(c("idx", "type", "start", "end", "token") %in% names(tokens)), TRUE)
    expect_equal(length(names(tokens)), 5)

    metadata <- corp_get_metadata(got)
    expect_equal(all(c("PACKAGE_VERSION", "DATE") %in% names(metadata)), TRUE)
    expect_equal(length(names(metadata)), 2)

    lookup <- corp_type_lookup(got)
    expect_equal(is.data.table(lookup), TRUE)
    expect_equal(nrow(lookup), 5)
})

test_that("corp_text_rbindlist", {
    aa <- "The cat sat on the mat."
    bb <- "This dog ate this cat."
    cc <- "That badger has a funny face."
    aaa <- corp_text(aa)
    bbb <- corp_text(bb)
    ccc <- corp_text(cc)
    ddd <- list(aaa, bbb, ccc)
    got <- corp_text_rbindlist(ddd)
    expect_equal(corp_get_text(got), "The cat sat on the mat. This dog ate this cat. That badger has a funny face.")
    tokens <- corp_get_tokens(got)
    # expect_equal doesn't work here, since tokens[...] is a list
    # but == coerces it to the number.
    expect_true(tokens[type == "badger", "start"] == 53)
    expect_true(tokens[type == "badger", "end"] == 58)
})
