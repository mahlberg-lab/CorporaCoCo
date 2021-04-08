test_that("corp_surface", {
    x <- "A man, a plan, a canal -- Panama!"
    y <- corp_text(x)
    got <- corp_surface(y, span = "2R")
    expect_true(is.corp_cooccurrence(got))
    expect_true(is.corp_surface(got))

    expect_equal(corp_get_text(got), x)
    expect_identical(corp_get_tokens(got), corp_get_tokens(y))

    # corp_get_counts returned a data.table
    counts <- corp_get_counts(got)
    expect_true(is.data.table(counts))

    # corp_get_metadata - correct vars
    metadata <- corp_get_metadata(got)
    expect_true(all(c("PACKAGE_VERSION", "DATE", "span", "nodes", "collocates") %in% names(metadata)))
    expect_equal(length(names(metadata)), 6)

    # corp_get_metadata - corect values
    expect_equal(metadata$span, "2R")
    expect_null(metadata$nodes)
    expect_null(metadata$collocates)
})
