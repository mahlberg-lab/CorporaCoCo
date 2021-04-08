test_that("corp_concordance", {
    x <- paste("`But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way,",
               "`Do cats eat bats? Do cats eat bats?' and sometimes, `Do bats eat cats?'",
               "for, you see, as she couldn't answer either question, it didn't much matter which way she put it.",
               sep = " ")

    # this is silly but it tests if we can concordance every instance of every type
    text <- corp_text(x)
    got <- corp_concordance(text, span = "2L3R")
    # with no filtering there is a concordance for every token
    expect_equal(nrow(got), 59)

    # node filtering
    got <- corp_concordance(text, span = "2L3R", nodes = "bats")
    expect_equal(nrow(got), 4)

    # collocate filtering
    got <- corp_concordance(text, span = "2L3R", collocates = "bats")
    expect_equal(nrow(got), 16)

    # corp_concordance.corp_surface
    counts <- corp_surface(text, span = "2L3R")
    expect_identical(corp_concordance(counts), corp_concordance(text, span = "2L3R"))
})
