library(CorporaCoCo)
library(data.table)
library(unittest, quietly = TRUE)

# Retrieve functions that are shared across multiple test files
source("tests/test_shared_functions.R")

# -----
# tests
# -----

ok_group("corp_concordance", {
    x <- paste("`But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way, ",
               "`Do cats eat bats? Do cats eat bats?' and sometimes, `Do bats eat cats?' ",
               "for, you see, as she couldn't answer either question, it didn't much matter which way she put it.")

    # this is silly but it tests if we can concordance every instance of every type
    text <- corp_text(x)
    got <- corp_concordance(text, span = "2L3R")
    ok(nrow(got) == 59, "with no filtering there is a concordance for every token")
    got <- corp_concordance(text, span = "2L3R", nodes = "bats")
    ok(nrow(got) == 4, "node filtering")
    got <- corp_concordance(text, span = "2L3R", collocates = "bats")
    ok(nrow(got) == 16, "collocate filtering")

    counts <- corp_surface(text, span = "2L3R")
    ok(identical(corp_concordance(counts), corp_concordance(text, span = "2L3R")), "corp_concordance.corp_surface")
})
