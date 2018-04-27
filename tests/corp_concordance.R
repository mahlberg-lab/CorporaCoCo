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

ok_group("corp_concordance", {
    x <- "`But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way, `Do cats eat bats? Do cats eat bats?' and sometimes, `Do bats eat cats?' for, you see, as she couldn't answer either question, it didn't much matter which way she put it."

    # this is silly but it tests if we can concordance every instance of every type
    counts <- corp_surface(x, span = '2L3R')
    got <- corp_concordance(counts)
    ok(nrow(got) == 59, "with no filtering there is a concordance for every token")
    got <- corp_concordance(counts, nodes = "bats")
    ok(nrow(got) == 4, "node filtering")
    got <- corp_concordance(counts, collocates = "bats")
    ok(nrow(got) == 16, "collocate filtering")
})

ok_group("corp_concordance", {
    x <- "`But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way, `Do cats eat bats? Do cats eat bats?' and sometimes, `Do bats eat cats?' for, you see, as she couldn't answer either question, it didn't much matter which way she put it."

    # this is silly but it tests if we can concordance every instance of every type
    counts <- corp_surface(x, span = '2L3R')
    got <- corp_concordance(counts)
    ok(nrow(got) == 59, "with no filtering there is a concordance for every token")
    got <- corp_concordance(counts, nodes = "bats")
    ok(nrow(got) == 4, "node filtering")
    got <- corp_concordance(counts, collocates = "bats")
    ok(nrow(got) == 16, "collocate filtering")
})
