test_that("main", {
    A <- CorporaCoCo:::.surface(
        x = c(
            rep(c("a", "man", NA), 100),
            rep(c("a", "plan", NA), 100),
            rep(c("a", "cat", NA), 100),
            rep(c("a", "badger", NA), 50),
            rep(c("a", "snake", NA), 100),
            rep(c("the", "man", NA), 100),
            rep(c("the", "plan", NA), 100),
            rep(c("another", "man", NA), 100),
            rep(c("another", "plan", NA), 100)
        ),
        span = "1R", nodes = NULL, collocates = NULL
    )
    B <- CorporaCoCo:::.surface(
        x = c(
            rep(c("a", "man", NA), 60),
            rep(c("a", "plan", NA), 100),
            rep(c("a", "cat", NA), 50),
            rep(c("a", "badger", NA), 100),
            rep(c("a", "mushroom", NA), 100),
            rep(c("a", "canal", NA), 40),
            rep(c("the", "man", NA), 60),
            rep(c("the", "plan", NA), 100),
            rep(c("the", "canal", NA), 40),
            rep(c("another", "man", NA), 60),
            rep(c("another", "plan", NA), 100),
            rep(c("another", "canal", NA), 40)
        ),
        span = "1R", nodes = NULL, collocates = NULL
    )
    nodes <- c("a", "the")

    rv <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = NULL, fdr = 1.0)

    # expect_equal( is(rv, "coco"), "is class "coco"")
    expect_true(is(rv, "data.table"))
    expect_true(is(rv, "data.frame"))

    expect_equal(sort(nodes), sort(unique(rv$x)))

    # Test all collocates of nodes from both corpora in results
    expect_true(all(rv$y %in% c("man", "plan", "canal", "cat", "badger", "mushroom", "snake")))

    # Test effect_size is symetric
    expect_equal(rv[x == "a" & y == "cat", effect_size], -rv[x == "a" & y == "badger", effect_size])

    # Test  effect_size, CI_upper when co-occurrence ratio is greater in corpus A
    expect_lt(rv[x == "a" & y == "cat", effect_size], 0)
    expect_lt(rv[x == "a" & y == "cat", CI_upper], 0)

    # Test effect_size, CI_lower when co-occurrence ratio is greater in corpus B
    expect_gt(rv[x == "a" & y == "badger", effect_size], 0)
    expect_gt(rv[x == "a" & y == "badger", CI_lower], 0)

    # Test effect_size, CI_upper, CI_lower if collocate is only in corpus A
    expect_lt(rv[x == "a" & y == "snake", effect_size], 0)
    expect_true(is.infinite(rv[x == "a" & y == "snake", effect_size]))
    expect_lt(rv[x == "a" & y == "snake", CI_lower], 0)
    expect_true(is.infinite(rv[x == "a" & y == "snake", CI_lower]))
    expect_lt(rv[x == "a" & y == "snake", CI_upper], 0)
    expect_true(is.finite(rv[x == "a" & y == "snake", CI_upper]))

    #Test effect_size, CI_upper, CI_lower if collocate is only in corpus B
    expect_gt(rv[x == "a" & y == "canal", effect_size], 0)
    expect_true(is.infinite(rv[x == "a" & y == "canal", effect_size]))
    expect_gt(rv[x == "a" & y == "canal", CI_lower], 0)
    expect_true(is.finite(rv[x == "a" & y == "canal", CI_lower]))
    expect_gt(rv[x == "a" & y == "canal", CI_upper], 0)
    expect_true(is.infinite(rv[x == "a" & y == "canal", CI_upper]))

    rv_2 <- rv[x == "a"]
    rv_3 <- CorporaCoCo:::.coco(A, B, nodes = "a", fdr = 1.0)
    attr(rv_2, "coco_metadata") <- NULL
    attr(rv_3, "coco_metadata") <- NULL
    expect_identical(rv_2, rv_3)

    rv <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = NULL, fdr = 0.01)
    expect_identical(c("a", "a", "a", "a", "a", "a", "the", "the"), sort(rv$x))
    expect_identical(c("badger", "canal", "canal", "cat", "man", "man", "mushroom", "snake"), sort(rv$y))

    # collocates filter
    rv_f <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = c("badger", "man"), fdr = 0.01)
    # Test collocates filter - vector - rows
    expect_identical(rv_f[, -"p_adjusted"], rv[y %in% c("badger", "man"), -"p_adjusted"])
    # Test collocates filter - vector - p_adjusted values
    expect_match(all.equal(rv_f$p_adjusted, rv[y %in% c("badger", "man")]$p_adjusted), "Mean relative difference: [0-9.]+")

    rv_f <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = "cat", fdr = 0.01)
    # Test collocates filter - string - rows
    expect_identical(rv_f[, -"p_adjusted"], rv[y == "cat", -"p_adjusted"])
    # Test collocates filter - string - p_adjusted values
    expect_equal(rv_f$p_value, rv_f$p_adjusted)

    # no rows returned
    rv <- CorporaCoCo:::.coco(A, B, nodes = "chimerical", collocate = NULL, fdr = 1.0)
    expect_equal(nrow(rv), 0)
})


test_that("bad arguments", {
    A <- CorporaCoCo:::.surface(
        x = c(
            rep(c("a", "man", NA), 100),
            rep(c("a", "plan", NA), 100),
            rep(c("the", "man", NA), 100),
            rep(c("the", "plan", NA), 100),
            rep(c("another", "man", NA), 100),
            rep(c("another", "plan", NA), 100)
        ),
        span = "1R", nodes = NULL, collocates = NULL
    )
    B <- CorporaCoCo:::.surface(
        x = c(
            rep(c("a", "man", NA), 60),
            rep(c("a", "plan", NA), 100),
            rep(c("a", "canal", NA), 40),
            rep(c("the", "man", NA), 60),
            rep(c("the", "plan", NA), 100),
            rep(c("the", "canal", NA), 40),
            rep(c("another", "man", NA), 60),
            rep(c("another", "plan", NA), 100),
            rep(c("another", "canal", NA), 40)
        ),
        span = "1R", nodes = NULL, collocates = NULL
    )
    nodes <- c("a", "the")

    ABad <- A
    ABad$x <- as.factor(ABad$x)
    BBad <- B
    BBad$H <- as.factor(BBad$H)

    # bad A - not a data.frame
    expect_error(CorporaCoCo:::.coco(A = "foo", B = B, nodes = nodes, collocates = NULL, fdr = 0.01), "'A'")
    # bad A - bad column
    expect_error(CorporaCoCo:::.coco(A = ABad, B = B, nodes = nodes, collocates = NULL, fdr = 0.01), "'A'")
    # bad B - not a data.frame
    expect_error(CorporaCoCo:::.coco(A = A, B = as.matrix(B), nodes = nodes, collocates = NULL, fdr = 0.01), "'B'")
    # bad B - bad column
    expect_error(CorporaCoCo:::.coco(A = A, B = BBad, nodes = nodes, collocates = NULL, fdr = 0.01), "'B'")
    # bad nodes - not char
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = 1:3, collocates = NULL, fdr = 0.01), "'nodes'")
    # bad fdr - zero
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = 0.0), "'fdr'")
    # bad fdr - negative
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = -0.4), "'fdr'")
    # bad fdr - greater than one
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = 100), "'fdr'")
    # bad fdr - char
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = "100"), "'fdr'")
    # bad fdr - length 2 numeric vector
    expect_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = c(0.05, 0.10)), "'fdr'")
})
