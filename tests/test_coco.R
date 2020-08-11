library(CorporaCoCo)
library(data.table)
library(unittest, quietly = TRUE)

# Retrieve functions that are shared across multiple test files
source("tests/test_shared_functions.R")

# -----
# tests
# -----

ok_group("main", {
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

    # ok( is(rv, "coco"), "is class "coco"")
    ok(is(rv, "data.table"), "is class 'data.table'")
    ok(is(rv, "data.frame"), "is class 'data.frame'")

    ok(all.equal(sort(nodes), sort(unique(rv$x))), "expected nodes in results")
    ok(all(rv$y %in% c("man", "plan", "canal", "cat", "badger", "mushroom", "snake")), "all collocates of nodes from both corpora in results")

    ok(all.equal(rv[x == "a" & y == "cat", effect_size], -rv[x == "a" & y == "badger", effect_size]), "effect_size is symetric")
    ok(rv[x == "a" & y == "cat", effect_size] < 0 &&
        rv[x == "a" & y == "cat", CI_upper] < 0
    , "effect_size, CI_upper when co-occurrence ratio is greater in corpus A")
    ok(rv[x == "a" & y == "badger", effect_size] > 0 &&
       rv[x == "a" & y == "badger", CI_lower] > 0
    , "effect_size, CI_lower when co-occurrence ratio is greater in corpus B")
    ok(
        rv[x == "a" & y == "snake", effect_size] < 0 && is.infinite(rv[x == "a" & y == "snake", effect_size]) &&
        rv[x == "a" & y == "snake", CI_lower] < 0 && is.infinite(rv[x == "a" & y == "snake", CI_lower]) &&
        rv[x == "a" & y == "snake", CI_upper] < 0 && is.finite(rv[x == "a" & y == "snake", CI_upper])
    , "effect_size, CI_upper, CI_lower if collocate is only in corpus A")
    ok(
        rv[x == "a" & y == "canal", effect_size] > 0 && is.infinite(rv[x == "a" & y == "canal", effect_size]) &&
        rv[x == "a" & y == "canal", CI_lower] > 0 && is.finite(rv[x == "a" & y == "canal", CI_lower]) &&
        rv[x == "a" & y == "canal", CI_upper] > 0 && is.infinite(rv[x == "a" & y == "canal", CI_upper])
    , "effect_size, CI_upper, CI_lower if collocate is only in corpus B")

    rv_2 <- rv[x == "a"]
    rv_3 <- CorporaCoCo:::.coco(A, B, nodes = "a", fdr = 1.0)
    attr(rv_2, "coco_metadata") <- NULL
    attr(rv_3, "coco_metadata") <- NULL
    ok(identical(rv_2, rv_3), "single node (as a string)")

    rv <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = NULL, fdr = 0.01)
    ok(all(identical(c("a", "a", "a", "a", "a", "a", "the", "the"), sort(rv$x)),
            identical(c("badger", "canal", "canal", "cat", "man", "man", "mushroom", "snake"), sort(rv$y))),
            "correct set of significant results")

    # collocates filter
    rv_f <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = c("badger", "man"), fdr = 0.01)
    ok(identical(rv_f[, -"p_adjusted"], rv[y %in% c("badger", "man"), -"p_adjusted"]), "collocates filter - vector - rows")
    ok(! isTRUE(all.equal(rv_f$p_adjusted, rv[y %in% c("badger", "man")]$p_adjusted)), "collocates filter - vector - p_adjusted values")

    rv_f <- CorporaCoCo:::.coco(A, B, nodes = nodes, collocates = "cat", fdr = 0.01)
    ok(identical(rv_f[, -"p_adjusted"], rv[y == "cat", -"p_adjusted"]), "collocates filter - string - rows")
    ok(isTRUE(all.equal(rv_f$p_value, rv_f$p_adjusted)), "collocates filter - string - p_adjusted values")

    # no rows returned
    rv <- CorporaCoCo:::.coco(A, B, nodes = "chimerical", collocate = NULL, fdr = 1.0)
    ok(nrow(rv) == 0, "no rows returned")
})


ok_group("bad arguments", {
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

    ok(test_for_error(CorporaCoCo:::.coco(A = "foo", B = B, nodes = nodes, collocates = NULL, fdr = 0.01), "'A'"), "bad A - not a data.frame")
    ok(test_for_error(CorporaCoCo:::.coco(A = ABad, B = B, nodes = nodes, collocates = NULL, fdr = 0.01), "'A'"), "bad A - bad column")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = as.matrix(B), nodes = nodes, collocates = NULL, fdr = 0.01), "'B'"), "bad B - not a data.frame")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = BBad, nodes = nodes, collocates = NULL, fdr = 0.01), "'B'"), "bad B - bad column")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = 1:3, collocates = NULL, fdr = 0.01), "'nodes'"), "bad nodes - not char")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = 0.0), "'fdr'"), "bad fdr - zero")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = -0.4), "'fdr'"), "bad fdr - negative")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = 100), "'fdr'"), "bad fdr - greater than one")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = "100"), "'fdr'"), "bad fdr - char")
    ok(test_for_error(CorporaCoCo:::.coco(A = A, B = B, nodes = nodes, collocates = NULL, fdr = c(0.05, 0.10)), "'fdr'"), "bad fdr - length 2 numeric vector")
})
