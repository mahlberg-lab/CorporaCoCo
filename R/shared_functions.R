fisher <- function(a, b, c, d) {
    f <- fisher.test(matrix(c(a, b, c, d), ncol = 2, byrow = TRUE))
    # need to reverse CI since we are going -ve
    return( list(f$p.value, -log2(f$estimate), -log2(f$conf.int[2]), -log2(f$conf.int[1])) )
}

