fisher <- function(a, b, c, d) {
    f <- fisher.test(matrix(c(a, b, c, d), ncol = 2, byrow = TRUE))
    # need to reverse CI since we are going -ve
    return( list(f$p.value, -log2(f$estimate), -log2(f$conf.int[2]), -log2(f$conf.int[1])) )
}

parse_span <- function(x) {
    if(! is.character(x) || length(x) > 1) stop("span must be a character string")
    m <- regexec(text = x, pattern = '^((\\d+)([LR]))((\\d*)([LR]))?$')
    s <- regmatches(x, m)[[1]]
    if(length(s) != 7) stop('span looks wrong')
    if(s[4] == s[7]) stop(paste0(s[4], ' given twice in span'))
    if(s[3] == 0 && (s[6] == 0 || s[6] == "")) stop('span must be specified with at least one none-zero direction')
    rv <- list(left = 0, right = 0)
    if(s[4] == 'L') {
        rv$left <- as.numeric(s[3])
        if(s[7] == 'R') {
            rv$right <- ifelse(s[6] == "", rv$left, as.numeric(s[6]))
        }
    } else {
        rv$right <- as.numeric(s[3])
        if(s[7] == 'L') {
            rv$left <- ifelse(s[6] == "", rv$right, as.numeric(s[6]))
        }
    }
    return(rv)
}


