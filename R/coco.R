coco <- function(A, B, nodes, fdr = 0.01, collocates = NULL) {
    if(! 'data.frame' %in% class(A)) stop("'A' must be a data.frame.")
    if(! 'data.frame' %in% class(B)) stop("'B' must be a data.frame.")
    if(! identical(c(x = 'character', y = 'character', H = 'integer', M = 'integer'), sapply(A, class))) stop("'A' looks wrong. Please check the required form of A.")
    if(! identical(c(x = 'character', y = 'character', H = 'integer', M = 'integer'), sapply(B, class))) stop("'B' looks wrong. Please check the required form of B.")
    if(! is.character(nodes) | length(nodes) < 1) stop("'nodes' must be a character vector of at least length one.")
    if(! is.numeric(fdr) || length(fdr) > 1) stop("'fdr' must be a single number.")
    if(fdr <= 0.0 || fdr > 1.0) stop("'fdr' must be greater than zero and less than or equal to zero.")
    if(length(collocates) != 0 && (! is.character(collocates) | length(collocates) < 1)) stop("'collocates' must be a character vector of at least length one.")
     
    # hack to stop R CMD check warnings
    x = y = H = M = H_A = H_B = M_A = M_B = p_adjusted = p_value = i.T = NULL

    # always use data.frames internally
    A <- as.data.table(A)
    setkey(A, x, y)
    B <- as.data.table(B)
    setkey(B, x, y)

    # filter by collocates
    if(length(collocates) != 0) {
        A <- A[y %in% collocates] 
        B <- B[y %in% collocates] 
    }

    # filter by nodes and combine
    DT <- merge(A[list(nodes)], B[list(nodes)], by= c('x', 'y'), suffixes = c("_A", "_B"), all = TRUE)
    # if pair missing in one corpus must be replaced by no hits and all misses
    # total possible hits for each node
    A_max <- A[list(nodes)][! duplicated(x), list(x, T = H+M)]
    A_max[is.na(T), T := 0]
    setkey(A_max, x)
    B_max <- B[list(nodes)][! duplicated(x), list(x, T = H+M)]
    B_max[is.na(T), T := 0]
    setkey(B_max, x)
    DT[A_max, M_A := ifelse(is.na(M_A), i.T, M_A), on = 'x']
    DT[is.na(H_A), H_A := 0]
    DT[B_max, M_B := ifelse(is.na(M_B), i.T, M_B), on = 'x']
    DT[is.na(H_B), H_B := 0]

    DT <- na.omit(DT)  # will get NA's for y if a node is not in either x

    if(nrow(DT) > 0) {
        # Fisher
        # calculating the confidence interval is expensive so we will only do it for the significant values
        DT[, c('p_value', 'effect_size', 'CI_lower', 'CI_upper') := fisher(H_A, H_B, M_A, M_B), by = 1:nrow(DT)]
        # FDR
        DT[, p_adjusted := p.adjust(p_value, method = 'BH'), by = x]
        DT <- DT[p_adjusted <= fdr]
    } else {
        # still need the columns for consistent output
        DT[, c('p_value', 'effect_size', 'CI_lower', 'CI_upper', 'p_adjusted') := list(numeric(), numeric())]
    }

    setcolorder(DT, c('x', 'y', 'H_A', 'M_A', 'H_B', 'M_B', 'effect_size', 'CI_lower', 'CI_upper', 'p_value', 'p_adjusted'))

    return( coco_construct(DT, nodes = nodes, fdr = fdr) )
}

