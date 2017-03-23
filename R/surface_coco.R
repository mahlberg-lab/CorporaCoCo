surface_coco <- function(a, b, span, nodes, fdr = 0.01) {
    DT_A <- surface(x = a, span = span) 
    DT_B <- surface(x = b, span = span) 
    rv <- coco(A = DT_A, B = DT_B, nodes = nodes, fdr = fdr)
    return(rv)
}

