surface_coco <- function(a, b, span, nodes, fdr = 0.01, collocates = NULL) {
    DT_A <- surface(x = a, span = span, nodes = nodes, collocates = collocates) 
    DT_B <- surface(x = b, span = span, nodes = nodes, collocates = collocates) 
    rv <- coco(A = DT_A, B = DT_B, nodes = nodes, fdr = fdr, collocates = collocates)
    return(rv)
}

