.surface_coco <- function(a, b, span, nodes, fdr, collocates) {
    DT_A <- .surface(x = a, span = span, nodes = nodes, collocates = collocates)
    DT_B <- .surface(x = b, span = span, nodes = nodes, collocates = collocates)
    rv <- .coco(A = DT_A, B = DT_B, nodes = nodes, fdr = fdr, collocates = collocates)
    return(rv)
}

surface_coco <- function(a, b, span, nodes, fdr = 0.01, collocates = NULL) {
    warning("The 'surface_coco' function is deprecated. Please consider first running 'corp_surface', then 'corp_coco'.", call. = FALSE, immediate. = TRUE)
    .surface_coco(a = a, b = b, span = span, nodes = nodes, fdr = fdr, collocates = collocates)
}
