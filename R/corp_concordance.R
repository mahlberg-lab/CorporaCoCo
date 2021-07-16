corp_concordance = function(obj, span, nodes, collocates, context) UseMethod("corp_concordance")

corp_concordance.corp_surface <- function(obj, span = attr(obj, "span"), nodes = attr(obj, "nodes"), collocates = attr(obj, "collocates"), context = 3) {
    corp_concordance(corp_get_text_obj(obj), span = span, nodes = nodes, collocates = collocates, context = context)
}

corp_concordance.corp_text <- function(obj, span, nodes = NULL, collocates = NULL, context = 3) {
    # hack to stop R CMD check warnings - ref: data.table
    idx = type = NULL

    s <- parse_span(span)
    n_tokens <- nrow(obj$tokens)
    n_chars <- nchar(obj$text, type = "chars")

    L_cols <- paste0("L", s$left:1)
    R_cols <- paste0("R", 1:s$right)
    if(! is.null(nodes)) {
        wanted <- obj$tokens[type %in% nodes, list(idx)]
    } else {
        wanted <- obj$tokens[, list(idx)]
    }
    set(wanted, j = c("CL_L", "CL_R", L_cols, "N", R_cols, "CR_L", "CR_R"), value = lapply(c(-s$left-context, -s$left-1, -(s$left):(s$right), s$right+1, s$right+context), function(x) wanted$idx + x))
    wanted[wanted < 1 | wanted > n_tokens] <- NA

    # TODO: edge whitespace/punctuation and context is broken
    rv <- wanted[, list(idx)]
    cols <- c(L_cols, "N", R_cols)
    set(rv, j = "CL", value = stri_sub(obj$text, from = obj$tokens[wanted$CL_L]$start, to = obj$tokens[wanted$CL_R + 1]$start - 1)) # TODO
    set(rv, j = cols, value = lapply(cols, function(x) obj$tokens[wanted[[x]]]$token))
    set(rv, j = "CR", value = stri_sub(obj$text, from = obj$tokens[wanted$CR_L]$start, to = obj$tokens[wanted$CR_R]$end)) # TODO
    set(rv, j = paste0(cols, "_type"), value = lapply(cols, function(x) obj$tokens[wanted[[x]]]$type))
    set(rv, j = paste0("_", cols), value = lapply(cols, function(x) stri_sub(obj$text, from = obj$tokens[wanted[[x]]]$end + 1, to = obj$tokens[wanted[[x]]+1]$start - 1))) # TODO

    if(! is.null(collocates)) {
        # TODO: slow?
        type_cols <- grep('^[LR]\\d+_type$', names(rv), value = TRUE)
        rv <- rv[apply(rv[, type_cols, with = FALSE], 1, function(x) any(collocates %in% x))]
    }

    setkeyv(rv, grep('^[^_]', names(rv), value = TRUE))

    class(rv) <- append("corp_concordance", class(rv))
    attr(rv, "PACKAGE_VERSION") <- packageVersion('CorporaCoCo')
    attr(rv, "DATE") <- Sys.Date()
    attr(rv, "span") <- span
    attr(rv, "nodes") <- nodes
    attr(rv, "collocates") <- collocates

    return(rv)
}

corp_get_metadata.corp_concordance <- function(obj) {list(
        "PACKAGE_VERSION" = attr(obj, "PACKAGE_VERSION"),
        "DATE" = attr(obj, "DATE"),
        "span" = attr(obj, "span"),
        "nodes" = attr(obj, "nodes"),
        "collocates" = attr(obj, "collocates")
)}


print.corp_concordance <- function(x, collocates = attr(x, "collocates"), collocate_marker = "*", as_data_table = FALSE, ...) {
    if(as_data_table) {
        rv <- NextMethod()
    } else {
        # using something like %12s in sprintf seemed very broken with UTF-8 text so padding manually with nchar which seems to work
        # x is a list of strings
        x <- copy(x)
        # TODO: slow?
        if(! is.null(collocates)) {
            token_cols <- grep('^[LR]\\d+$', names(x), value = TRUE)
            for(col in token_cols) {
               set(x, j = col, value = ifelse(x[[paste0(col, "_type")]] %in% collocates, paste0(collocate_marker, x[[col]], collocate_marker), x[[col]]))
            }
        }

        L_cols <- grep("^L\\d+$", colnames(x), value = TRUE)
        L_cols <- c(rbind(L_cols, paste0("_", L_cols)))
        R_cols <- grep("^R\\d+$", colnames(x), value = TRUE)
        R_cols <- c("_N", rbind(R_cols, paste0("_", R_cols)))

        L <- paste(x$CL, "--- ", apply(x[, L_cols, with = FALSE], 1, paste, collapse = ""), sep = "")
        L_len <- max(nchar(L, type = "chars"))

        R <- paste(apply(x[, R_cols, with = FALSE], 1, paste, collapse = ""), "--- ", x$CR, sep = "")
        R_len <- max(nchar(R, type = "chars"))

        N_len <- max(nchar(x$N, type = "chars"))

        labels <- as.character(1:nrow(x))
        labels_len <- max(nchar(labels, type = "chars"))

        rv <- paste(
            "[", strrep(" ", labels_len - nchar(labels, type = "chars")), labels, "] ",
            strrep(" ", L_len - nchar(L, type = "chars")), L,
            strrep(" ", ceiling((N_len - nchar(x$N, type = "chars")) / 2)), x$N, strrep(" ", floor((N_len - nchar(x$N, type = "chars")) / 2)),  # center
            R, strrep(" ", R_len - nchar(R, type = "chars")),
            sep = ""
        )
        cat(noquote(rv), sep = "\n")
    }
    invisible(rv)
}
