corp_surface <- function(
        text,
        span,
        tokens = NULL,
        nodes = NULL,
        collocates = NULL
    ) {
    if(! is.character(text) || length(text) != 1) {
        stop("'text' must be a single character string")
    }
    if(is.null(tokens)) {
        tokens <- data.table(
            type = unlist(stri_extract_all_words(stri_trans_tolower(text), omit_no_match = TRUE)),
            as.data.table(do.call(rbind, stri_locate_all_words(text, omit_no_match = TRUE)))
        )
    } else {
        if(! is.data.frame(tokens) || ! all(c("type", "start", "end") %in% names(tokens))) {
            stop("'tokens' must be a data.frame containing 'type', 'start' and 'end' columns")
        }
        if(! is.character(tokens$type) || ! is.integer(tokens$start) || ! is.integer(tokens$end)) {
            stop("tokens columns: 'type' must be 'character', and 'start' and 'end' must be 'integer'")
        }
        if(! is.data.table(tokens)) {
            tokens <- as.data.table(tokens)
        }
        if(any(c("idx", "tokens") %in% names(tokens))) {
            warning("'tokens': the 'idx' and 'token' columns are being recalculated", call. = FALSE, immediate. = TRUE)
        }
    }

    # hack to stop R CMD check warnings - ref: data.table
    idx = start = type = token = NULL

    set(tokens, j = "token", value = stri_sub(text, as.matrix(tokens[, c("start", "end")])))
    tokens[, idx := .I]
    setcolorder(tokens, c("idx", "type", "start", "end", "token"))
    setkey(tokens, idx, start, type, token)

    counts = .surface(
        x = tokens$type,
        span = span,
        nodes = nodes,
        collocates = collocates
    )

    obj <- structure(
        list(
            text = text,
            tokens = tokens,
            counts = counts
        ),
        class = c("corp_surface", "corp_cooccurrence")
    )
    attr(obj, "PACKAGE_VERSION") <- packageVersion('CorporaCoCo')
    attr(obj, "DATE") <- Sys.Date()
    attr(obj, "span") <- span
    attr(obj, "nodes") <- nodes
    attr(obj, "collocates") <- collocates

    obj
}

is.corp_cooccurrence <- function(obj) inherits(obj, "corp_cooccurrence")

# s3 methods
# ==========

# corp_cooccurrence
# -----------------
print.corp_cooccurrence <- function(x, ...) {
    print(corp_get_counts(x), ...)
}

# Getters
corp_get_text.corp_cooccurrence <- function(obj) obj$text
corp_get_tokens.corp_cooccurrence <- function(obj) obj$tokens
corp_get_counts.corp_cooccurrence <- function(obj) obj$counts


# corp_surface
# ------------

corp_get_metadata.corp_cooccurrence <- function(obj) {list(
        "PACKAGE_VERSION" = attr(obj, "PACKAGE_VERSION"),
        "DATE" = attr(obj, "DATE"),
        "span" = attr(obj, "span"),
        "nodes" = attr(obj, "nodes"),
        "collocates" = attr(obj, "collocates")
)}

is.corp_surface <- function(obj) inherits(obj, "corp_surface")

corp_type_lookup.corp_surface <- function(obj) {
    # hack to stop R CMD check warnings - ref: data.table
    type = token = NULL

    rv <- corp_get_tokens(obj)[, list("tokens" = paste0(unique(token), collapse = ", ")), by = type]
    setkey(rv)
    return(rv)
}

summary.corp_surface <- function(object, ...) {
    print(
        array(c(
            nrow(object$tokens),
            length(unique(object$tokens$tokens)),
            length(unique(object$tokens$type))
        ), dim = c(3, 1), dimnames = list(c("Tokens", "Unique tokens", "Unique types"), c('count')))
    )
    invisible(object)
}

