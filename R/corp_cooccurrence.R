corp_surface <- function(
        text,
        span,
        nodes = NULL,
        collocates = NULL
    ) {
    if (! is.corp_text(text)) {
        stop("'text' must be a 'corp_text' object")
    }

    counts <- .surface(
        x = text$tokens$type,
        span = span,
        nodes = nodes,
        collocates = collocates
    )

    obj <- structure(
        list(
            text = text,
            counts = counts
        ),
        class = c("corp_surface", "corp_cooccurrence")
    )
    attr(obj, "PACKAGE_VERSION") <- packageVersion("CorporaCoCo")
    attr(obj, "DATE") <- Sys.Date()
    attr(obj, "span") <- span
    attr(obj, "nodes") <- nodes
    attr(obj, "collocates") <- collocates

    obj
}

is.corp_cooccurrence <- function(obj) inherits(obj, "corp_cooccurrence")
is.corp_surface <- function(obj) inherits(obj, "corp_surface")

# s3 methods
# ==========

# corp_cooccurrence
# -----------------
print.corp_cooccurrence <- function(x, ...) {
    print(corp_get_counts(x), ...)
}

# Getters
corp_get_text.corp_cooccurrence <- function(obj) corp_get_text(obj$text)
corp_get_text_obj.corp_cooccurrence <- function(obj) obj$text
corp_get_tokens.corp_cooccurrence <- function(obj) corp_get_tokens(obj$text)
corp_get_counts.corp_cooccurrence <- function(obj) obj$counts
corp_get_metadata.corp_cooccurrence <- function(obj) {
    list(
        "PACKAGE_VERSION" = attr(obj, "PACKAGE_VERSION"),
        "DATE" = attr(obj, "DATE"),
        "span" = attr(obj, "span"),
        "nodes" = attr(obj, "nodes"),
        "collocates" = attr(obj, "collocates")
)}
