corp_text <- function(
        text,
        tokens = NULL
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
        if(! is.character(tokens$type) || ! is.numeric(tokens$start) || ! is.numeric(tokens$end)) {
            stop("tokens columns: 'type' must be 'character', and 'start' and 'end' must be 'numeric'")
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

    obj <- structure(
        list(
            text = text,
            tokens = tokens
        ),
        class = "corp_text"
    )
    attr(obj, "PACKAGE_VERSION") <- packageVersion('CorporaCoCo')
    attr(obj, "DATE") <- Sys.Date()

    obj
}

is.corp_text <- function(obj) inherits(obj, "corp_text")

corp_text_rbindlist <- function(x) {
    # hack to stop R CMD check warnings - ref: data.table
    start = end = NULL

    Ns <- sapply(x, function(xx) nchar(xx$text, type = "chars"))
    offsets <- shift(Ns, 1, fill = 0)
    corp_text(
        text = paste(sapply(x, corp_get_text), collapse = " "),
        tokens <- rbindlist(lapply(1:length(x), function(i) {
            x[[i]]$tokens[, c("start", "end") := list(start + offsets[i] + i - 1, end + offsets[i] + i - 1)]
            return(x[[i]]$tokens[, c("type", "start", "end")])
        }))
    )
}

# s3 methods
# ==========

corp_get_text.corp_text <- function(obj) obj$text
corp_get_tokens.corp_text <- function(obj) obj$tokens

corp_get_metadata.corp_text <- function(obj) {list(
        "PACKAGE_VERSION" = attr(obj, "PACKAGE_VERSION"),
        "DATE" = attr(obj, "DATE")
)}

corp_type_lookup.corp_text <- function(obj) {
    # hack to stop R CMD check warnings - ref: data.table
    type = token = NULL

    rv <- corp_get_tokens(obj)[, list("tokens" = paste0(unique(token), collapse = ", ")), by = type]
    setkey(rv)
    return(rv)
}

summary.corp_text <- function(object, ...) {
    print(
        array(c(
            nrow(object$tokens),
            length(unique(object$tokens$tokens)),
            length(unique(object$tokens$type))
        ), dim = c(3, 1), dimnames = list(c("Tokens", "Unique tokens", "Unique types"), c('count')))
    )
    invisible(object)
}

