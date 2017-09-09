pkg_vars <- new.env()
pkg_vars$unittest_surface_include_na = FALSE

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

surface <- function(x, span, nodes = NULL, collocates = NULL) {
    if(! is.character(x)) stop("'x' must be a character vector")
    if(length(x) < 2) stop("'x' must be a vector of at least length two")
    s <- parse_span(span)    
    if(length(nodes) != 0 && ! is.character(nodes)) stop("'nodes' must be a character vector.")
    if(length(collocates) != 0 && ! is.character(collocates)) stop("'collocates' must be a character vector.")

    # hack to stop R CMD check warnings
    y = M = H = NULL

    # filter x (doing this early reduces memory footprint
    if(length(nodes) != 0) {
        i <- which(x %in% nodes)
        x <- x[intersect(
            1:length(x),
            as.vector(vapply(i, function(ii) {(ii-s$left):(ii+s$right)}, FUN.VALUE = integer(s$left + s$right + 1)))
        )]
    }

    DT <- data.table(x = x)
    if(s$right != 0) {
        DT[, (paste("R", 1:s$right, sep="_")) := shift(x, 1:s$right, type = 'lead')]
    }
    if(s$left != 0) {
        DT[, (paste("L", 1:s$left, sep="_")) := shift(x, 1:s$left, type = 'lag')]
    }
    DT <- melt(DT, id.vars = 'x', value.name = 'y')[,list(x,y)]

    setkey(DT)
    DT <- DT[, list(H = .N), by = list(x, y)]
    DT[!is.na(x) & !is.na(y), M := sum(H) - H, by = list(x)]

    if(length(nodes) != 0) {
        DT <- DT[x %in% nodes]
    }
    if(length(collocates) != 0) {
        DT <- DT[y %in% collocates]
    }

    if(pkg_vars$unittest_surface_include_na){
        return(DT)
    } else {
        return( na.omit(DT) )
    }
}

