corp_coco <- function(A, B, nodes, collocates = NULL, fdr = 0.01) {
    obj <- .coco(
        A = corp_get_counts(A),
        B = corp_get_counts(B),
        nodes = nodes,
        collocates = collocates,
        fdr = fdr
    )
    class(obj) <- append("corp_coco", class(obj))
    attr(obj, "PACKAGE_VERSION") <- packageVersion('CorporaCoCo')
    attr(obj, "DATE") <- Sys.Date()
    attr(obj, "nodes") <- nodes
    attr(obj, "collocates") <- collocates
    attr(obj, "fdr") <- fdr
    #obj <- sticky(obj)
    invisible(obj)
}


# s3 methods
# ----------
# ref: http://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Generic-functions-and-methods
# for explanation of argument names

corp_get_metadata.corp_coco <- function(obj) {list(
        "PACKAGE_VERSION" = attr(obj, "PACKAGE_VERSION"),
        "DATE" = attr(obj, "DATE"),
        "nodes" = attr(obj, "nodes"),
        "collocates" = attr(obj, "collocates"),
        "fdr" = attr(obj, "fdr")
)}

# TODO: old coco objects need to plot too
plot.corp_coco <- function(x, as_matrix = FALSE, nodes = NULL, forest_plot_args = NULL, ...) {
    # hack to stop R CMD check warnings
    effect_size = V1 = y = CI_upper = CI_lower = NULL

    # can't use metadata attr - data.table functions do not preserve non-data.table attributes - https://github.com/Rdatatable/data.table/issues/995
    if(! is.null(nodes)) {
        if(! all(nodes %in% unique(x[,x]))) {
            warning('Some of the supplied plot nodes are not in the significant results and therefore will be absent from the plot')
        }
        x <- x[x %in% nodes]
    }
    if(nrow(x) == 0) {
        warning("Nothing to plot: 'x' has zero rows.")
    } else {
        op <- par(no.readonly=TRUE)
        on.exit(par(op))
        setkey(x)

        if(as_matrix) {
            if(! is.null(nodes)) {
                node_order <- nodes
            } else {
                node_order <- x[, sum(effect_size), by = list(x)][order(-V1)]$x
            }
            collocate_order <- x[, sum(sign(effect_size)), by = list(y)][order(V1)]$y
            DT <- dcast(x, x ~ y, value.var = 'effect_size')
            m <- as.matrix( DT[, -1, with = FALSE] )
            dimnames(m) <- list(DT$x, dimnames(m)[[2]])
            m <- m[node_order, collocate_order, drop = FALSE]
            colors <- colorRampPalette(brewer.pal(11, 'PRGn'))(1024)
            if(all(is.infinite(x$effect_size))) {
                # must only have infinite effect_size scale limit is irrelevant
                scale_limit <- 1
            } else {
                scale_limit <- ceiling(max(abs(range(x$effect_size, finite = TRUE))))
            }
            x_axis_labels_width <- max(strwidth(collocate_order, units = 'inches', cex = par('cex.lab')))
            y_axis_labels_width <- max(strwidth(node_order, units = 'inches', cex = par('cex.lab')))
            par(omi = c(x_axis_labels_width, y_axis_labels_width, 0, 0))
            # Inf is considered missing value and plotted as transparent
            image(
                1:ncol(m), 1:nrow(m), t(m), zlim = c(-scale_limit, scale_limit),
                xlab = 'Collocates',
                ylab = 'Nodes',
                xaxt = 'n',
                yaxt = 'n',
                bty = 'n',
                col = colors
            )
            axis(1, at = 1:length(collocate_order), labels = collocate_order, tick = FALSE, las = 2)
            axis(2, at = 1:length(node_order), labels = node_order, tick = FALSE, las = 2)
            # plot infinites (incalculables) 
            dimnames(m) <- NULL  # want dimnames as numbers to use as co-ordinated
            m <- melt(m, variable.factor = FALSE)
            m <- m[is.infinite(m$value), , drop = FALSE]
            if(nrow(m) > 0) {
                rect(m[ , 2]-0.5, m[ , 1]-0.5, m[ , 2]+0.5, m[ , 1]+0.5, col = 'lightgrey', border = NA)
            }
        } else {
            scale_limit = ceiling(max(abs(range(x$CI_lower, x$CI_upper, finite = TRUE))))
            plot_args <- list(
                xlim = c(-scale_limit, scale_limit),
                xlab = 'Effect Size',
                main = NULL,
                sub = NULL,
                asp = NA,
                pch = 15,
                cex.pch = 1,
                lwd.xaxt = 1,
                col.xaxt = 'black',
                col.whisker = 'black',
                col.zero = 'darkgray',
                length.wisker_end = 0.05
            )
            if(! is.null(forest_plot_args)){
                if(! is.list(forest_plot_args)) stop("if 'forest_plot_args' is supplied then it must be a list")
                plot_args <- list.merge(plot_args, forest_plot_args)
            }
            if(is.null(nodes)) {
                nodes <- sort(unique(x$x))
            } else {
                nodes <- nodes[nodes %in% x$x]
            }
            x <- x[nodes]
            gaps <- length(nodes) - 1
            max_x_chars <- max(nchar(x$x))
            max_y_chars <- max(nchar(x$y))
            max_y_label_width <- strwidth(paste(rep('a', max_x_chars + max_y_chars + 1), collapse = ''), units = 'inches', cex = par('cex.lab'), family = "mono")
            par(omi = c(0, max_y_label_width, 0, 0))
            # empty plot
            plot(
                0, 0,
                xlim = plot_args$xlim,
                ylim = c(0, nrow(x) + gaps),
                xlab = plot_args$xlab,
                ylab = '',
                yaxt = 'n',
                xaxt = 'n',
                bty = 'n',
                type = 'n',
                main = plot_args$main,
                sub = plot_args$sub,
                asp = plot_args$asp
            )
            axis(1, lwd = plot_args$lwd.xaxt, col = plot_args$col.xaxt)
            # and fill it in
            y_start = 0
            y_labels <- c()
            for(node in nodes) {
                # -ve infinite
                DT <- subset(x, x == node & effect_size == -Inf)[order(CI_upper)]
                DT[, "rank" := .I]
                if(nrow(DT) > 0) {
                    arrows(-scale_limit, DT$rank + y_start, DT$CI_upper, DT$rank + y_start, code = 2, length = plot_args$length.wisker_end, angle = 90, col = plot_args$col.whisker )
                    y_labels <- c(y_labels, paste(format(DT$x, width = max_x_chars, justify = 'right'), format(DT$y, width = max_y_chars, justify = 'left'), sep = ' '))
                    y_start <- y_start + nrow(DT)
                }
                # finite
                DT <- subset(x, x == node & is.finite(effect_size))[order(effect_size)]
                DT[ , rank := .I]
                if(nrow(DT) > 0) {
                    points(
                        DT$effect_size, 1:nrow(DT) + y_start,
                        pch = plot_args$pch,
                        cex = plot_args$cex.pch
                    )
                    arrows(DT$CI_lower, DT$rank + y_start, DT$CI_upper, DT$rank + y_start, code = 3, length = plot_args$length.wisker_end, angle = 90, col = plot_args$col.whisker )
                    # need to overplot the arrows which may have a different color
                    points(
                        DT$effect_size, 1:nrow(DT) + y_start,
                        pch = plot_args$pch,
                        cex = plot_args$cex.pch
                    )
                    y_labels <- c(y_labels, paste(format(DT$x, width = max_x_chars, justify = 'right'), format(DT$y, width = max_y_chars, justify = 'left'), sep = ' '))
                    y_start <- y_start + nrow(DT)
                }
                # +ve infinite
                DT <- subset(x, x == node & effect_size == Inf)[order(CI_lower)]
                DT[ , rank := .I]
                if(nrow(DT) > 0) {
                    arrows(DT$CI_lower, DT$rank + y_start, scale_limit, DT$rank + y_start, code = 1, length = plot_args$length.wisker_end, angle = 90, col = plot_args$col.whisker )
                    y_labels <- c(y_labels, paste(format(DT$x, width = max_x_chars, justify = 'right'), format(DT$y, width = max_y_chars, justify = 'left'), sep = ' '))
                    y_start <- y_start + nrow(DT)
                }
                # add gap
                y_start <- y_start + 1
                y_labels <- c(y_labels, '')
            }
            par(family = "mono")
            axis(2, at = 1:length(y_labels), labels = y_labels, tick = FALSE, las = 2)
            # zero effect reference
            abline(v = 0, lty = 5, col = plot_args$col.zero)
        }
    }
    invisible(NULL)
}

