\name{plot.corp_coco}
\alias{plot.corp_coco}

\title{plot.corp_coco}
\description{
  Plotting \code{corp_coco} objects.
}

\usage{
  \method{plot}{corp_coco}(x, as_matrix = FALSE, nodes = NULL, forest_plot_args = NULL, \dots)
}
\arguments{
  \item{x}{
     An \code{corp_coco} object.
  }
  \item{as_matrix}{
     If \code{as_matrix} is set to \code{TRUE} a matrix plot
     rather than a forest plot is produced.
  }
  \item{nodes}{
     If a vector of \code{nodes} is supplied this will
     be used to filter the set of results that are plotted.
     If \code{nodes} are supplied the plot will use the
     \code{nodes} order.
  }
  \item{forest_plot_args}{
     This is a list of arguments that is passed to the
     \code{\link{plot.default}} that produces the foundation
     of the forest plot. The list may contain a subset or
     all of the following documented arguments; any arguments that are
     not documented here will be ignored. A description of each argument
     can be found in the help for the
     \code{\link{plot.default}} function.
     Available arguments are
     \itemize{
         \item \code{xlim} Default: Calculated from the ranges of the confidence intervals.
         \item \code{xlab} Default: 'Effect Size'
         \item \code{main} Default: NULL
         \item \code{sub} Default: NULL
         \item \code{asp} Default: NA
         \item \code{pch} Default: 15
         \item \code{cex.pch} Default: 1
         \item \code{lwd.xaxt} Default: 1
         \item \code{col.xaxt} Default: 'black'
         \item \code{col.whisker} Default: 'black'
         \item \code{col.zero} Default: 'darkgray'
         \item \code{length.wisker_end} Default: 0.05
      }
  }
  \item{...}{
     Other arguments will be ignored.
  }
}

\details{
  An object of class \code{corp_coco} is returned by
  \code{\link{corp_coco}()}.
}


