\name{surface_coco}
\alias{surface_coco}

\title{Deprecated -- Surface co-occurrence comparison}
\description{
  Convenience function that combined the functionality of the
  \code{\link{surface}} and \code{\link{coco}} functions.
}

\usage{
  # Deprecated
  surface_coco(a, b, span, nodes, fdr = 0.01, collocates = NULL)
}

\arguments{
  \item{a}{
    A character vector.
  }
  \item{b}{
    A character vector.
  }
  \item{span}{
    A character string defining the co-occurrence span.
    See \code{\link{surface}} function for details.
  }
  \item{nodes}{
    A \code{character vector} of nodes or \code{character string}
    representing a single node.
  }
  \item{fdr}{
    The desired level at which to control the False Discovery Rate.
  }
  \item{collocates}{
    A \code{character vector} of collocates or \code{character string}
    representing a single collocate.
  }
}

\value{
  A \code{\link[data.table]{data.table}} of the form
  returned by the \code{\link{coco}} functions.
}

\details{
  See \code{\link{surface}} and \code{\link{coco}}.
}

