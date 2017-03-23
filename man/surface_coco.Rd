\name{surface_coco}
\alias{surface_coco}

\title{Surface co-occurrence comparison}
\description{
  Convenience function that combined the functionality of the
  \code{\link{surface}} and \code{\link{coco}} functions.
}

\usage{
  surface_coco(a, b, span, nodes, fdr = 0.01)
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
    A \code{charactor vector} of nodes.
  }
  \item{fdr}{
    The desired level at which to control the False Discovery Rate.
  }
}

\value{
  A \code{\link[data.table]{data.table}} of the form
  returned by the \code{\link{coco}} functions.
}

\details{
  See \code{\link{surface}} and \code{\link{coco}}.

  For an example of usage see the
  \sQuote{\href{../doc/proof_of_concept.pdf}{Proof of Concept}}
  vignette.
}

