\name{corp_concordance}
\alias{corp_concordance}
\alias{corp_concordance.corp_surface}
\alias{print.corp_concordance}

\title{Concordance}
\description{
  Concordances from \code{\link{corp_cooccurrence}} objects.
}

\usage{
  corp_concordance(obj, span, nodes, collocates, context)

  \method{print}{corp_concordance}(x, collocates = attr(x, "collocates"), collocate_marker = "*", as_data_table = FALSE, \dots)
}

\arguments{
  \item{obj}{
    A \code{\link{corp_cooccurrence}} object.
    co-occurrence counting.
  }
  \item{span}{
    A character string defining the co-occurrence span.
    See details section in \code{\link{corp_surface}}.
    Defaults to the \var{span} used to create the \code{corp_cooccurrence}
    object.
  }

  \item{nodes}{
    A \code{character vector} of node types or \code{character string}
    representing a single node type.
    If supplied, only concordance lines for the specified node types will
    be displayed.
    Defaults to the \var{nodes} used to create the \code{corp_cooccurrence}
    object.
  }
  \item{collocates}{
    A \code{character vector} of collocate types or \code{character string}
    representing a single collocate type.
    If supplied, only concordances for the specified collocate types will
    be selected.
    Defaults to the \var{collocates} used to create the \code{corp_cooccurrence}
    object.

    For \code{print} \var{collocates} is used to determine which collocate
    types to highlight; and does not act as a filter.
    Defaults to the collocates passed to the \code{corp_concordance}
    dunction.
  }
  \item{context}{
    The number of contect tokens to be displayed.
    Default is 3.
  }
  \item{x}{
    A \code{corp_concordance} object.
  }
  \item{collocate_marker}{
    The characters used to highlight the highlighted collocates.
    Default is '*'.
  }
  \item{as_data_table}{
    Print the internal \code{\link{data.table}} object. 
  }
  \item{\dots}{
    Ignored.
  }
}

\value{
  Returns a \code{corp_concordance} object.
}

