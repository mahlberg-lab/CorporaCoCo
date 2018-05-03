\name{corp_get_*}
\alias{corp_get_text}
\alias{corp_get_text.corp_text}
\alias{corp_get_text.corp_cooccurrence}
\alias{corp_get_text_obj}
\alias{corp_get_text_obj.corp_cooccurrence}
\alias{corp_get_tokens}
\alias{corp_get_tokens.corp_text}
\alias{corp_get_tokens.corp_cooccurrence}
\alias{corp_get_counts}
\alias{corp_get_counts.corp_cooccurrence}
\alias{corp_get_metadata}
\alias{corp_get_metadata.corp_text}
\alias{corp_get_metadata.corp_cooccurrence}
\alias{corp_get_metadata.corp_concordance}
\alias{corp_get_metadata.corp_coco}

\title{Accessors}
\description{
  Accessor methods for various \code{corp_* objects}.
}

\usage{
  \method{corp_get_text}{corp_text}(obj)
  \method{corp_get_text}{corp_cooccurrence}(obj)

  \method{corp_get_text_obj}{corp_cooccurrence}(obj)

  \method{corp_get_tokens}{corp_text}(obj)
  \method{corp_get_tokens}{corp_cooccurrence}(obj)

  \method{corp_get_metadata}{corp_text}(obj)
  \method{corp_get_metadata}{corp_cooccurrence}(obj)
  \method{corp_get_metadata}{corp_concordance}(obj)
  \method{corp_get_metadata}{corp_coco}(obj)

  \method{corp_get_counts}{corp_cooccurrence}(obj)
}

\arguments{
  \item{obj}{
    A \code{corp_*} object.
  }
}

\value{
  \subsection{corp_get_text}{
    Returns a \code{character string} of the text that the co-occurrence
    counts were calculated against.
    This comes from the \code{\link{corp_text}} object used to create
    the \code{corp_cooccurrence} object.
  }
  \subsection{corp_get_tokens}{
    Returns a \code{data.table} of the tokenization that the co-occurrence
    counts were calculated against.
    This comes from the \code{\link{corp_text}} object used to create
    the \code{corp_cooccurrence} object.
  }
  \subsection{corp_get_counts}{
    Returns a \pkg{data.table}
    containing the co-occurrence counts.
    Note that a \code{data.table} is also a \code{data.frame} so
    if the \code{data.table} library is not loaded the returned
    object will behave exactly as a \code{data.frame}; however,
    for large data sets there will be significant performance
    enhancement offered by exploiting \pkg{data.table} functionality.

    The \code{data.table} is of the form:
\preformatted{
    Classes ‘data.table’ and 'data.frame': ...
     $ x: chr
     $ y: chr
     $ H: int
     $ M: int
     - attr(*, "sorted")= chr  "x" "y"
     - attr(*, ".internal.selfref")=<externalptr>
}
    where \code{H} is the number of times \code{x} types
    co-occurs with \code{y} types (think \emph{H}its), and \code{M}
    is the number of times \code{x} types fail to co-occur with
    \code{y} types when they could have (think \emph{M}isses);
    hence \code{H + M} is the maximum number of times
    that \code{x} types can co-occur with \code{y} types.
  }
}

