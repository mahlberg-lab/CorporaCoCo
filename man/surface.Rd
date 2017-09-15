\name{surface}
\alias{surface}

\title{Calculate Surface Co-occurrence Counts}
\description{
  Calculates co-occurrence counts for the supplied vector.
  For each co-occurrence the maximum possible number of co-occurrences
  is also calculated.
}

\usage{
  surface(x, span, nodes = NULL, collocates = NULL)
}

\arguments{
  \item{x}{
    A vector. This is the subject of the co-occurrence counting.
    See details.
  }
  \item{span}{
    A character string defining the co-occurrence span.
    See details.
  }
  \item{nodes}{
    A \code{character vector} of nodes or \code{character string}
    representing a single \code{node}.
    The \var{nodes} essentially act as a filter on the \var{x} column
    of the returned data structure. Use of \code{nodes} will
    significantly reduce memory usage.
  }
  \item{collocates}{
    A \code{character vector} of collocates or \code{character string}
    representing a single collocate.
    The \var{collocates} essentially act as a filter on the \var{y} column
    of the returned data structure.
  }
}

\value{
  Returns a \code{\link[data.table]{data.table}} containing
  counts for all co-occurrences in \var{x}.
  Note that a \code{data.table} is also a \code{data.frame} so
  if the \code{data.table} library is not loaded the returned
  object will behave exactly as a \code{data.frame}; however,
  for large data sets there will be significant performance
  enhancement offered by exploiting \code{data.table} functionality.

  The returned object is of the form:
\preformatted{
    Classes ‘data.table’ and 'data.frame': ...
     $ x: chr
     $ y: chr
     $ H: int
     $ M: int
     - attr(*, "sorted")= chr  "x" "y"
     - attr(*, ".internal.selfref")=<externalptr> 
}
  where \code{H} is the number of times \code{x} co-occurs with \code{y}
  (think \emph{H}its), and \code{M} is the number of times \code{x}
  fails to co-occur with \code{y} when it could have (think
  \emph{M}isses); hence \code{H + M} is the maximum number of times
  that \code{x} could have co-occurred with \code{y}.
}

\details{
  \var{x} is assumed to be an ordered vector of tokenized text.
  No processing will be applied to \code{x} prior to the co-occurrence
  count calculations.

  \sQuote{surface} co-occurrence is easiest to describe with an example.
  The following is a \code{span} of \code{'2LR'}, that is 2 to the left and
  2 to the right.
\preformatted{
    ("a", "man", "a", "plan", "a", "cat", "a", "canal", "panama")
          |___________|____|___________|
}
  In this example the term \dQuote{plan} would co-occur once each with
  the collocates \dQuote{man} and \dQuote{cat}, and twice with the collocate
  \dQuote{a}.

  Other examples of \code{span}:

  \code{span = '1L2R'}
\preformatted{
    ("a", "man", "a", "plan", "a", "cat", "a", "canal", "panama")
                 |____|____|___________|
}

  \code{span = '2R'}
\preformatted{
    ("a", "man", "a", "plan", "a", "cat", "a", "canal", "panama")
                      |____|___________|
}

  \code{NA}s can be used to implement co-occurrence barriers
  eg if two \code{NA} characters are inserted
  into \var{x} at each sentence boundary then with \code{span = 2}
  co-occurrences will not happen across sentences.
  See Evert (2008) for detailed description of co-occurrence barriers.

  For a detailed description of \sQuote{surface} co-occurrence and the
  other types of co-occurrence see Evert (2008).

}

\examples{
    # =====================
    # surface co-occurrence
    # =====================

    x <- c("a", "man", "a", "plan", "a", "canal", "panama")
    surface(x, span = '2R')

    ##         x      y H M
    ##  1:     a      a 2 4
    ##  2:     a  canal 1 5
    ##  3:     a    man 1 5
    ##  4:     a panama 1 5
    ##  5:     a   plan 1 5
    ##  6: canal panama 1 0
    ##  7:   man      a 1 1
    ##  8:   man   plan 1 1
    ##  9:  plan      a 1 1
    ## 10:  plan  canal 1 1


    # filter on nodes
    surface(x, span = '2R', nodes = c("canal", "man", "plan"))

    ##         x      y H M
    ##  1: canal panama 1 0
    ##  2:   man      a 1 1
    ##  3:   man   plan 1 1
    ##  4:  plan      a 1 1
    ##  5:  plan  canal 1 1

    # filter on nodes and collocates
    surface(x, span = '2R', nodes = c("canal", "man", "plan"), collocates = c("panama", "a"))

    ##         x      y H M
    ##  1: canal panama 1 0
    ##  2:   man      a 1 1
    ##  3:  plan      a 1 1


    # co-occurrence barrier
    x <- c("a", "man", "a", "plan", NA, NA, "a", "canal", "panama")
    surface(x, span = '2R')

    #         x      y H M
    #  1:     a      a 1 4
    #  2:     a  canal 1 4
    #  3:     a    man 1 4
    #  4:     a panama 1 4
    #  5:     a   plan 1 4
    #  6: canal panama 1 0
    #  7:   man      a 1 1
    #  8:   man   plan 1 1
}

\references{
  S. Evert (2008) \emph{Corpora and collocations.}
  Corpus Linguistics: An International Handbook \bold{1212–1248}.
}

