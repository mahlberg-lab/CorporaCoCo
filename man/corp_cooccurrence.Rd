\name{corp_cooccurrence}
\alias{corp_cooccurrence}
\alias{is.corp_cooccurrence}
\alias{corp_surface}
\alias{is.corp_surface}
\alias{surface}

\title{Calculate Co-occurrence Counts}
\description{
  Calculates co-occurrence counts.
  For each co-occurrence the maximum possible number of co-occurrences
  is also calculated.
}

\usage{
  corp_surface(text, span, nodes = NULL, collocates = NULL)

  is.corp_cooccurrence(obj)
  is.corp_surface(obj)

  # deprecated
  surface(x, span, nodes = NULL, collocates = NULL)
}

\arguments{
  \item{text}{
    A \code{\link{corp_text}} object.
  }
  \item{span}{
    A character string defining the co-occurrence span.
    See details.
  }
  \item{nodes}{
    A \code{character vector} of node types or \code{character string}
    representing a single node type.
    If supplied, only co-occurrences for the specified node types will
    be calculated.
    If \var{nodes} is not supplied co-occurrences will be calculated for
    the set of all node types.
    Restricting \var{nodes} can
    significantly reduce memory usage and execution times.
  }
  \item{collocates}{
    A \code{character vector} of collocate types or \code{character string}
    representing a single collocate type.
    If supplied, only co-occurrences for the specified collocate types will
    be calculated.
    If \var{collocates} is not supplied, co-occurrences will be calculated for
    all collocate types with a non-zero co-occurrence count.
    Restricting \var{collocates} can
    significantly reduce execution times.
  }
  \item{obj}{
    A \code{corp_cooccurrence} object as is returned by the
    \code{corp_surface} function. 
  }    
  \item{x}{
    In the deprecated \code{surface} function \var{x} is a  vector
    of tokens.
    \var{x} is assumed to be an ordered vector of tokenized text.
    No processing will be applied to \var{x} prior to the co-occurrence
    count calculations.
  
  }
}

\value{
  \subsection{corp_surface}{
    Returns a \code{corp_surface} object.
    
    The \code{corp_surface} object can be interogated using the
    \code{corp_get_*} accessor functions.

    The \code{corp_surface} objects are used as arguments to the
    \code{\link{corp_coco}}) function. 
  }
}

\details{
  \subsection{Surface co-occurrence}{
    \sQuote{surface} co-occurrence is easiest to describe with an example.
    The following is a \code{span} of \code{'2LR'}, that is 2 to the left and
    2 to the right.
\preformatted{
    ("a", "man", "a", "plan", "a", "cat", "a", "canal", "panama")
          |___________|____|___________|
}
    In this example the node \dQuote{plan} would co-occur once each with
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

    For a detailed description of \sQuote{surface} co-occurrence see
    Evert (2008).

  }
  \subsection{Co-occurrence barriers}{ 
    \code{NA}s can be used to implement co-occurrence barriers
    eg if two \code{NA} characters are inserted
    into \var{x} at each sentence boundary then with \code{span = 2}
    co-occurrences will not happen across sentences.
    See Evert (2008) for detailed description of co-occurrence barriers.
  }
}

\seealso{
    \code{\link{corp_coco}}) and
    \code{\link{corp_concordance}}).
}

\examples{
    # =====================
    # surface co-occurrence
    # =====================

    x <- corp_text("A man, a plan, a canal -- Panama!")

    y <- corp_surface(x, span = "2R")
    corp_get_counts(y)

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
    y <- corp_surface(x, span = '2R', nodes = c("canal", "man", "plan"))
    corp_get_counts(y)

    ##         x      y H M
    ##  1: canal panama 1 0
    ##  2:   man      a 1 1
    ##  3:   man   plan 1 1
    ##  4:  plan      a 1 1
    ##  5:  plan  canal 1 1

    # filter on nodes and collocates
    y <- corp_surface(x, span = '2R', nodes = c("canal", "man", "plan"), collocates = c("panama", "a"))
    corp_get_counts(y)

    ##         x      y H M
    ##  1: canal panama 1 0
    ##  2:   man      a 1 1
    ##  3:  plan      a 1 1

    # co-occurrence barrier
    tokens_with_barrier <- data.frame(
         type =            c("a", "man", "a", "plan", NA, NA, "a", "canal", "panama"),
        start = as.integer(c( 1,   3,     8,   10,    NA, NA,  16,  18,      27)),
          end = as.integer(c( 1,   5,     8,   13,    NA, NA,  16,  22,      32)),
        stringsAsFactors = FALSE
    )
    x <- corp_text("A man, a plan, a canal -- Panama!", tokens = tokens_with_barrier)

    y <- corp_surface(x, span = '2R')
    corp_get_counts(y)

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

