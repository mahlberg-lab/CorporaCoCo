\name{coco}
\alias{coco}

\title{Co-occurrence comparison}
\description{
  Calculates statistically significant difference in co-occurrence counts.
}

\usage{
  coco(A, B, nodes, fdr = 0.01)
}

\arguments{
  \item{A}{
    A \code{data.frame} of co-occurrence counts.
    See details.
  }
  \item{B}{
    A \code{data.frame} of co-occurrence counts.
    See details.
  }
  \item{nodes}{
    A \code{charactor vector} of nodes or \code{charactor string}
    representing a single \code{node}.
  }
  \item{fdr}{
    The desired level at which to control the False Discovery Rate.
    Default value is \code{0.01}.
  }
}

\value{
  A \code{\link[data.table]{data.table}} of the form
\preformatted{
    Classes ‘data.table’ and 'data.frame': 11 variables:
     $ x           : chr
     $ y           : chr
     $ H_A         : int
     $ M_A         : int
     $ H_B         : int
     $ M_B         : int
     $ effect_size : num
     $ CI_lower    : num
     $ CI_upper    : num
     $ p_value     : num
     $ p_adjusted  : num
     - attr(*, "sorted")= chr  "x" "y"
     - attr(*, ".internal.selfref")=<externalptr> 
     - attr(*, "coco_metadata")=List of 4
      ..$ nodes : chr
      ..$ fdr       : num
      ..$ PACKAGE_VERSION:Classes 'package_version', 'numeric_version'
      .. ..$ : int
      ..$ date      : Date, format: "2016-11-01"
}
}

\details{
  This function implements the method described in
  Hennessey and Wiegand (2017).

  \var{A} and \var{B} are \code{data.frame}s of the form
\preformatted{
    Classes 'data.frame': ...
     $ x: chr  
     $ y: chr  
     $ H: int  
     $ M: int  
}
  The \code{data.frame}s encapsulate the co-occurrence counts for the
  \code{(x, y)} term pairs within a corpus. For a description of the
  columns see the details section of the \code{\link{surface}} function.

  The \var{nodes} essentially act as a filter on the \var{A$x} and 
  \var{B$x} columns. For a description of the use of nodes see
  Hennessey and Wiegand (2017).

  \var{fdr} indicates the level at which the False Discovery Rate will be
  controlled. For a description of the form of FDR used see
  Benjamini and Hochberg (1995). For a description of the use of FDR in
  this context see Hennessey and Wiegand (2017). For description of the
  \var{p_adjusted} column in the returned structure see
  \code{\link{p.adjust}}.

  The returned data structure is a \code{\link[data.table]{data.table}}.
  A \code{data.table} is also a \code{data.frame} and will behave exactly
  as such if the \code{data.table} library is not loaded.

  The returned \code{data.table} contains details of all the
  co-occurrences for which there is evidence of a difference in 
  co-occurrence between the two supplied data sets.
  The effect size is calculated as the log base 2 of the odds ratio.
  The effects size and its confidence interval are captured in the
  \var{effect_size}, \var{CI_lower} and \var{CI_upper} columns.
  The \var{p_value} column contains the non-adjusted p-value from the
  Fisher's Exact Test.
  For more details see Hennessey and Wiegand (2017).

  For an example of usage see the
  \sQuote{\href{../doc/proof_of_concept.pdf}{Proof of Concept}}
  vignette.

}


\references{
  Y. Benjamini and Y. Hochberg (1995) \emph{Controlling the False Discovery
  Rate: A Practical and Powerful Approach to Multiple Testing.}
  Journal of the Royal Statistical Society. Series B (Methodological)
  \bold{57 (1)289–300}.

  A. Hennessey and V. Wiegand and C. R. Tench and M. Mahlberg (2017)
  \emph{Comparing co-occurrences between corpora.} In preparation.
}

