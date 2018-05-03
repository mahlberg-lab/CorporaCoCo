\name{corp_text}
\alias{corp_text}
\alias{is.corp_text}
\alias{corp_type_lookup}
\alias{corp_type_lookup.corp_text}

\title{Tokenized text}
\description{
  Encapsulates the tokenization of a piece of text.
}

\usage{
  corp_text(text, tokens = NULL)

  is.corp_text(obj)

  \method{corp_type_lookup}{corp_text}(obj)
}

\arguments{
  \item{text}{
    A \code{character string} of the text which is the subject of the
    co-occurrence counting.
  }
  \item{tokens}{
    This is a \code{data.frame} containing \var{type}, \var{start}
    and \var{end} variables.
\preformatted{
    $ tokens:Classes 'data.frame': 3 variables:
     ..$ type : chr
     ..$ start: int
     ..$ end  : int
}
    \var{tokens} captures the types within the text along with their
    character positions. For example we could represent the
    types in the text \code{"Do cats eat bats?"} with the \var{tokens}
    \code{data.frame}:
\preformatted{
       type start end
    1:   do     1   2
    2: cats     4   7
    3:  eat     9  11
    4: bats    13  16
}

    If the \var{tokens} argument is not supplied, the \var{tokens}
    will be calculated from the supplied \var{text} argument. 
    The default behaviour is to tokenize on word boundaries according
    to the
    \href{http://www.unicode.org/reports/tr29/#Word_Boundaries}{Unicode Standard}
    with the \var{types} being the unique set of lowercased
    extracted words. 
    This is achieved using the \pkg{stringi}
    CRAN package and will work for any UTF-8 encoded text
    (in any language).
    For details of constructing your own \var{tokens} see details.
  }
  \item{obj}{
    A \code{corp_text} object as is returned by the
    \code{corp_text} function. 
  }    
}

\value{
  \subsection{corp_text}{
    Returns a \code{corp_text} object.
    
    The \code{corp_text} object can be interogated using the
    \code{corp_get_*} accessor functions.

    A concordance can be generated from the \code{corp_text} object
    using the \code{\link{corp_concordance}}) function.

    The \code{corp_text} objects are used as arguments to the
    \code{\link{corp_cooccurrence}}) function. 
  }
  \subsection{corp_type_lookup}{
    Returns a \pkg{data.table} that can be used to lookup the tokens
    associated with each type. See example.
  }
  \subsection{summary}{
    Prints a summary of the token and type counts for the \var{text}.
  }
}

\seealso{
    \code{\link{corp_cooccurrence}}) and
    \code{\link{corp_concordance}}).
}

\examples{
    x <- "A man, a plan, a canal -- Panama!"

    y <- corp_text(x)

    corp_get_tokens(y)

    ##      type start end  token idx
    ## 1:      a     1   1      A   1
    ## 2:    man     3   5    man   2
    ## 3:      a     8   8      a   3
    ## 4:   plan    10  13   plan   4
    ## 5:      a    16  16      a   5
    ## 6:  canal    18  22  canal   6
    ## 7: panama    27  32 Panama   7

    corp_get_text(y)

    ## [1] "A man, a plan, a canal -- Panama!"

    corp_type_lookup(y)

    ##      type tokens
    ## 1:      a   A, a
    ## 2:  canal  canal
    ## 3:    man    man
    ## 4: panama Panama
    ## 5:   plan   plan
}

