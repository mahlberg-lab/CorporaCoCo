CorporaCoCo
===========

A set of functions used to compare co-occurrence between two corpora.

The package is the result of work by the [Centre for Corpus Research](http://www.birmingham.ac.uk/research/activity/corpus/) at the University of Birmingham.  There is a paper in preparation but for now a good place to get an idea of what is going on is the 'Proof of Concept' document.

A very simple example of usage
------------------------------

This example takes the two Dickens novels 'Great Expectations' and 'A Tale of Two Cities' and compares the co-occurrences of a set of body part nouns. The idea is that since body part nouns are common in speech suspensions the statistically significant co-occurrence differences should include personal pronouns reflecting the differing narrative voices of the texts.

    library(CorporaCoCo)
    library(CorporaCorpus)
    library(stringi)

    GE  <- unlist( stri_extract_all_words( stri_trans_tolower( readLines(corpus_filepaths('DNov', 'GE')) ) ))
    TTC  <- unlist( stri_extract_all_words( stri_trans_tolower( readLines(corpus_filepaths('DNov', 'TTC')) ) ))

    nodes <- c('back', 'eye', 'eyes', 'forehead', 'hand', 'hands', 'head', 'shoulder')

    results <- surface_coco(TTC, GE, span = '5LR', nodes = nodes, fdr = 0.01)
    results

    ##        x   y H_A  M_A H_B  M_B effect_size  CI_lower   CI_upper      p_value   p_adjusted
    ##  1: back  me   3 1316  48 2355    3.159998  1.521928  5.4917238 9.754793e-07 9.423130e-04
    ##  2: back  my   1 1318  31 2372    4.105901  1.517363  9.4521419 1.987134e-05 9.597855e-03
    ##  3: eyes   i  10 1611  52 1724    2.280107  1.281850  3.4267531 2.247538e-07 6.869976e-05
    ##  4: eyes  my   5 1616  58 1718    3.446625  2.137003  5.1270592 1.061195e-11 9.731159e-09
    ##  5: eyes the 120 1501  57 1719   -1.269288 -1.761782 -0.7909003 4.323172e-08 1.982175e-05
    ##  6: hand his 175 2267 114 2543   -0.783898 -1.147324 -0.4250235 1.158348e-05 4.413307e-03
    ##  7: hand   i  17 2425  74 2583    2.030509  1.250655  2.8889719 7.519299e-09 4.297280e-06
    ##  8: hand  my  12 2430  85 2572    2.742060  1.858321  3.7535208 1.043073e-13 1.192232e-10
    ##  9: head  my   9 1732  62 2219    2.426331  1.404175  3.6251454 3.575486e-08 3.822194e-05

    plot(results)

The results are easier to see if you plot them.

Installing from CRAN
====================

In an R session type

    install.packages('CorporaCoCo')

Installing the latest development version directly from GitHub
==============================================================

Linux
-----

In an R session type:

    pkg_file <- tempfile()
    download.file(url = 'https://github.com/birmingham-ccr/CorporaCoCo/archive/master.tar.gz', mode = 'wb', method = 'wget', destfile = pkg_file)
    install.packages(pkg_file, repos = NULL, type = 'source')

Mac OSX / Windows
-----------------

``download.file`` may not support fetching ``https`` URLs. Alternatively, you
can use the the CRAN package [downloader](https://CRAN.R-project.org/package=downloader)
to fetch the archive instead:

    # install.packages("downloader")
    pkg_file <- tempfile()
    downloader::download(url = 'https://github.com/birmingham-ccr/CorporaCoCo/archive/master.tar.gz', mode = 'wb', destfile = pkg_file)
    install.packages(pkg_file, repos = NULL, type = 'source')

Alternatively use the `devtools` CRAN package
---------------------------------------------

If you have the CRAN package [devtools](https://CRAN.R-project.org/package=devtools)
you can use this to install directly from github:

    # install.packages("devtools")
    devtools::install_github("birmingham-ccr/CorporaCoCo")

