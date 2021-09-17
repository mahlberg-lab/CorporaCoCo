[![R-CMD-check](https://github.com/mahlberg-lab/CorporaCoCo/workflows/R-CMD-check/badge.svg)](https://github.com/mahlberg-lab/CorporaCoCo/actions)
[![CRAN version badge](https://img.shields.io/cran/v/CorporaCoCo.svg)](https://cran.r-project.org/package=CorporaCoCo)
[![codecov](https://codecov.io/gh/mahlberg-lab/CorporaCoCo/branch/master/graph/badge.svg)](https://codecov.io/gh/mahlberg-lab/CorporaCoCo)
[![CRAN RStudio mirror total downloads badge](https://cranlogs.r-pkg.org/badges/grand-total/CorporaCoCo?color=001577)](https://cran.r-project.org/package=CorporaCoCo)
[![CRAN RStudio mirror monthly downloads badge](https://cranlogs.r-pkg.org/badges/CorporaCoCo?color=001577)](https://cran.r-project.org/package=CorporaCoCo)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1174881.svg)](https://doi.org/10.5281/zenodo.1174881)
  
CorporaCoCo
===========

The package implements the method introduced in Wiegand and Hennessey et al. (2017a). It identifies significant difference in co-occurrence counts for a given node or set of nodes across two corpora, using a Fisher’s Exact test.

A good place to start is the ‘Introduction to CorporaCoCo’ vignette. You can open the vignette with `vignette("intro", package = "CorporaCoCo")`. For a list of all documentation use `library(help="CorporaCoCo")`. For updates on development versions of the package and documentation, please watch this GitHub page.

References
----------
* Wiegand, V., Hennessey, A., Tench, C. R., & Mahlberg, M. (2017a, May 24). *Comparing co-occurrences between corpora*. 38th ICAME conference, Charles University, Prague.

*	Wiegand, V., Hennessey, A., Tench, C. R., & Mahlberg, M. (2017b, July 24). *A cookbook of co-occurrence comparison techniques and how they relate to the subtleties in your research question*. 9th International Corpus Linguistics Conference, University of Birmingham, Birmingham.


A very simple example of usage
------------------------------

This example takes the two Dickens novels 'Great Expectations' and 'A Tale of Two Cities' and compares the co-occurrences of a set of body part nouns. The idea is that since body part nouns are common in suspensions the statistically significant co-occurrence differences should include personal pronouns reflecting the differing narrative voices of the texts. We use the texts here only as sample data; we retrieve them from the [CLiC API](https://clic.readthedocs.io/en/latest/advanced/api_usage.html) using the clicclient package (the other functions of that package are still under development; please see the [clicclient GitHub page for details](https://github.com/mahlberg-lab/clicclient)). 

    #devtools::install_github("mahlberg-lab/clicclient")
    library(clicclient)
    
    # retrieve texts for 'A Tale of two Cities' (TTC) and 'Great Expectations' from the CLiC corpora using the 'clicclient' package
    TTC <- clic_texts("TTC")
    GE <- clic_texts("GE")

    # tokenize / create corp_text objects
    TTC_text <- corp_text(TTC)
    GE_text <- corp_text(GE)

    # count co-occurrences / create corp_surface objects
    TTC_cooccurs <- corp_surface(TTC_text, span = "5LR")
    GE_cooccurs <- corp_surface(GE_text, span = "5LR")

     # set the body part nodes
    nodes <- c('back', 'eye', 'eyes', 'forehead', 'hand', 'hands', 'head', 'shoulder')
  
    # run co-occurrence comparison with corp_coco
    results <- corp_coco(TTC_cooccurs, GE_cooccurs, nodes = nodes, fdr = 0.01)
    
    results

    ##          x   y H_A  M_A H_B  M_B effect_size  CI_lower   CI_upper      p_value   p_adjusted
    ##   1:  back  me   3 1347  49 2391   3.2014513  1.565327  5.5296252 5.771195e-07 5.638457e-04
    ##   2:  back  my   1 1349  31 2409   4.1171190  1.528647  9.4632893 1.931898e-05 9.437321e-03
    ##   3:  eyes   i  10 1640  53 1747   2.3142117  1.318110  3.4597915 1.320717e-07 6.114918e-05
    ##   4:  eyes joe   0 1650  16 1784         Inf  1.831254        Inf 3.641000e-05 7.000398e-03
    ##   5:  eyes  me   3 1647  25 1775   2.9502767  1.233703  5.3219224 3.779912e-05 7.000398e-03
    ##   6:  eyes  my   5 1645  58 1742   3.4522796  2.142760  5.1326458 1.058629e-11 9.802907e-09
    ##   7:  eyes the 122 1528  62 1738  -1.1620034 -1.638598 -0.6973805 2.839285e-07 8.763925e-05
    ##   8:  hand his 175 2315 114 2586  -0.7778652 -1.140960 -0.4192322 1.183505e-05 4.540716e-03
    ##   9:  hand   i  19 2471  75 2625   1.8932508  1.146902  2.7069140 2.704759e-08 1.556589e-05
    ##  10:  hand  my  12 2478  86 2614   2.7637906  1.880930  3.7755453 5.884107e-14 6.772608e-11
    ##  11: hands  my   5 1125  45 1775   2.5113109  1.177037  4.2063750 1.127123e-05 9.321308e-03
    ##  12:  head  my  10 1760  62 2258   2.2723508  1.292764  3.4061853 1.024855e-07 1.111968e-04
    
    plot(results)

![Plot of example results.](tools/readme_image_01.png)

Further examples of how the method has been used can be found in:

* Mahlberg, M., Wiegand, V., & Hennessey, A. (2020). Eye language – body part collocations and textual contexts in the nineteenth-century novel. In L. Fesenmeier & I. Novakova (Eds.), *Phraseology and Stylistics of Literary Language/Phraséologie et Stylistique de la Langue Littéraire* (pp. 143–176). Peter Lang. https://www.academia.edu/45152494/Eye_language_body_part_collocations_and_textual_contexts_in_the_nineteenth_century_novel

* Wiegand, V. (2019). *A Corpus Linguistic Approach to Meaning-Making Patterns in Surveillance Discourse* [PhD, University of Birmingham]. https://etheses.bham.ac.uk//id/eprint/9778/

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
    download.file(url = 'https://github.com/mahlberg-lab/CorporaCoCo/archive/master.tar.gz', mode = 'wb', method = 'wget', destfile = pkg_file)
    install.packages(pkg_file, repos = NULL, type = 'source')

Mac OSX / Windows
-----------------

``download.file`` may not support fetching ``https`` URLs. Alternatively, you
can use the the CRAN package [downloader](https://CRAN.R-project.org/package=downloader)
to fetch the archive instead:

    # install.packages("downloader")
    pkg_file <- tempfile()
    downloader::download(url = 'https://github.com/mahlberg-lab/CorporaCoCo/archive/master.tar.gz', mode = 'wb', destfile = pkg_file)
    install.packages(pkg_file, repos = NULL, type = 'source')

Alternatively use the `devtools` CRAN package
---------------------------------------------

If you have the CRAN package [devtools](https://CRAN.R-project.org/package=devtools)
you can use this to install directly from github:

    # install.packages("devtools")
    devtools::install_github("mahlberg-lab/CorporaCoCo")

Testing
=======

Unit tests are located in the /tests/testthat directory. We use the 'testthat' package to generate tests.

To run the tests yourself, just do:

```
devtools::test()
ℹ Loading CorporaCoCo
ℹ Testing CorporaCoCo
✔ |  OK F W S | Context
✔ |  39       | coco [0.5 s]
✔ |   4       | corp_concordance [0.1 s]
✔ |  10       | corp_cooccurrence
✔ |  12       | corp_text
✔ |   2       | surface_coco [0.1 s]
✔ |  44       | surface [0.3 s]
```

══ Results ══════════════════════════════════

Duration: 1.2 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 111 ]


Continuous integration testing is set up using GitHub Actions - see [the workflows](/.github/workflows).

