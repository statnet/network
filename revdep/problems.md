# bingat

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/bingat
* Date/Publication: 2017-07-05 18:30:37 UTC
* Number of recursive dependencies: 32

Run `revdep_details(, "bingat")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘bingat’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘genalg’
    ```

# discourseGT

<details>

* Version: 1.1.5
* GitHub: NA
* Source code: https://github.com/cran/discourseGT
* Date/Publication: 2021-04-02 19:00:02 UTC
* Number of recursive dependencies: 105

Run `revdep_details(, "discourseGT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘discourseGT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot1Att
    > ### Title: Plots Graphs using ggplot2 with one attribute
    > ### Aliases: plot1Att
    > 
    > ### ** Examples
    > 
    > df <- sampleData1
    ...
    > plot1Att(baseNet, prop = 20, graphmode = "fruchtermanreingold",
    + attribute = attdata$gender,
    + attribute.label = "Gender",
    + attribute.node.labels = attdata$node, attribute.nodesize = 12)
    Error: `loops` is `FALSE`, but `x` contains loops.
    The following values are affected:
    	- `x[1, 1:2]`
    	- `x[10, 1:2]`
    	- `x[15, 1:2]`
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocManager’ ‘ggpubr’ ‘sna’
      All declared Imports should be used.
    ```

# EloRating

<details>

* Version: 0.46.11
* GitHub: https://github.com/gobbios/EloRating
* Source code: https://github.com/cran/EloRating
* Date/Publication: 2020-03-12 16:10:14 UTC
* Number of recursive dependencies: 66

Run `revdep_details(, "EloRating")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: zoo
    
    Attaching package: ‘zoo’
    
    The following objects are masked from ‘package:base’:
    
        as.Date, as.Date.numeric
    
    Loading required package: sna
    Loading required package: statnet.common
    Error: package or namespace load failed for ‘statnet.common’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘statnet.common’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

# geomnet

<details>

* Version: 0.3.1
* GitHub: https://github.com/sctyner/geomnet
* Source code: https://github.com/cran/geomnet
* Date/Publication: 2020-11-26 11:00:06 UTC
* Number of recursive dependencies: 87

Run `revdep_details(, "geomnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geomnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: football
    > ### Title: College football games network (undirected)
    > ### Aliases: football
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    +   scale_colour_brewer("Conference", palette = "Paired")
    Error: `loops` is `FALSE`, but `x` contains loops.
    The following values are affected:
    	- `x[604, 1:2]`
    	- `x[605, 1:2]`
    	- `x[610, 1:2]`
    	- `x[615, 1:2]`
    	- `x[617, 1:2]`
    	- `x[618, 1:2]`
    Execution halted
    ```

# GGally

<details>

* Version: 2.1.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2021-03-08 06:00:03 UTC
* Number of recursive dependencies: 136

Run `revdep_details(, "GGally")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       4.     └─network:::.validate_edge_df(...)
      ── Error (test-ggnetworkmap.R:34:1): (code run outside of `test_that()`) ───────
      Error: `loops` is `FALSE`, but `x` contains loops.
      The following values are affected:
      	- `x[114, 1:2]`
      Backtrace:
          █
       1. └─network::network(flights, directed = TRUE) test-ggnetworkmap.R:34:0
       2.   ├─network::as.network(...)
       3.   └─network::as.network.data.frame(...)
       4.     └─network:::.validate_edge_df(...)
      
      [ FAIL 3 | WARN 1330 | SKIP 3 | PASS 563 ]
      Error: Test failures
      Execution halted
    ```

# GOxploreR

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/GOxploreR
* Date/Publication: 2020-11-20 09:30:03 UTC
* Number of recursive dependencies: 91

Run `revdep_details(, "GOxploreR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GOxploreR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: visRsubDAGBP
    > ### Title: Visualise the GO-DAG for a specific organism based on certain
    > ###   GO-terms
    > ### Aliases: visRsubDAGBP
    > 
    > ### ** Examples
    > 
    ...
      3. │   └─node.data1 %>% mutate(label1 = p1) %>% arrange(id)
      4. ├─dplyr::arrange(., id)
      5. ├─dplyr::mutate(., label1 = p1)
      6. ├─dplyr:::mutate.data.frame(., label1 = p1)
      7. │ └─dplyr:::mutate_cols(.data, ...)
      8. │   ├─base::withCallingHandlers(...)
      9. │   └─mask$eval_all_mutate(quo)
     10. └─dplyr:::abort_glue(character(0), list(x_size = 35L), "dplyr:::mutate_incompatible_size")
     11.   └─rlang::exec(abort, class = class, !!!data)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        R   6.7Mb
    ```

# haplotypes

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/haplotypes
* Date/Publication: 2020-02-28 13:50:06 UTC
* Number of recursive dependencies: 28

Run `revdep_details(, "haplotypes")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘haplotypes’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# ICON

<details>

* Version: 0.4.0
* GitHub: https://github.com/rrrlw/ICON
* Source code: https://github.com/cran/ICON
* Date/Publication: 2020-10-23 23:00:02 UTC
* Number of recursive dependencies: 75

Run `revdep_details(, "ICON")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘ICON’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# mpa

<details>

* Version: 0.7.3
* GitHub: NA
* Source code: https://github.com/cran/mpa
* Date/Publication: 2012-10-29 08:59:13
* Number of recursive dependencies: 14

Run `revdep_details(, "mpa")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: network
    Error: package or namespace load failed for ‘network’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘network’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    NB: .First.lib is obsolete and will not be used in R >= 3.0.0
    File ‘mpa/R/zzz.R’:
      .First.lib calls:
        cat("\n mpa: CoWords Method")
        cat("\n ")
        cat("\n Universidad Nacional de Colombia")
        cat("\n ")
        cat("\n Elaborado por: Daniel Rodriguez <dhrodriguezt@unal.edu.co>")
        cat("\n                Campo Elias Pardo <cepardot@unal.edu.co>")
        cat("\n For help: ?mpa   \n\n")
    ...
      generate messages.
    See section ‘Good practice’ in '?.onAttach'.
    
    diagram.mpa: no visible global function definition for ‘abline’
    diagram.mpa: no visible global function definition for ‘text’
    Undefined global functions or variables:
      abline text
    Consider adding
      importFrom("graphics", "abline", "text")
    to your NAMESPACE file.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# netdiffuseR

<details>

* Version: 1.22.0
* GitHub: https://github.com/USCCANA/netdiffuseR
* Source code: https://github.com/cran/netdiffuseR
* Date/Publication: 2020-05-07 13:00:06 UTC
* Number of recursive dependencies: 69

Run `revdep_details(, "netdiffuseR")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘netdiffuseR’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.0Mb
      sub-directories of 1Mb or more:
        doc    2.3Mb
        libs  11.8Mb
    ```

# nethet

<details>

* Version: 1.22.0
* GitHub: NA
* Source code: https://github.com/cran/nethet
* Date/Publication: 2020-10-27
* Number of recursive dependencies: 95

Run `revdep_details(, "nethet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(nethet)
      Warning messages:
      1: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame' when loading 'dplyr' 
      2: replacing previous import 'mclust::dmvnorm' by 'mvtnorm::dmvnorm' when loading 'nethet' 
      3: replacing previous import 'multtest::update' by 'stats::update' when loading 'nethet' 
      > 
      > test_check("nethet")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (???): Correct means ────────────────────────────────────────────────
      sum(abs(full.result$Mu - Mu)) is not strictly less than 0.1 * p * n.comps. Difference: 0.223
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 21 ]
      Error: Test failures
      Execution halted
    ```

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘nethet’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    In addition: Warning message:
    replacing previous import ‘mclust::dmvnorm’ by ‘mvtnorm::dmvnorm’ when loading ‘nethet’ 
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking whether package ‘nethet’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘mclust::dmvnorm’ by ‘mvtnorm::dmvnorm’ when loading ‘nethet’
      Warning: replacing previous import ‘multtest::update’ by ‘stats::update’ when loading ‘nethet’
    See ‘/homes/morrism/GitHub/StatnetOrganization/network/revdep/checks/nethet/new/nethet.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    screen_shrink: no visible global function definition for
      ‘performance.pcor’
    Undefined global functions or variables:
      performance.pcor
    ```

# networkABC

<details>

* Version: 0.8-0
* GitHub: https://github.com/fbertran/networkABC
* Source code: https://github.com/cran/networkABC
* Date/Publication: 2021-03-20 21:30:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(, "networkABC")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘networkABC’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# NetworkDistance

<details>

* Version: 0.3.3
* GitHub: NA
* Source code: https://github.com/cran/NetworkDistance
* Date/Publication: 2020-05-08 09:30:12 UTC
* Number of recursive dependencies: 45

Run `revdep_details(, "NetworkDistance")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘NetworkDistance’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# networkDynamic

<details>

* Version: 0.10.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamic
* Date/Publication: 2020-01-21 09:50:02 UTC
* Number of recursive dependencies: 36

Run `revdep_details(, "networkDynamic")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: network
    Error: package or namespace load failed for ‘network’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘network’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    get.edge.attribute:
      function(x, ..., el)
    get.edge.attribute.active:
      function(x, prefix, onset, terminus, length, at, rule,
               active.default, dynamic.only, require.active, return.tea,
               unlist)
    
    get.edge.value:
      function(x, ...)
    get.edge.value.active:
    ...
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    
    Found the following apparent S3 methods exported but not registered:
      get.edge.attribute.active get.edge.value.active
      get.network.attribute.active list.edge.attributes.active
      list.network.attributes.active
    See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'attribute.activity.functions':
      ‘get.edge.value.active’ ‘get.network.attribute.active’
      ‘list.edge.attributes.active’ ‘list.network.attributes.active’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# networkDynamicData

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamicData
* Date/Publication: 2016-01-12 00:23:27
* Number of recursive dependencies: 19

Run `revdep_details(, "networkDynamicData")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: networkDynamic
    Loading required package: network
    Error: package or namespace load failed for ‘network’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘network’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# networksis

<details>

* Version: 2.1-3
* GitHub: NA
* Source code: https://github.com/cran/networksis
* Date/Publication: 2015-04-16 08:44:42
* Number of recursive dependencies: 14

Run `revdep_details(, "networksis")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: network
    Error: package or namespace load failed for ‘network’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘network’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    simulate.sisnetwork: no visible global function definition for ‘var’
    Undefined global functions or variables:
      var
    Consider adding
      importFrom("stats", "var")
    to your NAMESPACE file.
    ```

# PAFit

<details>

* Version: 1.0.1.8
* GitHub: https://github.com/thongphamthe/PAFit
* Source code: https://github.com/cran/PAFit
* Date/Publication: 2020-02-17 18:00:15 UTC
* Number of recursive dependencies: 50

Run `revdep_details(, "PAFit")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘PAFit’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking whether package ‘PAFit’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/homes/morrism/GitHub/StatnetOrganization/network/revdep/checks/PAFit/new/PAFit.Rcheck/00install.out’ for details.
    ```

# PDN

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PDN
* Date/Publication: 2017-11-03 23:27:43 UTC
* Number of recursive dependencies: 52

Run `revdep_details(, "PDN")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘PDN’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

# sna

<details>

* Version: 2.6
* GitHub: NA
* Source code: https://github.com/cran/sna
* Date/Publication: 2020-10-06 08:50:03 UTC
* Number of recursive dependencies: 62

Run `revdep_details(, "sna")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Loading required package: statnet.common
    Error: package or namespace load failed for ‘statnet.common’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Error: package ‘statnet.common’ could not be loaded
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

# SparseTSCGM

<details>

* Version: 4.0
* GitHub: NA
* Source code: https://github.com/cran/SparseTSCGM
* Date/Publication: 2021-01-13 11:10:08 UTC
* Number of recursive dependencies: 26

Run `revdep_details(, "SparseTSCGM")` for more info

</details>

## Newly broken

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘SparseTSCGM’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called ‘purrr’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

