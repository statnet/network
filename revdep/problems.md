# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 74

Run `revdep_details(, "BasketballAnalyzeR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘hexbin’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# BGGM

<details>

* Version: 2.0.3
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2020-12-03 08:20:06 UTC
* Number of recursive dependencies: 165

Run `revdep_details(, "BGGM")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.5Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        help   1.2Mb
        libs   8.0Mb
    ```

# bingat

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/bingat
* Date/Publication: 2017-07-05 18:30:37 UTC
* Number of recursive dependencies: 34

Run `revdep_details(, "bingat")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘genalg’
    ```

# Blaunet

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/Blaunet
* Date/Publication: 2020-05-22 08:10:11 UTC
* Number of recursive dependencies: 85

Run `revdep_details(, "Blaunet")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘Blaunet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: blau
    > ### Title: Converts raw data into an object for Blau status analysis
    > ### Aliases: blau
    > 
    > ### ** Examples
    > 
    > ##simple example
    ...
    > ##example with relational data
    > data(BSANet)
    > square.data <- BSANet$square.data
    > el <- BSANet$el
    > 
    > b <- blau(square.data, node.ids = 'person', ecology.ids = 'city', graph = el)
    Error in Ops.factor(sources, targets) : 
      level sets of factors are different
    Calls: blau ... as.network.data.frame -> .validate_edge_df -> which -> Ops.factor
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'gWidgets', 'gWidgetsRGtk2'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RGtk2’ ‘cairoDevice’ ‘ergm’ ‘foreign’ ‘haven’ ‘plot3D’ ‘plot3Drgl’
      ‘rgl’ ‘sna’ ‘statnet.common’
      All declared Imports should be used.
    ```

# bootcluster

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/bootcluster
* Date/Publication: 2021-06-10 08:20:04 UTC
* Number of recursive dependencies: 69

Run `revdep_details(, "bootcluster")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# broom

<details>

* Version: 0.7.7
* GitHub: https://github.com/tidymodels/broom
* Source code: https://github.com/cran/broom
* Date/Publication: 2021-06-13 04:40:17 UTC
* Number of recursive dependencies: 295

Run `revdep_details(, "broom")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'rgeos', 'spdep', 'spatialreg'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘spatialreg’
    ```

# CEMiTool

<details>

* Version: 1.16.0
* GitHub: NA
* Source code: https://github.com/cran/CEMiTool
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 183

Run `revdep_details(, "CEMiTool")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.5Mb
      sub-directories of 1Mb or more:
        data      4.8Mb
        doc       4.0Mb
        extdata   4.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    flip_vector: no visible global function definition for ‘setNames’
    select_genes: no visible global function definition for ‘var’
    get_hubs,CEMiTool : <anonymous>: no visible global function definition
      for ‘head’
    get_merged_mods,CEMiTool: no visible global function definition for
      ‘as.dist’
    get_mods,CEMiTool: no visible global function definition for ‘as.dist’
    get_phi,CEMiTool: no visible global function definition for ‘tail’
    get_phi,CEMiTool: no visible global function definition for ‘head’
    mod_gene_num,CEMiTool: no visible binding for global variable ‘modules’
    ...
    save_plots,CEMiTool: no visible global function definition for
      ‘dev.off’
    Undefined global functions or variables:
      ..eq.label.. ..rr.label.. := Mean Variance as.dist data dev.off dist
      head modules num_genes setNames tail var
    Consider adding
      importFrom("grDevices", "dev.off")
      importFrom("stats", "as.dist", "dist", "setNames", "var")
      importFrom("utils", "data", "head", "tail")
    to your NAMESPACE file.
    ```

# CeTF

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/CeTF
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 244

Run `revdep_details(, "CeTF")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘CeTF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: netConditionsPlot
    > ### Title: Network plot of gene-gene/gene-TFs interactions
    > ### Aliases: netConditionsPlot
    > 
    > ### ** Examples
    > 
    > # loading a simulated counts data
    ...
    Trios for gene    20
    Trios for gene    30
    Trios for gene    40
    > 
    > # plotting networks conditions
    > netConditionsPlot(out)
    Error in set.vertex.attribute.network(out, attrname = "vertex.names",  : 
      Inappropriate value given in set.vertex.attribute.
    Calls: netConditionsPlot ... set.vertex.attribute -> set.vertex.attribute.network
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   1.7Mb
        doc    4.0Mb
    ```

# chorrrds

<details>

* Version: 0.1.9.5
* GitHub: https://github.com/r-music/chorrrds
* Source code: https://github.com/cran/chorrrds
* Date/Publication: 2020-06-30 17:30:02 UTC
* Number of recursive dependencies: 73

Run `revdep_details(, "chorrrds")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forcats’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4004 marked UTF-8 strings
    ```

# CINNA

<details>

* Version: 1.1.54
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2021-01-28 15:00:02 UTC
* Number of recursive dependencies: 135

Run `revdep_details(, "CINNA")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘circlize’
      All declared Imports should be used.
    ```

# CluMSID

<details>

* Version: 1.8.0
* GitHub: https://github.com/tdepke/CluMSID
* Source code: https://github.com/cran/CluMSID
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 168

Run `revdep_details(, "CluMSID")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
    ```

# discourseGT

<details>

* Version: 1.1.5
* GitHub: NA
* Source code: https://github.com/cran/discourseGT
* Date/Publication: 2021-04-02 19:00:02 UTC
* Number of recursive dependencies: 103

Run `revdep_details(, "discourseGT")` for more info

</details>

## In both

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

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocManager’ ‘ggpubr’ ‘sna’
      All declared Imports should be used.
    ```

# EGAnet

<details>

* Version: 0.9.8
* GitHub: NA
* Source code: https://github.com/cran/EGAnet
* Date/Publication: 2021-02-16 18:10:06 UTC
* Number of recursive dependencies: 206

Run `revdep_details(, "EGAnet")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
    ```

# ergm

<details>

* Version: 3.11.0
* GitHub: https://github.com/statnet/ergm
* Source code: https://github.com/cran/ergm
* Date/Publication: 2020-10-14 09:30:02 UTC
* Number of recursive dependencies: 69

Run `revdep_details(, "ergm")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        help   1.5Mb
        libs   3.9Mb
    ```

# ergMargins

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/ergMargins
* Date/Publication: 2021-02-23 14:50:05 UTC
* Number of recursive dependencies: 75

Run `revdep_details(, "ergMargins")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘btergm’ ‘methods’ ‘sna’ ‘statnet’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘margins’
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ergmito

<details>

* Version: 0.3-0
* GitHub: https://github.com/muriteams/ergmito
* Source code: https://github.com/cran/ergmito
* Date/Publication: 2020-08-10 21:40:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(, "ergmito")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        libs   7.9Mb
    ```

# fcoex

<details>

* Version: 1.6.0
* GitHub: NA
* Source code: https://github.com/cran/fcoex
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 280

Run `revdep_details(, "fcoex")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘fcoex-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mod_ora
    > ### Title: Over Representation Analysis (ORA)
    > ### Aliases: mod_ora mod_ora,fcoex-method
    > 
    > ### ** Examples
    > 
    > data("fc")
    ...
    > gmt_fname <- system.file("extdata", "pathways.gmt", package = "CEMiTool")
    > gmt_in <- pathwayPCA::read_gmt(gmt_fname)
    Warning in file(con, "rb") :
      file("") only supports open = "w+" and open = "w+b": using the former
    Warning in readChar(file, nchars = nChars, useBytes = TRUE) :
      text connection used with readChar(), results may be incorrect
    Error in readChar(file, nchars = nChars, useBytes = TRUE) : 
      invalid 'nchars' argument
    Calls: <Anonymous> -> readChar
    Execution halted
    ```

*   checking whether package ‘fcoex’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘Matrix::head’ by ‘utils::head’ when loading ‘fcoex’
    See ‘/srv/scratch/z3528859/github/statnet/network/revdep/checks/fcoex/new/fcoex.Rcheck/00install.out’ for details.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object '.plot_one_interaction':
      ‘...’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘schex’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    3.8Mb
    ```

# fssemR

<details>

* Version: 0.1.6
* GitHub: https://github.com/Ivis4ml/fssemR
* Source code: https://github.com/cran/fssemR
* Date/Publication: 2019-12-04 16:10:05 UTC
* Number of recursive dependencies: 80

Run `revdep_details(, "fssemR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.8Mb
      sub-directories of 1Mb or more:
        libs  15.9Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# genBaRcode

<details>

* Version: 1.2.4
* GitHub: NA
* Source code: https://github.com/cran/genBaRcode
* Date/Publication: 2020-12-02 22:50:07 UTC
* Number of recursive dependencies: 152

Run `revdep_details(, "genBaRcode")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        extdata   3.9Mb
    ```

# geomnet

<details>

* Version: 0.3.1
* GitHub: https://github.com/sctyner/geomnet
* Source code: https://github.com/cran/geomnet
* Date/Publication: 2020-11-26 11:00:06 UTC
* Number of recursive dependencies: 86

Run `revdep_details(, "geomnet")` for more info

</details>

## In both

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

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.6Mb
        doc    1.4Mb
    ```

# GGally

<details>

* Version: 2.1.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2021-03-08 06:00:03 UTC
* Number of recursive dependencies: 135

Run `revdep_details(, "GGally")` for more info

</details>

## In both

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
      
      [ FAIL 3 | WARN 1329 | SKIP 3 | PASS 563 ]
      Error: Test failures
      Execution halted
    ```

# GGMnonreg

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/GGMnonreg
* Date/Publication: 2021-04-08 11:30:06 UTC
* Number of recursive dependencies: 111

Run `revdep_details(, "GGMnonreg")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘Rdpack’
      All declared Imports should be used.
    ```

# ggnetwork

<details>

* Version: 0.5.9
* GitHub: https://github.com/briatte/ggnetwork
* Source code: https://github.com/cran/ggnetwork
* Date/Publication: 2021-06-04 17:50:02 UTC
* Number of recursive dependencies: 65

Run `revdep_details(, "ggnetwork")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggnetwork)
      Loading required package: ggplot2
      > 
      > test_check("ggnetwork")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-ggnetwork.R:57:3): ggnetwork works ────────────────────────────
      `ggnetwork(n)` did not produce any warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 42 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sna’ ‘utils’
      All declared Imports should be used.
    ```

# GGPA

<details>

* Version: 1.4.0
* GitHub: https://github.com/dongjunchung/GGPA
* Source code: https://github.com/cran/GGPA
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 68

Run `revdep_details(, "GGPA")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        libs   3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘network’ ‘scales’ ‘sna’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘GGPA/libs/GGPA.so’:
      Found ‘_ZSt4cout’, possibly from ‘std::cout’ (C++)
        Object: ‘3_Param.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# ggraph

<details>

* Version: 2.0.5
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2021-02-23 12:20:30 UTC
* Number of recursive dependencies: 95

Run `revdep_details(, "ggraph")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 17.1Mb
      sub-directories of 1Mb or more:
        doc    7.5Mb
        libs   7.9Mb
    ```

# GOxploreR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/GOxploreR
* Date/Publication: 2021-06-07 14:50:02 UTC
* Number of recursive dependencies: 99

Run `revdep_details(, "GOxploreR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        R   8.5Mb
    ```

# GRENITS

<details>

* Version: 1.44.0
* GitHub: NA
* Source code: https://github.com/cran/GRENITS
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 40

Run `revdep_details(, "GRENITS")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        libs  16.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘Rcpp’ ‘RcppArmadillo’ ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .heatMap.ggplot: no visible global function definition for ‘ggplot’
    .heatMap.ggplot: no visible global function definition for ‘aes’
    .heatMap.ggplot: no visible binding for global variable ‘Var2’
    .heatMap.ggplot: no visible binding for global variable ‘Var1’
    .heatMap.ggplot: no visible global function definition for ‘geom_tile’
    .heatMap.ggplot: no visible binding for global variable ‘value’
    .heatMap.ggplot: no visible global function definition for
      ‘scale_fill_gradient’
    .heatMap.ggplot: no visible global function definition for ‘theme’
    .heatMap.ggplot: no visible global function definition for
    ...
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘scale_x_discrete’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘scale_y_discrete’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘facet_wrap’
    Undefined global functions or variables:
      GeneNames Var1 Var2 aes element_blank element_text facet_wrap
      geom_tile ggplot ggtitle labs scale_fill_gradient scale_x_discrete
      scale_y_discrete theme value variable
    ```

# gwdegree

<details>

* Version: 0.1.1
* GitHub: https://github.com/michaellevy/gwdegree
* Source code: https://github.com/cran/gwdegree
* Date/Publication: 2016-07-09 10:46:45
* Number of recursive dependencies: 82

Run `revdep_details(, "gwdegree")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    simCCCent : <anonymous>: no visible global function definition for
      ‘simulate.formula’
    Undefined global functions or variables:
      simulate.formula
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ICON

<details>

* Version: 0.4.0
* GitHub: https://github.com/rrrlw/ICON
* Source code: https://github.com/cran/ICON
* Date/Publication: 2020-10-23 23:00:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(, "ICON")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-s3-network.R:21:3): as.network.ICON generic works ─────────────
      network::get.edge.attribute(converted_network, "attrib") not equal to test_obj$attrib.
      Lengths differ: 24 is not 25
      ── Failure (test-s3-network.R:24:3): as.network.ICON generic works ─────────────
      network::network.edgecount(converted_network) not equal to `NROW`.
      1/1 mismatches
      [1] 24 - 25 == -1
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

# leiden

<details>

* Version: 0.3.8
* GitHub: https://github.com/TomKellyGenetics/leiden
* Source code: https://github.com/cran/leiden
* Date/Publication: 2021-05-24 04:30:03 UTC
* Number of recursive dependencies: 124

Run `revdep_details(, "leiden")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3.   └─reticulate::import("numpy", delay_load = TRUE)
       4.     └─reticulate:::py_module_import(module, convert = convert)
      ── Error (test_multiplex.R:251:3): run with ModularityVertexPartition multiplexed and max_comm_size ──
      Error: Node size vector not the same size as the number of nodes.
      Backtrace:
          █
       1. ├─leiden::leiden(...) test_multiplex.R:251:2
       2. └─leiden:::leiden.list(...)
       3.   └─leiden:::find_partition_multiplex(...)
       4.     └─leidenalg$find_partition_multiplex(...)
       5.       └─reticulate:::py_call_impl(callable, dots$args, dots$keywords)
      
      [ FAIL 7 | WARN 0 | SKIP 8 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

# lolog

<details>

* Version: 1.2
* GitHub: https://github.com/statnet/lolog
* Source code: https://github.com/cran/lolog
* Date/Publication: 2019-01-12 22:52:41 UTC
* Number of recursive dependencies: 75

Run `revdep_details(, "lolog")` for more info

</details>

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘lolog-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.network.Rcpp_DirectedNet
    > ### Title: Convert a DirectedNet to a network object
    > ### Aliases: as.network.Rcpp_DirectedNet
    > 
    > ### ** Examples
    > 
    > el <- matrix(c(1,2),ncol=2)
    > 
    > #make an UndirectedNet with one edge and 5 nodes
    > net <- new(UndirectedNet, el, 5L)
    > 
    > nw <- as.network(net)
    Error in apply(x[, 1:2], 1, sort) : dim(X) must have a positive length
    Calls: as.network ... as.network.matrix -> network.edgelist -> t -> apply
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 34.0Mb
      sub-directories of 1Mb or more:
        libs  32.0Mb
    ```

# MBCbook

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/MBCbook
* Date/Publication: 2019-07-02 07:00:03 UTC
* Number of recursive dependencies: 25

Run `revdep_details(, "MBCbook")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 66 marked UTF-8 strings
    ```

# migraph

<details>

* Version: 0.6.6
* GitHub: https://github.com/snlab-ch/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2021-05-13 23:20:10 UTC
* Number of recursive dependencies: 83

Run `revdep_details(, "migraph")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
    ```

# mpa

<details>

* Version: 0.7.3
* GitHub: NA
* Source code: https://github.com/cran/mpa
* Date/Publication: 2012-10-29 08:59:13
* Number of recursive dependencies: 17

Run `revdep_details(, "mpa")` for more info

</details>

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

* Version: 1.22.3
* GitHub: https://github.com/USCCANA/netdiffuseR
* Source code: https://github.com/cran/netdiffuseR
* Date/Publication: 2021-05-28 16:30:08 UTC
* Number of recursive dependencies: 73

Run `revdep_details(, "netdiffuseR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.0Mb
      sub-directories of 1Mb or more:
        doc    2.3Mb
        libs   8.9Mb
    ```

# nethet

<details>

* Version: 1.24.0
* GitHub: NA
* Source code: https://github.com/cran/nethet
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 97

Run `revdep_details(, "nethet")` for more info

</details>

## In both

*   checking whether package ‘nethet’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘mclust::dmvnorm’ by ‘mvtnorm::dmvnorm’ when loading ‘nethet’
      Warning: replacing previous import ‘multtest::update’ by ‘stats::update’ when loading ‘nethet’
    See ‘/srv/scratch/z3528859/github/statnet/network/revdep/checks/nethet/new/nethet.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    screen_shrink: no visible global function definition for
      ‘performance.pcor’
    Undefined global functions or variables:
      performance.pcor
    ```

# NetMix

<details>

* Version: 0.2.0
* GitHub: https://github.com/solivella/NetMix
* Source code: https://github.com/cran/NetMix
* Date/Publication: 2021-03-01 17:40:08 UTC
* Number of recursive dependencies: 49

Run `revdep_details(, "NetMix")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        libs   7.9Mb
    ```

# NetworkChange

<details>

* Version: 0.7
* GitHub: NA
* Source code: https://github.com/cran/NetworkChange
* Date/Publication: 2020-07-11 22:00:14 UTC
* Number of recursive dependencies: 124

Run `revdep_details(, "NetworkChange")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        doc   8.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# networkDynamic

<details>

* Version: 0.10.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamic
* Date/Publication: 2020-01-21 09:50:02 UTC
* Number of recursive dependencies: 35

Run `revdep_details(, "networkDynamic")` for more info

</details>

## In both

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

# networksis

<details>

* Version: 2.1-3
* GitHub: NA
* Source code: https://github.com/cran/networksis
* Date/Publication: 2015-04-16 08:44:42
* Number of recursive dependencies: 17

Run `revdep_details(, "networksis")` for more info

</details>

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

* Version: 1.1.0.2
* GitHub: https://github.com/thongphamthe/PAFit
* Source code: https://github.com/cran/PAFit
* Date/Publication: 2021-05-17 21:50:24 UTC
* Number of recursive dependencies: 49

Run `revdep_details(, "PAFit")` for more info

</details>

## In both

*   checking whether package ‘PAFit’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/srv/scratch/z3528859/github/statnet/network/revdep/checks/PAFit/new/PAFit.Rcheck/00install.out’ for details.
    ```

# PDN

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PDN
* Date/Publication: 2017-11-03 23:27:43 UTC
* Number of recursive dependencies: 54

Run `revdep_details(, "PDN")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

# PhosR

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/PhosR
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 203

Run `revdep_details(, "PhosR")` for more info

</details>

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        doc    4.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::disableValidity’
      ‘SummarizedExperiment:::.SummarizedExperiment.charbound’
      See the note in ?`:::` about the use of this operator.
    ```

# pkggraph

<details>

* Version: 0.2.3
* GitHub: https://github.com/talegari/pkggraph
* Source code: https://github.com/cran/pkggraph
* Date/Publication: 2018-11-15 09:50:03 UTC
* Number of recursive dependencies: 64

Run `revdep_details(, "pkggraph")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        doc   8.0Mb
    ```

# predictionet

<details>

* Version: 1.38.0
* GitHub: NA
* Source code: https://github.com/cran/predictionet
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 40

Run `revdep_details(, "predictionet")` for more info

</details>

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘catnet’ ‘igraph’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .build.regression.regrnet: no visible global function definition for
      ‘formula’
    .build.regression.regrnet: no visible global function definition for
      ‘lm’
    .build2.mim: no visible global function definition for ‘cor’
    .exportGML: no visible global function definition for ‘vcount’
    .exportGML: no visible global function definition for ‘ecount’
    .get.ii4triplets.gaussian: no visible global function definition for
      ‘cor’
    .pred.onegene.bayesnet.fs : <anonymous>: no visible global function
    ...
    Undefined global functions or variables:
      as cnMatParents coefficients complete.cases cor ecount formula lm
      quantile sessionInfo vcount
    Consider adding
      importFrom("methods", "as")
      importFrom("stats", "coefficients", "complete.cases", "cor", "formula",
                 "lm", "quantile")
      importFrom("utils", "sessionInfo")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking compiled code ... NOTE
    ```
    File ‘predictionet/libs/predictionet.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘mrnet_ensemble_standalone.o’
      Found ‘srand’, possibly from ‘srand’ (C)
        Object: ‘mrnet_ensemble_standalone.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# PubScore

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/PubScore
* Date/Publication: 2021-05-19
* Number of recursive dependencies: 129

Run `revdep_details(, "PubScore")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.4Mb
      sub-directories of 1Mb or more:
        data   8.1Mb
        doc    8.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    get_literature_score: no visible binding for global variable
      ‘gene2pubmed_db’
    get_literature_score: no visible binding for global variable
      ‘hgcn_entrez_reference’
    get_sum_of_weights: no visible binding for global variable ‘Genes’
    get_sum_of_weights: no visible binding for global variable ‘Topic’
    plot_literature_score: no visible binding for global variable ‘Genes’
    plot_literature_score: no visible binding for global variable ‘Topic’
    plot_literature_score: no visible binding for global variable
      ‘number_of_articles’
    show,PubScore: no visible binding for global variable ‘pub’
    Undefined global functions or variables:
      Genes Topic gene2pubmed_db hgcn_entrez_reference number_of_articles
      pub
    ```

# quanteda.textplots

<details>

* Version: 0.94
* GitHub: https://github.com/quanteda/quanteda.textplots
* Source code: https://github.com/cran/quanteda.textplots
* Date/Publication: 2021-04-06 07:30:05 UTC
* Number of recursive dependencies: 94

Run `revdep_details(, "quanteda.textplots")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# RSiena

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/RSiena
* Date/Publication: 2021-05-10 16:22:15 UTC
* Number of recursive dependencies: 21

Run `revdep_details(, "RSiena")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.1Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        libs  17.2Mb
    ```

# sand

<details>

* Version: 2.0.0
* GitHub: https://github.com/kolaczyk/sand
* Source code: https://github.com/cran/sand
* Date/Publication: 2020-07-02 07:20:06 UTC
* Number of recursive dependencies: 163

Run `revdep_details(, "sand")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# SBICgraph

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/SBICgraph
* Date/Publication: 2021-03-02 19:10:09 UTC
* Number of recursive dependencies: 40

Run `revdep_details(, "SBICgraph")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘network’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# sparsebnUtils

<details>

* Version: 0.0.8
* GitHub: https://github.com/itsrainingdata/sparsebnUtils
* Source code: https://github.com/cran/sparsebnUtils
* Date/Publication: 2021-01-27 06:30:02 UTC
* Number of recursive dependencies: 75

Run `revdep_details(, "sparsebnUtils")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘sparsebn’
    ```

# statnetWeb

<details>

* Version: 0.5.6
* GitHub: NA
* Source code: https://github.com/cran/statnetWeb
* Date/Publication: 2020-08-05 18:00:03 UTC
* Number of recursive dependencies: 53

Run `revdep_details(, "statnetWeb")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tergmLite

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/tergmLite
* Date/Publication: 2020-07-22 16:50:03 UTC
* Number of recursive dependencies: 67

Run `revdep_details(, "tergmLite")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# tidygraph

<details>

* Version: 1.2.0
* GitHub: https://github.com/thomasp85/tidygraph
* Source code: https://github.com/cran/tidygraph
* Date/Publication: 2020-05-12 07:30:03 UTC
* Number of recursive dependencies: 93

Run `revdep_details(, "tidygraph")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# vivid

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-04-09 09:10:02 UTC
* Number of recursive dependencies: 205

Run `revdep_details(, "vivid")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `zpath` has type 'NULL', not 'list'.
      ── Failure (test_plots.R:203:3): Test zpath ────────────────────────────────────
      `zpath1` has type 'NULL', not 'character'.
      ── Failure (test_plots.R:206:3): Test zpath ────────────────────────────────────
      `zpath2` has type 'NULL', not 'character'.
      ── Failure (test_plots.R:208:3): Test zpath ────────────────────────────────────
      `zPath(aqVivi, cutoff = 100)` did not throw the expected error.
      Backtrace:
          █
       1. └─testthat::expect_error(zPath(aqVivi, cutoff = 100)) test_plots.R:208:2
       2.   └─testthat:::expect_condition_matching(...)
      
      [ FAIL 15 | WARN 0 | SKIP 0 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘zenplots’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘colorspace’ ‘tidyr’
      All declared Imports should be used.
    ```

# xergm.common

<details>

* Version: 1.7.8
* GitHub: https://github.com/leifeld/xergm.common
* Source code: https://github.com/cran/xergm.common
* Date/Publication: 2020-04-07 09:50:02 UTC
* Number of recursive dependencies: 26

Run `revdep_details(, "xergm.common")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘RSiena’
    ```

