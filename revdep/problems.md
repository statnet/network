# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 76

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

* Version: 2.0.4
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2021-08-20 15:50:06 UTC
* Number of recursive dependencies: 167

Run `revdep_details(, "BGGM")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.8Mb
      sub-directories of 1Mb or more:
        doc    3.8Mb
        help   1.1Mb
        libs   7.2Mb
    ```

# bingat

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/bingat
* Date/Publication: 2017-07-05 18:30:37 UTC
* Number of recursive dependencies: 32

Run `revdep_details(, "bingat")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘genalg’
    ```

# Blaunet

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/Blaunet
* Date/Publication: 2022-09-27 08:10:08 UTC
* Number of recursive dependencies: 65

Run `revdep_details(, "Blaunet")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgets2tcltk’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# bootcluster

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/bootcluster
* Date/Publication: 2022-01-29 22:50:03 UTC
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

* Version: 1.0.1
* GitHub: https://github.com/tidymodels/broom
* Source code: https://github.com/cran/broom
* Date/Publication: 2022-08-29 21:00:08 UTC
* Number of recursive dependencies: 292

Run `revdep_details(, "broom")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'epiR', 'spdep', 'spatialreg'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘spatialreg’, ‘epiR’
    ```

# CEMiTool

<details>

* Version: 1.20.0
* GitHub: NA
* Source code: https://github.com/cran/CEMiTool
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 194

Run `revdep_details(, "CEMiTool")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data      3.1Mb
        doc       2.3Mb
        extdata   2.8Mb
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

* Version: 1.8.0
* GitHub: NA
* Source code: https://github.com/cran/CeTF
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 228

Run `revdep_details(, "CeTF")` for more info

</details>

## Newly fixed

*   checking re-building of vignette outputs ...sh: line 1: 247279 Killed                  '/srv/scratch/z3528859/R-4.2.0/bin/R' --vanilla --no-echo > '/srv/scratch/z3528859/github/statnet/network/revdep/checks/CeTF/old/CeTF.Rcheck/build_vignettes.log' 2>&1 < '/scratch/pbs.3584821.kman.restech.unsw.edu.au/Rtmp5JkIUj/file375426c1496a9'
    ```
     ERROR
    Error(s) in re-building vignettes:
    --- re-building ‘CeTF.Rmd’ using rmarkdown
    
    ========================================
    CeTF version 1.8.0
    Bioconductor page: http://bioconductor.org/packages/CeTF/
    Github page: https://github.com/cbiagii/CeTF or https://cbiagii.github.io/CeTF/
    Documentation: http://bioconductor.org/packages/CeTF/
    If you use it in published research, please cite:
    ...
        count
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    3.0Mb
        libs   2.5Mb
    ```

# chorrrds

<details>

* Version: 0.1.9.5
* GitHub: https://github.com/r-music/chorrrds
* Source code: https://github.com/cran/chorrrds
* Date/Publication: 2020-06-30 17:30:02 UTC
* Number of recursive dependencies: 72

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

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2022-04-07 15:12:31 UTC
* Number of recursive dependencies: 145

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

* Version: 1.12.0
* GitHub: https://github.com/tdepke/CluMSID
* Source code: https://github.com/cran/CluMSID
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 182

Run `revdep_details(, "CluMSID")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘MSnbase’
    
    Packages suggested but not available for checking: 'metaMS', 'xcms'
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# conserveR

<details>

* Version: 1.0.4
* GitHub: https://github.com/azizka/conserveR
* Source code: https://github.com/cran/conserveR
* Date/Publication: 2021-08-02 09:10:06 UTC
* Number of recursive dependencies: 51

Run `revdep_details(, "conserveR")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 244 marked UTF-8 strings
    ```

# edgebundle

<details>

* Version: 0.4.0
* GitHub: https://github.com/schochastics/edgebundle
* Source code: https://github.com/cran/edgebundle
* Date/Publication: 2022-07-05 00:10:10 UTC
* Number of recursive dependencies: 53

Run `revdep_details(, "edgebundle")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 22 marked UTF-8 strings
    ```

# ergm

<details>

* Version: 4.2.2
* GitHub: https://github.com/statnet/ergm
* Source code: https://github.com/cran/ergm
* Date/Publication: 2022-06-01 12:00:04 UTC
* Number of recursive dependencies: 89

Run `revdep_details(, "ergm")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        doc    1.7Mb
        libs   3.8Mb
    ```

# ergMargins

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/ergMargins
* Date/Publication: 2021-06-30 07:40:02 UTC
* Number of recursive dependencies: 58

Run `revdep_details(, "ergMargins")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘sna’ ‘statnet.common’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘margins’
    ```

# ergmito

<details>

* Version: 0.3-0
* GitHub: https://github.com/muriteams/ergmito
* Source code: https://github.com/cran/ergmito
* Date/Publication: 2020-08-10 21:40:02 UTC
* Number of recursive dependencies: 65

Run `revdep_details(, "ergmito")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   5.7Mb
    ```

# fcoex

<details>

* Version: 1.10.0
* GitHub: NA
* Source code: https://github.com/cran/fcoex
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 298

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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fcoex.Rmd’ using rmarkdown
    
    
    Attaching package: 'MatrixGenerics'
    
    The following objects are masked from 'package:matrixStats':
    
        colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
        colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
    ...
    Quitting from lines 67-75 (fcoex_and_seurat.Rmd) 
    Error: processing vignette 'fcoex_and_seurat.Rmd' failed with diagnostics:
    invalid 'nchars' argument
    --- failed re-building ‘fcoex_and_seurat.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘fcoex.Rmd’ ‘fcoex_and_seurat.Rmd’
    
    Error: Vignette re-building failed.
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
    Packages suggested but not available for checking: 'TENxPBMCData', 'schex'
    ```

# fssemR

<details>

* Version: 0.1.8
* GitHub: https://github.com/Ivis4ml/fssemR
* Source code: https://github.com/cran/fssemR
* Date/Publication: 2022-02-11 13:00:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(, "fssemR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.2Mb
      sub-directories of 1Mb or more:
        libs  14.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘qpdf’
      All declared Imports should be used.
    ```

# geomnet

<details>

* Version: 0.3.1
* GitHub: https://github.com/sctyner/geomnet
* Source code: https://github.com/cran/geomnet
* Date/Publication: 2020-11-26 11:00:06 UTC
* Number of recursive dependencies: 98

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

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘geomnet-vignette.Rmd’ using rmarkdown
    Quitting from lines 15-112 (geomnet-vignette.Rmd) 
    Error: processing vignette 'geomnet-vignette.Rmd' failed with diagnostics:
    The first two columns of `x` must be of the same type.
    --- failed re-building ‘geomnet-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘geomnet-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# GGally

<details>

* Version: 2.1.2
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2021-06-21 04:40:10 UTC
* Number of recursive dependencies: 147

Run `revdep_details(, "GGally")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘scagnostics’
    ```

# GGMncv

<details>

* Version: 2.1.1
* GitHub: https://github.com/donaldRwilliams/GGMncv
* Source code: https://github.com/cran/GGMncv
* Date/Publication: 2021-12-15 07:40:28 UTC
* Number of recursive dependencies: 152

Run `revdep_details(, "GGMncv")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rdpack’ ‘mathjaxr’
      All declared Imports should be used.
    ```

# GGMnonreg

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/GGMnonreg
* Date/Publication: 2021-04-08 11:30:06 UTC
* Number of recursive dependencies: 112

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

* Version: 0.5.10
* GitHub: https://github.com/briatte/ggnetwork
* Source code: https://github.com/cran/ggnetwork
* Date/Publication: 2021-07-06 05:30:02 UTC
* Number of recursive dependencies: 70

Run `revdep_details(, "ggnetwork")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘sna’ ‘utils’
      All declared Imports should be used.
    ```

# GGPA

<details>

* Version: 1.8.0
* GitHub: https://github.com/dongjunchung/GGPA
* Source code: https://github.com/cran/GGPA
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 74

Run `revdep_details(, "GGPA")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
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

* Version: 2.0.6
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2022-08-08 11:40:02 UTC
* Number of recursive dependencies: 96

Run `revdep_details(, "ggraph")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        libs   4.0Mb
    ```

# GOxploreR

<details>

* Version: 1.2.6
* GitHub: NA
* Source code: https://github.com/cran/GOxploreR
* Date/Publication: 2022-04-19 14:02:33 UTC
* Number of recursive dependencies: 114

Run `revdep_details(, "GOxploreR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        R   6.1Mb
    ```

# GRENITS

<details>

* Version: 1.48.0
* GitHub: NA
* Source code: https://github.com/cran/GRENITS
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 38

Run `revdep_details(, "GRENITS")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        libs  10.6Mb
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
* Number of recursive dependencies: 89

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
* Number of recursive dependencies: 80

Run `revdep_details(, "ICON")` for more info

</details>

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-s3-network.R:21:3): as.network.ICON generic works ─────────────
      network::get.edge.attribute(converted_network, "attrib") not equal to test_obj$attrib.
      Lengths differ: 21 is not 25
      ── Failure (test-s3-network.R:24:3): as.network.ICON generic works ─────────────
      network::network.edgecount(converted_network) not equal to `NROW`.
      1/1 mismatches
      [1] 21 - 25 == -4
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

# InflectSSP

<details>

* Version: 1.4.4
* GitHub: NA
* Source code: https://github.com/cran/InflectSSP
* Date/Publication: 2022-03-18 14:40:02 UTC
* Number of recursive dependencies: 84

Run `revdep_details(, "InflectSSP")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘xlsx’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# latentnet

<details>

* Version: 2.10.6
* GitHub: https://github.com/statnet/latentnet
* Source code: https://github.com/cran/latentnet
* Date/Publication: 2022-05-11 12:30:05 UTC
* Number of recursive dependencies: 112

Run `revdep_details(, "latentnet")` for more info

</details>

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# leiden

<details>

* Version: 0.4.3
* GitHub: https://github.com/TomKellyGenetics/leiden
* Source code: https://github.com/cran/leiden
* Date/Publication: 2022-09-10 17:22:53 UTC
* Number of recursive dependencies: 146

Run `revdep_details(, "leiden")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’/srv/scratch/z3528859/R-4.2.0/bin/BATCH: line 60: 67256 Killed                  ${R_HOME}/bin/R -f ${in} ${opts} ${R_BATCH_OPTIONS} > ${out} 2>&1
    
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("leiden")
      Unable to set up conda environment r-reticulate
      run in terminal:
      conda init
      conda create -n r-reticulate
      conda environment r-reticulate installed
      Unable to install python modules igraph and leidenalg
      run in terminal:
      conda install -n r-reticulate -c conda-forge vtraag python-igraph pandas umap learn
      python modules igraph and leidenalg installed
      > 
      > test_check("leiden")
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc   5.9Mb
    ```

# lolog

<details>

* Version: 1.3
* GitHub: https://github.com/statnet/lolog
* Source code: https://github.com/cran/lolog
* Date/Publication: 2021-07-01 07:50:06 UTC
* Number of recursive dependencies: 80

Run `revdep_details(, "lolog")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is 24.8Mb
      sub-directories of 1Mb or more:
        libs  23.1Mb
    ```

# MBCbook

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/MBCbook
* Date/Publication: 2019-07-02 07:00:03 UTC
* Number of recursive dependencies: 23

Run `revdep_details(, "MBCbook")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 66 marked UTF-8 strings
    ```

# migraph

<details>

* Version: 0.12.0
* GitHub: https://github.com/snlab-ch/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2022-09-27 07:50:02 UTC
* Number of recursive dependencies: 131

Run `revdep_details(, "migraph")` for more info

</details>

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-viz_autographr.R:115:3): fancy node mods graph correctly ──────
      class(testcolnodes) not equal to c("ggraph", "gg", "ggplot").
      Lengths differ: 1 is not 3
      ── Error (test-viz_autographr.R:116:3): fancy node mods graph correctly ────────
      Error in `round(testcolnodes$data$x[1])`: non-numeric argument to mathematical function
      Backtrace:
          ▆
       1. └─testthat::expect_equal(round(testcolnodes$data$x[1]), 4) at test-viz_autographr.R:116:2
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 526 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘concaveman’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.0Mb
    ```

# mpa

<details>

* Version: 0.7.3
* GitHub: NA
* Source code: https://github.com/cran/mpa
* Date/Publication: 2012-10-29 08:59:13
* Number of recursive dependencies: 15

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

* Version: 1.22.4
* GitHub: https://github.com/USCCANA/netdiffuseR
* Source code: https://github.com/cran/netdiffuseR
* Date/Publication: 2022-09-20 07:46:13 UTC
* Number of recursive dependencies: 79

Run `revdep_details(, "netdiffuseR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.2Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   8.8Mb
    ```

# nethet

<details>

* Version: 1.28.0
* GitHub: NA
* Source code: https://github.com/cran/nethet
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 102

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

# netmap

<details>

* Version: 0.1.1
* GitHub: https://github.com/artod83/netmap
* Source code: https://github.com/cran/netmap
* Date/Publication: 2022-03-18 16:00:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(, "netmap")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘sf’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# netmediate

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/netmediate
* Date/Publication: 2022-08-31 07:50:02 UTC
* Number of recursive dependencies: 99

Run `revdep_details(, "netmediate")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mediation’
    ```

# networksis

<details>

* Version: 2.1-3
* GitHub: NA
* Source code: https://github.com/cran/networksis
* Date/Publication: 2015-04-16 08:44:42
* Number of recursive dependencies: 15

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

# PDN

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PDN
* Date/Publication: 2017-11-03 23:27:43 UTC
* Number of recursive dependencies: 60

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

* Version: 1.6.0
* GitHub: NA
* Source code: https://github.com/cran/PhosR
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 182

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
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   3.3Mb
        doc    2.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::disableValidity’
      ‘SummarizedExperiment:::.SummarizedExperiment.charbound’
      See the note in ?`:::` about the use of this operator.
    ```

# predictionet

<details>

* Version: 1.40.0
* GitHub: NA
* Source code: https://github.com/cran/predictionet
* Date/Publication: 2021-10-26
* Number of recursive dependencies: 36

Run `revdep_details(, "predictionet")` for more info

</details>

## In both

*   checking whether package ‘predictionet’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Package 'predictionet' is deprecated and will be removed from
    See ‘/srv/scratch/z3528859/github/statnet/network/revdep/checks/predictionet/new/predictionet.Rcheck/00install.out’ for details.
    ```

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

* Version: 1.8.0
* GitHub: NA
* Source code: https://github.com/cran/PubScore
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 132

Run `revdep_details(, "PubScore")` for more info

</details>

## Newly fixed

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘PubScore_vignette.Rmd’ using rmarkdown
    Quitting from lines 144-153 (PubScore_vignette.Rmd) 
    Error: processing vignette 'PubScore_vignette.Rmd' failed with diagnostics:
    Gateway Timeout (HTTP 504).
    --- failed re-building ‘PubScore_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘PubScore_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking whether package ‘PubScore’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Package 'PubScore' is deprecated and will be removed from Bioconductor
    See ‘/srv/scratch/z3528859/github/statnet/network/revdep/checks/PubScore/new/PubScore.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        data   4.2Mb
        doc    4.9Mb
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

# RSiena

<details>

* Version: 1.3.0.1
* GitHub: NA
* Source code: https://github.com/cran/RSiena
* Date/Publication: 2021-11-15 17:13:35 UTC
* Number of recursive dependencies: 19

Run `revdep_details(, "RSiena")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 19.1Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        libs  17.2Mb
    ```

# sand

<details>

* Version: 2.0.0
* GitHub: https://github.com/kolaczyk/sand
* Source code: https://github.com/cran/sand
* Date/Publication: 2020-07-02 07:20:06 UTC
* Number of recursive dependencies: 160

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
* Number of recursive dependencies: 47

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
* Number of recursive dependencies: 64

Run `revdep_details(, "sparsebnUtils")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Unknown package ‘sparsebn’ in Rd xrefs
    ```

# statnetWeb

<details>

* Version: 0.5.6
* GitHub: NA
* Source code: https://github.com/cran/statnetWeb
* Date/Publication: 2020-08-05 18:00:03 UTC
* Number of recursive dependencies: 65

Run `revdep_details(, "statnetWeb")` for more info

</details>

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# vivid

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-11-20 01:30:02 UTC
* Number of recursive dependencies: 201

Run `revdep_details(, "vivid")` for more info

</details>

## In both

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

# wildlifeDI

<details>

* Version: 0.4.1
* GitHub: https://github.com/jedalong/wildlifeDI
* Source code: https://github.com/cran/wildlifeDI
* Date/Publication: 2021-06-16 15:20:02 UTC
* Number of recursive dependencies: 90

Run `revdep_details(, "wildlifeDI")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘sf’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# WOTPLY

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/WOTPLY
* Date/Publication: 2022-09-12 07:43:01 UTC
* Number of recursive dependencies: 70

Run `revdep_details(, "WOTPLY")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sna’
      All declared Imports should be used.
    ```

# xergm.common

<details>

* Version: 1.7.8
* GitHub: https://github.com/leifeld/xergm.common
* Source code: https://github.com/cran/xergm.common
* Date/Publication: 2020-04-07 09:50:02 UTC
* Number of recursive dependencies: 35

Run `revdep_details(, "xergm.common")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘RSiena’
    ```

