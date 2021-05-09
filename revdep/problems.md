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
      7. │ └─dplyr:::mutate_cols(.data, ..., caller_env = caller_env())
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

