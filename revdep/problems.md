# Bergm

<details>

* Version: 5.0.1
* GitHub: NA
* Source code: https://github.com/cran/Bergm
* Date/Publication: 2019-11-04 12:00:02 UTC
* Number of recursive dependencies: 42

Run `revdep_details(, "Bergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: bergm
    > ### Title: Parameter estimation for Bayesian ERGMs
    > ### Aliases: bergm
    > 
    > ### ** Examples
    > 
    > # Load the florentine marriage network
    > data(florentine)
    > 
    > # Posterior parameter estimation:
    > p.flo <- bergm(flomarriage ~ edges + kstar(2),
    +                burn.in    = 50,
    +                aux.iters  = 500,
    +                main.iters = 1000,
    +                gamma      = 1.2)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: bergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

# broom

<details>

* Version: 0.7.0
* GitHub: https://github.com/tidymodels/broom
* Source code: https://github.com/cran/broom
* Date/Publication: 2020-07-09 12:30:09 UTC
* Number of recursive dependencies: 277

Run `revdep_details(, "broom")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    All networks are conformable.
    
    Dimensions of the network and covariates after adjustment:
                     t=1 t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    Starting pseudolikelihood estimation with 100 bootstrapping replications on a single computing core...
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: btergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

# btergm

<details>

* Version: 1.9.9
* GitHub: https://github.com/leifeld/btergm
* Source code: https://github.com/cran/btergm
* Date/Publication: 2020-06-18 05:00:06 UTC
* Number of recursive dependencies: 73

Run `revdep_details(, "btergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    All networks are conformable.
    
    Dimensions of the network and covariates after adjustment:
                     t=1 t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10
    networks (row)    10  10  10  10  10  10  10  10  10   10
    networks (col)    10  10  10  10  10  10  10  10  10   10
    covariates (row)  10  10  10  10  10  10  10  10  10   10
    covariates (col)  10  10  10  10  10  10  10  10  10   10
    
    Starting pseudolikelihood estimation with 100 bootstrapping replications on a single computing core...
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: btergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 3. [39mergm::ergm.design(nw, verbose = verbose)
      [90m 7. [39mergm:::InitErgmConstraint..attributes(...)
      [90m 8. [39mergm::rlebdm(compact.rle(d), n)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 1 | WARNINGS: 6 | FAILED: 6 ]
      1. Error: btergm estimation works (@test-btergm.R#23) 
      2. Error: fastglm works like speedglm (@test-btergm.R#58) 
      3. Error: offset argument in btergm works without composition change (@test-btergm.R#66) 
      4. Error: mtergm estimation works (@test-btergm.R#144) 
      5. Error: simulation of new networks works (@test-btergm.R#156) 
      6. Error: (unknown) (@test-gof.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking whether package ‘btergm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/homes/morrism/GitHub/StatnetOrganization/network/revdep/checks/btergm/new/btergm.Rcheck/00install.out’ for details.
    ```

# discourseGT

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/discourseGT
* Date/Publication: 2020-02-17 15:00:06 UTC
* Number of recursive dependencies: 107

Run `revdep_details(, "discourseGT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > df <- sampleData1
    > prepNet <- tabulate_edges(df, iscsvfile = FALSE)
    > baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1",
    + directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE)
    > attdata <- attributeData
    > plotGraphs(baseNet, prop = 20, graphmode = "fruchtermanreingold",
    + attribute = attdata$gender,
    + attribute.label = "Gender",
    + attribute.node.labels = attdata$node, attribute.nodesize = 12)
    Error: `loops` is `FALSE`, but `x` contains loops.
    The following values are affected:
    	- `x[1, 1:2]`
    	- `x[12, 1:2]`
    	- `x[23, 1:2]`
    	- `x[34, 1:2]`
    	- `x[45, 1:2]`
    	- `x[55, 1:2]`
    Execution halted
    ```

# dnr

<details>

* Version: 0.3.4
* GitHub: NA
* Source code: https://github.com/cran/dnr
* Date/Publication: 2018-07-26 07:40:03 UTC
* Number of recursive dependencies: 100

Run `revdep_details(, "dnr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > maxlag <- 3
    > lambda=NA
    > intercept = c("edges")
    > cdim <- length(model.terms)
    > lagmat <- matrix(sample(c(0,1),(maxlag+1)*cdim,replace = TRUE),ncol = cdim)
    > ylag <- rep(1,maxlag)
    > exvar <- NA
    > out <- paramEdge(input_network,model.terms, model.formula,
    +                 graph_mode='digraph',group,intercept = c("edges"),exvar=NA,
    +                 maxlag = 3,
    +                 lagmat = matrix(sample(c(0,1),(maxlag+1)*cdim,
    +                                        replace = TRUE),ncol = cdim),
    +                 ylag = rep(1,maxlag),
    +                 lambda = NA, method='bayesglm',
    +                 alpha.glmnet=1)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: paramEdge ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

# EpiModel

<details>

* Version: 2.0.2
* GitHub: https://github.com/statnet/EpiModel
* Source code: https://github.com/cran/EpiModel
* Date/Publication: 2020-08-05 09:52:19 UTC
* Number of recursive dependencies: 99

Run `revdep_details(, "EpiModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: as.data.frame.netdx
    > ### Title: Extract Timed Edgelists netdx Objects
    > ### Aliases: as.data.frame.netdx
    > ### Keywords: extract
    > 
    > ### ** Examples
    > 
    > # Initialize and parameterize the network model
    > nw <- network_initialize(n = 100)
    > formation <- ~edges
    > target.stats <- 50
    > coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 20)
    > 
    > # Model estimation
    > est <- netest(nw, formation, target.stats, coef.diss, verbose = FALSE)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: netest ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 221 | SKIPPED: 78 | WARNINGS: 41 | FAILED: 41 ]
      1. Error: Copying attributes from network to attribute list (@test-attr-copy.R#15) 
      2. Error: (unknown) (@test-get.R#13) 
      3. Error: merge for netsim (@test-merge.R#63) 
      4. Error: merge for netsim (@test-merge.R#82) 
      5. Error: merge works for open sims saving nw stats (@test-merge.R#102) 
      6. Error: mutate_epi.netsim (@test-mutate.R#9) 
      7. Error: status.vector and infTime.vector (@test-net-long.R#736) 
      8. Error: tergmLite: 1G, Closed (@test-net-tergmLite.R#15) 
      9. Error: tergmLite: 2G, Closed (@test-net-tergmLite.R#59) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ndtv’
    ```

# ergm

<details>

* Version: 3.10.4
* GitHub: https://github.com/statnet/ergm
* Source code: https://github.com/cran/ergm
* Date/Publication: 2019-06-10 05:30:07 UTC
* Number of recursive dependencies: 71

Run `revdep_details(, "ergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘ergm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ergmMPLE
    > ### Title: ERGM Predictors and response for logistic regression calculation
    > ###   of MPLE
    > ### Aliases: ergmMPLE
    > ### Keywords: models regression
    > 
    > ### ** Examples
    > 
    > 
    > data(faux.mesa.high)
    > formula <- faux.mesa.high ~ edges + nodematch("Sex") + nodefactor("Grade")
    > mplesetup <- ergmMPLE(formula)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergmMPLE ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/constrain_degrees_edges.R’ failed.
    Last 13 lines of output:
      > id <- function(nw) apply(as.matrix(nw, matrix.type="adjacency"), 2, sum)
      > e <- function(nw) network.edgecount(nw)
      > 
      > ###### Directed
      > y0 <- as.network(n, density=d, directed=TRUE)
      > 
      > ### Outdegrees
      > ys <- simulate(y0~sender(nodes=TRUE)+receiver(nodes=TRUE), constraints=~odegrees, coef=rep(0,n*2), nsim=nsim, output="stats")
      Error in as.rle(x) : could not find function "as.rle"
      Calls: simulate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    rlebdm: no visible global function definition for ‘as.rle’
    Undefined global functions or variables:
      as.rle
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R      1.2Mb
        doc    1.7Mb
        libs   2.5Mb
    ```

# ergm.count

<details>

* Version: 3.4.0
* GitHub: https://github.com/statnet/ergm.count
* Source code: https://github.com/cran/ergm.count
* Date/Publication: 2019-05-15 07:42:59 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.count")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/valued_fit.R’ failed.
    Last 13 lines of output:
      > diag(m) <- 0
      > y <- as.network(m, matrix.type="a", directed=TRUE, ignore.eval=FALSE, names.eval="w")
      > 
      > truth <- log(sum(m)/n/(n-1))
      > diag(m) <- NA
      > 
      > efit <- ergm(y ~ sum, response="w", reference=~Poisson, verbose=TRUE, control=control.ergm(MCMLE.effectiveSize=128))
      Evaluating network in model.
      Initializing Metropolis-Hastings proposal(s):Error in as.rle(x) : could not find function "as.rle"
      Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.ego

<details>

* Version: 0.5
* GitHub: https://github.com/statnet/ergm.ego
* Source code: https://github.com/cran/ergm.ego
* Date/Publication: 2019-05-31 16:00:03 UTC
* Number of recursive dependencies: 57

Run `revdep_details(, "ergm.ego")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘ergm.ego-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: degreedist.egodata
    > ### Title: Plotting the degree distribution of an egocentric dataset
    > ### Aliases: degreedist.egodata degreedist
    > 
    > ### ** Examples
    > 
    > 
    > data(faux.mesa.high)
    > fmh.ego <- as.egodata(faux.mesa.high)
    Network does not have vertex attribute ‘vertex.names’ to use as ego ID; using 1..N.
    > 
    > degreedist(fmh.ego,by="Grade",brgmod=TRUE)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: degreedist ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/EgoStat.tests.R’ failed.
    Last 13 lines of output:
      > ds <- c(10,15,5,20)
      > 
      > y <- network.initialize(n, directed=FALSE)
      > y %v% "a" <- sample(1:3+6,n,replace=TRUE)
      > y %v% "b" <- sample(letters[1:4],n,replace=TRUE)
      > y %v% "c" <- sample(runif(10),n,replace=TRUE)
      > y %v% "d" <- runif(n)
      > y <- san(y~edges+degree(0:3), target.stats=c(e,ds))
      Error in as.rle(x) : could not find function "as.rle"
      Calls: san ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.rank

<details>

* Version: 1.2.0
* GitHub: https://github.com/statnet/ergm.rank
* Source code: https://github.com/cran/ergm.rank
* Date/Publication: 2019-05-15 07:43:03 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.rank")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/termTests_rank.R’ failed.
    Last 13 lines of output:
      +                   rank.nonconformity("local2")+
      +                   rank.nonconformity("localAND")+
      +                   rank.deference+
      +                   rank.nodeicov("v")+
      +                   rank.edgecov("m")+
      +                   rank.inconsistency(nw0,"r",xa),
      +                 coef=rep(0,8),response="r", reference=~DiscUnif(1, n-1), nsim=S, statsonly=FALSE)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: simulate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning messages:
      1: Use of 'statsonly=' argument has been deprecated. Use 'output='stats'' instead. 
      2: 'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergm.userterms

<details>

* Version: 3.10.0
* GitHub: https://github.com/statnet/ergm.userterms
* Source code: https://github.com/cran/ergm.userterms
* Date/Publication: 2019-05-15 07:43:05 UTC
* Number of recursive dependencies: 31

Run `revdep_details(, "ergm.userterms")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: ergm.userterms-package
    > ### Title: User-defined terms used in Exponential Family Random Graph
    > ###   Models
    > ### Aliases: ergm.userterms-package ergm.userterms
    > ### Keywords: package models
    > 
    > ### ** Examples
    > 
    > data(faux.mesa.high)
    > summary(faux.mesa.high~mindegree(2))
    mindegree2 
            97 
    > fit <- ergm(faux.mesa.high~mindegree(2), estimate="MPLE")
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/examples.R’ failed.
    Last 13 lines of output:
               9 
      > summary(flomarriage~mindegree(1,by="priorates"))
      mindegree.priorates1 
                         2 
      Warning message:
      `set_attrs()` is deprecated as of rlang 0.3.0
      [90mThis warning is displayed once per session.[39m 
      > fit <- ergm(flomarriage~edges+mindegree(1,by="priorates"))
      Error in as.rle(x) : could not find function "as.rle"
      Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# ergMargins

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/ergMargins
* Date/Publication: 2020-05-22 15:00:03 UTC
* Number of recursive dependencies: 90

Run `revdep_details(, "ergMargins")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > a1<-network::as.network(matrix(c(rbinom(10, 1,.3),
    +             rbinom(10, 1,.3),
    +              rbinom(10, 1,.3),
    +               rbinom(10, 1,.3),
    +                rbinom(10, 1,.3),
    +             rbinom(10, 1,.3),
    +             rbinom(10, 1,.3),
    +             rbinom(10, 1,.3),
    +             rbinom(10, 1,.3),
    +             rbinom(10, 1,.3)),
    +           nrow=10,ncol=10))
    > 
    > network::set.vertex.attribute(a1,"var.1",rbinom(10,1,.3))
    > 
    > a<-ergm(a1~edges+nodeifactor("var.1")+nodeofactor("var.1"))
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

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

# ergmito

<details>

* Version: 0.3-0
* GitHub: https://github.com/muriteams/ergmito
* Source code: https://github.com/cran/ergmito
* Date/Publication: 2020-08-10 21:40:02 UTC
* Number of recursive dependencies: 62

Run `revdep_details(, "ergmito")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    addition, Sampson's Monks datasets had mislabeled vertices. See the
    NEWS and the documentation for more details.
    
    NOTE: Some common term arguments pertaining to vertex attribute and
    level selection have changed in 3.10.0. See terms help for more
    details. Use ‘options(ergm.term=list(version="3.9.4"))’ to use old
    behavior.
    
    > data(fivenets)
    > 
    > fivenets2 <- blockdiagonalize(fivenets, attrname = "block") # A network with
    > ans0 <- ergm(
    +   fivenets2 ~ edges + nodematch("female"),
    +   constraints = ~blockdiag("block")
    +   )
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: ergm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      Running test-ergm_blockdiagonal.R.....    1 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    2 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    3 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    3 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    3 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    3 tests [0;32mOK[0m 
      Running test-ergm_blockdiagonal.R.....    3 tests [0;32mOK[0m Error in as.rle(x) : could not find function "as.rle"
      Calls: <Anonymous> ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning messages:
      1: `set_attrs()` is deprecated as of rlang 0.3.0
      [90mThis warning is displayed once per session.[39m 
      2: 'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        libs   7.8Mb
    ```

# GGally

<details>

* Version: 2.0.0
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2020-06-06 05:00:13 UTC
* Number of recursive dependencies: 121

Run `revdep_details(, "GGally")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following values are affected:
      	- `x[114, 1:2]`
      [1mBacktrace:[22m
      [90m 1. [39mnetwork::network(flights, directed = TRUE)
      [90m 3. [39mnetwork::as.network.data.frame(...)
      [90m 4. [39mnetwork:::.validate_edge_df(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 483 | SKIPPED: 3 | WARNINGS: 2 | FAILED: 3 ]
      1. Error: examples (@test-ggnet.R#211) 
      2. Error: examples (@test-ggnet2.R#244) 
      3. Error: (unknown) (@test-ggnetworkmap.R#34) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# latentnet

<details>

* Version: 2.10.5
* GitHub: https://github.com/statnet/latentnet
* Source code: https://github.com/cran/latentnet
* Date/Publication: 2020-03-22 08:50:02 UTC
* Number of recursive dependencies: 110

Run `revdep_details(, "latentnet")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/nolatent.R’ failed.
    Last 13 lines of output:
          order
      
      > 
      > data(sampson)
      > 
      > monks.nmr<-ergmm(samplike~nodematch("group")+rreceiver)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: ergmm ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning messages:
      1: `set_attrs()` is deprecated as of rlang 0.3.0
      [90mThis warning is displayed once per session.[39m 
      2: 'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# lolog

<details>

* Version: 1.2
* GitHub: https://github.com/statnet/lolog
* Source code: https://github.com/cran/lolog
* Date/Publication: 2019-01-12 22:52:41 UTC
* Number of recursive dependencies: 77

Run `revdep_details(, "lolog")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tests.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("lolog")
      [31m──[39m [31m1. Error: lolog_fit (@test-lolog-fit.R#21) [39m [31m─────────────────────────────────[39m
      could not find function "as.rle"
      [1mBacktrace:[22m
      [90m 1. [39mergm::ergm(samplike ~ edges + nodematch("group"))
      [90m 7. [39mergm:::InitErgmConstraint..attributes(...)
      [90m 8. [39mergm::rlebdm(compact.rle(d), n)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 61 | SKIPPED: 1 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: lolog_fit (@test-lolog-fit.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 35.8Mb
      sub-directories of 1Mb or more:
        libs  33.9Mb
    ```

# networkDynamic

<details>

* Version: 0.10.1
* GitHub: NA
* Source code: https://github.com/cran/networkDynamic
* Date/Publication: 2020-01-21 09:50:02 UTC
* Number of recursive dependencies: 34

Run `revdep_details(, "networkDynamic")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
    
    list.edge.attributes:
      function(x, ...)
    list.edge.attributes.active:
      function(x, onset, terminus, length, at, na.omit, rule, e,
               require.active, active.default, dynamic.only)
    
    list.network.attributes:
      function(x, ...)
    list.network.attributes.active:
      function(x, onset, terminus, length, at, na.omit, rule, dynamic.only)
    
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

# tergm

<details>

* Version: 3.6.1
* GitHub: https://github.com/statnet/tergm
* Source code: https://github.com/cran/tergm
* Date/Publication: 2019-06-12 10:10:18 UTC
* Number of recursive dependencies: 47

Run `revdep_details(, "tergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Draw from the distribution of an Separable Temporal Exponential
    > ###   Family Random Graph Model
    > ### Aliases: simulate.stergm simulate.network simulate.networkDynamic
    > 
    > ### ** Examples
    > 
    > 
    > logit<-function(p)log(p/(1-p))
    > coef.form.f<-function(coef.diss,density) -log(((1+exp(coef.diss))/(density/(1-density)))-1)
    > 
    > # Construct a network with 20 nodes and 20 edges
    > n<-20
    > target.stats<-edges<-20
    > g0<-network.initialize(n,dir=TRUE)
    > g1<-san(g0~edges,target.stats=target.stats,verbose=TRUE)
    Warning: 'compact.rle' is deprecated.
    Use 'compress' instead.
    See help("Deprecated")
    Error in as.rle(x) : could not find function "as.rle"
    Calls: san ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/combine.networks.R’ failed.
    Last 13 lines of output:
      +                   y <- network.initialize(n,dir=FALSE)
      +                   y <- simulate(y~edges, coef=-1)
      +                   y %n% "x" <- matrix(runif(n*n),n,n)
      +                   y %v% "v" <- runif(n)
      +                   y %e% "e" <- runif(network.edgecount(y))
      +                   y
      +                 },
      +                 simplify=FALSE)
      Error in as.rle(x) : could not find function "as.rle"
      Calls: replicate ... ergm_conlist -> eval -> eval -> <Anonymous> -> rlebdm
      In addition: Warning message:
      'compact.rle' is deprecated.
      Use 'compress' instead.
      See help("Deprecated") 
      Execution halted
    ```

# tergmLite

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/tergmLite
* Date/Publication: 2020-07-22 16:50:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(, "tergmLite")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 24 | FAILED: 24 ]
      1. Error: simulate_network (@test-examples.R#16) 
      2. Error: simulate_ergm (@test-examples.R#58) 
      3. Error: init_tergmLite (@test-examples.R#90) 
      4. Error: networkLite (@test-examples.R#123) 
      5. Error: updateModelTermInputs (@test-examples.R#153) 
      6. Error: add_vertices (@test-examples.R#193) 
      7. Error: delete_vertices (@test-examples.R#225) 
      8. Error: concurrent (@test-updateModelTermInputs.R#10) 
      9. Error: concurrent_by (@test-updateModelTermInputs.R#38) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

