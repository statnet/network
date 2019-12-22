suppressPackageStartupMessages(library(network))
library(testthat)

# context("as.network")

# test_that("simple networks are built correctly", {
  simple_edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                               to = c("a", "b", "a", "a", "b", "a"),
                               stringsAsFactors = FALSE)
  simple_vertex_df <- data.frame(name = letters[1:5],
                                 stringsAsFactors = FALSE)
  expect_s3_class(
    as.network(x = simple_edge_df),
    "network"
  )
  expect_s3_class(
    as.network(x = simple_edge_df, vertices = simple_vertex_df),
    "network"
  )
  
  expect_true(
    is.directed(as.network(x = simple_edge_df))
  )
  expect_false(
    is.directed(as.network(x = simple_edge_df, directed = FALSE))
  )
  expect_false(
    has.loops(as.network(x = simple_edge_df))
  )
  expect_false(
    is.multiplex(as.network(x = simple_edge_df))
  )
  
  expect_equal(
    network.edgecount(as.network(x = simple_edge_df)),
    nrow(simple_edge_df)
  )
  expect_equal(
    network.size(as.network(x = simple_edge_df)),
    nrow(simple_vertex_df)
  )
# })

  
# test_that("simple and complex edge/vertex/R-object attributes are safely handled", {
  vertex_df <- data.frame(name = letters[5:1],
                          lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE),
                          int_attr = as.integer(seq_len(5)),
                          dbl_attr = as.double(seq_len(5)),
                          chr_attr = LETTERS[1:5],
                          date_attr = seq.Date(as.Date("2019-12-22"),
                                               as.Date("2019-12-26"),
                                               by = 1),
                          dttm_attr = as.POSIXct(
                            seq.Date(as.Date("2019-12-22"), as.Date("2019-12-26"), by = 1)
                          ),
                          stringsAsFactors = FALSE)
  attr(vertex_df$date_attr, "tzone") <- "PST"
  attr(vertex_df$dttm_attr, "tzone") <- "EST"
  vertex_df$list_attr <- replicate(5, LETTERS, simplify = FALSE)
  vertex_df$mat_list_attr <- replicate(5, as.matrix(mtcars), simplify = FALSE)
  vertex_df$df_list_attr <- replicate(5, mtcars, simplify = FALSE)
  vertex_df$sfg_attr <- list(
    structure(c(1, 2, 3), class = c("XY", "POINT", "sfg")),
    structure(1:10, .Dim = c(5L, 2L), class = c("XY", "MULTIPOINT", "sfg")),
    structure(1:10, .Dim = c(5L, 2L), class = c("XY", "LINESTRING", "sfg")),
    structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                   structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                   structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
              class = c("XY", "MULTILINESTRING", "sfg")),
    structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0),.Dim = c(5L, 2L)), 
                   structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)),
                   structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
              class = c("XY", "POLYGON", "sfg"))
  )
  vertex_df$sfc_attr <- structure(
    list(structure(c(1, 2, 3), class = c("XY", "POINT", "sfg")), 
         structure(1:10, .Dim = c(5L, 2L), class = c("XY", "MULTIPOINT", "sfg")), 
         structure(1:10, .Dim = c(5L, 2L), class = c("XY", "LINESTRING", "sfg")), 
         structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                        structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                        structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                   class = c("XY", "MULTILINESTRING", "sfg")), 
         structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                        structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                        structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                   class = c("XY", "POLYGON", "sfg"))), class = c("sfc_GEOMETRY", "sfc"), 
    precision = 0, bbox = structure(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10), class = "bbox"), 
    crs = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs"), 
    classes = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON"), 
    n_empty = 0L
  )
    
  
  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                        to = c("a", "b", "a", "a", "b", "a"),
                        lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                        int_attr = as.integer(seq_len(6)),
                        dbl_attr = as.double(seq_len(6)),
                        chr_attr = LETTERS[1:6],
                        date_attr = seq.Date(as.Date("2019-12-22"), as.Date("2019-12-27"),
                                             by = 1),
                        dttm_attr = as.POSIXct(
                          seq.Date(as.Date("2019-12-22"), as.Date("2019-12-27"), by = 1)
                        ),
                        stringsAsFactors = FALSE)
  attr(edge_df$date_attr, "tzone") <- "PST"
  attr(edge_df$dttm_attr, "tzone") <- "EST"
  edge_df$list_attr <- replicate(6, LETTERS, simplify = FALSE)
  edge_df$mat_list_attr <- replicate(6, as.matrix(mtcars), simplify = FALSE)
  edge_df$df_list_attr <- replicate(6, mtcars, simplify = FALSE)
  edge_df$sfg_attr <- list(
    structure(c(1, 2, 3), class = c("XY", "POINT", "sfg")),
    structure(1:10, .Dim = c(5L, 2L), class = c("XY", "MULTIPOINT", "sfg")),
    structure(1:10, .Dim = c(5L, 2L), class = c("XY", "LINESTRING", "sfg")),
    structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                   structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                   structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
              class = c("XY", "MULTILINESTRING", "sfg")),
    structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0),.Dim = c(5L, 2L)), 
                   structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)),
                   structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
              class = c("XY", "POLYGON", "sfg")),
    structure(list(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                        structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                        structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                   list(structure(c(12, 22, 22, 12, 12, 12, 12, 22, 22, 12), .Dim = c(5L, 2L)), 
                        structure(c(13, 13, 14, 14, 13, 13, 14, 14, 13, 13), .Dim = c(5L, 2L))), 
                   list(structure(c(24, 34, 34, 24, 24, 24, 24, 34, 34, 24), .Dim = c(5L, 2L)))), 
              class = c("XY", "MULTIPOLYGON", "sfg"))
  )
  edge_df$sfc_attr <- structure(
    list(
      structure(c(1, 2, 3), class = c("XY", "POINT", "sfg")), 
      structure(1:10, .Dim = c(5L, 2L), class = c("XY", "MULTIPOINT", "sfg")), 
      structure(1:10, .Dim = c(5L, 2L), class = c("XY", "LINESTRING", "sfg")), 
      structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                     structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                     structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                class = c("XY", "MULTILINESTRING", "sfg")), 
      structure(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                     structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                     structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                class = c("XY", "POLYGON", "sfg")), 
      structure(list(list(structure(c(0, 10, 10, 0, 0, 0, 0, 10, 10, 0), .Dim = c(5L, 2L)), 
                          structure(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1), .Dim = c(5L, 2L)), 
                          structure(c(5, 5, 6, 6, 5, 5, 6, 6, 5, 5), .Dim = c(5L, 2L))), 
                     list(structure(c(12, 22, 22, 12, 12, 12, 12, 22, 22, 12), .Dim = c(5L, 2L)), 
                          structure(c(13, 13, 14, 14, 13, 13, 14, 14, 13, 13), .Dim = c(5L, 2L))), 
                     list(structure(c(24, 34, 34, 24, 24, 24, 24, 34, 34, 24), .Dim = c(5L, 2L)))), 
                class = c("XY", "MULTIPOLYGON", "sfg"))), 
    class = c("sfc_GEOMETRY", "sfc"), precision = 0, 
    bbox = structure(c(xmin = 0, ymin = 0, xmax = 34, ymax = 34), class = "bbox"), 
    crs = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs"), 
    classes = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"),
    n_empty = 0L
  )
  
  g_many_attrs <- as.network(edge_df, vertices = vertex_df)
  
  # edge attributes ======================================================================
  # bare atomic vectors
  expect_identical(
    get.edge.attribute(g_many_attrs, "lgl_attr"),
    edge_df$lgl_attr
  )
  expect_identical(
    get.edge.attribute(g_many_attrs, "int_attr"),
    edge_df$int_attr
  )
  expect_identical(
    get.edge.attribute(g_many_attrs, "dbl_attr"),
    edge_df$dbl_attr
  )
  expect_identical(
    get.edge.attribute(g_many_attrs, "chr_attr"),
    edge_df$chr_attr
  )
  # atomic vectors w/ attributes
  # TODO is there a way to get atomic vectors back while preserving attributes?
  # `c()` `v/sapply()` strip attributes
  edge_date_attr <- get.edge.attribute(g_many_attrs, "date_attr", unlist = FALSE)
  edge_date_attr_to_test <- `attributes<-`(unlist(edge_date_attr),
                                           attributes(edge_date_attr[[1]]))
  expect_identical(
    edge_date_attr_to_test,
    edge_df$date_attr
    
  )
  edge_dttm_attr <- get.edge.attribute(g_many_attrs, "dttm_attr", unlist = FALSE)
  edge_dttm_attr_to_test <- `attributes<-`(unlist(edge_dttm_attr),
                                           attributes(edge_dttm_attr[[1]]))
  expect_identical(
    edge_dttm_attr_to_test,
    edge_df$dttm_attr
  )
  # list of bare atomic vectors
  expect_identical(
    get.edge.attribute(g_many_attrs, "list_attr", unlist = FALSE),
    edge_df$list_attr
  )
  # list of vectors with attributes
  expect_identical(
    get.edge.attribute(g_many_attrs, "mat_list_attr", unlist = FALSE),
    edge_df$mat_list_attr
  )
  # recursive lists
  expect_identical(
    get.edge.attribute(g_many_attrs, "df_list_attr", unlist = FALSE),
    edge_df$df_list_attr
  )
  # sf objects
  expect_identical(
    get.edge.attribute(g_many_attrs, "sfg_attr", unlist = FALSE),
    edge_df$sfg_attr
  )
  expect_identical(
    unlist(get.edge.attribute(g_many_attrs, "sfc_attr", unlist = FALSE), 
           recursive = FALSE),
    unlist(edge_df$sfc_attr, recursive = FALSE)
  )
  
  # sf-based spatial network package will likely require setting a global CRS
  # via a network-level attribute
  #  - ignoring CRS, reconstructing the edge geometry/sfc column works like this:
  #   - sf::st_sfc(get.edge.attribute(g_many_attrs, "sfc_attr", unlist = FALSE))
  # expect_identical(
  #   sf::st_sfc(get.edge.attribute(g_many_attrs, "sfc_attr", unlist = FALSE)),
  #   edge_df$sfc_attr
  # )

  # vertex attributes ====================================================================
  # bare atomic vectors
  expect_identical(
    get.vertex.attribute(g_many_attrs, "vertex.names"),
    vertex_df[[1]]
  )
  expect_identical(
    get.vertex.attribute(g_many_attrs, "lgl_attr"),
    vertex_df$lgl_attr
  )
  expect_identical(
    get.vertex.attribute(g_many_attrs, "int_attr"),
    vertex_df$int_attr
  )
  expect_identical(
    get.vertex.attribute(g_many_attrs, "dbl_attr"),
    vertex_df$dbl_attr
  )
  expect_identical(
    get.vertex.attribute(g_many_attrs, "chr_attr"),
    vertex_df$chr_attr
  )
  # atomic vectors w/ attributes
  # TODO is there a way to get atomic vectors back while preserving attributes?
  # `c()` `v/sapply()` strip attributes
  vertex_date_attr <- get.vertex.attribute(g_many_attrs, "date_attr", unlist = FALSE)
  vertex_date_attr_to_test <- `attributes<-`(unlist(vertex_date_attr),
                                             attributes(vertex_date_attr[[1]]))
  expect_identical(
    vertex_date_attr_to_test,
    vertex_df$date_attr
    
  )
  vertex_dttm_attr <- get.vertex.attribute(g_many_attrs, "dttm_attr", unlist = FALSE)
  vertex_dttm_attr_to_test <- `attributes<-`(unlist(vertex_dttm_attr),
                                             attributes(vertex_dttm_attr[[1]]))
  expect_identical(
    vertex_dttm_attr_to_test,
    vertex_df$dttm_attr
  )
  # list of bare atomic vectors
  expect_identical(
    get.vertex.attribute(g_many_attrs, "list_attr", unlist = FALSE),
    vertex_df$list_attr
  )
  # list of vectors with attributes
  expect_identical(
    get.vertex.attribute(g_many_attrs, "mat_list_attr", unlist = FALSE),
    vertex_df$mat_list_attr
  )
  # recursive lists
  expect_identical(
    get.vertex.attribute(g_many_attrs, "df_list_attr", unlist = FALSE),
    vertex_df$df_list_attr
  )
  # sf objects
  expect_identical(
    get.vertex.attribute(g_many_attrs, "sfg_attr", unlist = FALSE),
    vertex_df$sfg_attr
  )
  expect_identical(
    unlist(get.vertex.attribute(g_many_attrs, "sfc_attr", unlist = FALSE), 
           recursive = FALSE),
    unlist(vertex_df$sfc_attr, recursive = FALSE)
  )
  
  # sf-based spatial network package will likely require setting a global CRS
  # via a network-level attribute
  #  - ignoring CRS, reconstructing the vertex geometry/sfc column works like this:
  #   - sf::st_sfc(get.vertex.attribute(g_many_attrs, "sfc_attr", unlist = FALSE))
  # expect_identical(
  #   sf::st_sfc(get.vertex.attribute(g_many_attrs, "sfc_attr", unlist = FALSE)),
  #   vertex_df$sfc_attr
  # )

# })


# test_that("`multiple` arguments work", {
  dir_parallel_edge_df <- data.frame(from = c("a", "a"),
                                     to = c("b", "b"),
                                     stringsAsFactors = FALSE)
  expect_error(
    as.network(dir_parallel_edge_df),
    "`multiple` is `FALSE`, but `x` contains parallel edges."
  )
  expect_s3_class(
    as.network(dir_parallel_edge_df, multiple = TRUE),
    "network"
  )
  expect_true(
    is.multiplex(as.network(dir_parallel_edge_df, multiple = TRUE))
  )
  expect_true(
    is.directed(as.network(dir_parallel_edge_df, multiple = TRUE))
  )

  undir_parallel_edge_df <- data.frame(from = c("a", "b"),
                                       to = c("b", "a"),
                                       stringsAsFactors = FALSE)
  expect_s3_class(
    as.network(undir_parallel_edge_df),
    "network"
  )
  expect_error(
    as.network(undir_parallel_edge_df, directed = FALSE),
    "`multiple` is `FALSE`, but `x` contains parallel edges."
  )
  expect_s3_class(
    as.network(undir_parallel_edge_df, directed = FALSE, multiple = TRUE),
    "network"
  )
  expect_true(
    is.multiplex(as.network(undir_parallel_edge_df, directed = FALSE, multiple = TRUE))
  )
  expect_false(
    is.directed(as.network(undir_parallel_edge_df, directed = FALSE, multiple = TRUE))
  )
# })

# test_that("`loops` works", {
  df_with_loops <- data.frame(from = c("b", "c", "c", "d", "d", "e", "f"),
                              to = c("a", "b", "a", "a", "b", "a", "f"),
                              stringsAsFactors = FALSE)
  expect_error(
    as.network(df_with_loops),
    "`loops` is `FALSE`"
  )
  expect_s3_class(
    as.network(df_with_loops, loops = TRUE),
    "network"
  )
# })

# test_that("missing vertex names are caught", {
  missing_vertex_df <- data.frame(name = letters[1:5],
                                  stringsAsFactors = FALSE)

  missing_edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e", "f"),
                                to = c("a", "b", "a", "a", "b", "a", "g"),
                                stringsAsFactors = FALSE)

  expect_error(
    as.network(missing_edge_df, vertices = missing_vertex_df),
    "The following vertices are in `x`, but not in `vertices`:\n\t- f\n\t- g", fixed = TRUE
  )
# })

# test_that("duplicate vertex names are caught", {
  dup_vertex_df <- data.frame(name = c("a", "a", "b", "c", "d", "e"),
                              stringsAsFactors = FALSE)

  dup_edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                            to = c("a", "b", "a", "a", "b", "a"),
                            stringsAsFactors = FALSE)

  expect_error(
    as.network(dup_edge_df, vertices = dup_vertex_df),
    "The following vertex names are duplicated in `vertices`:\n\t- a", fixed = TRUE
  )
# })

# test_that("bad data frames are caught", {
  edge_df_with_NAs1 <- data.frame(from = c(letters, NA),
                                  to = c("a", letters),
                                  stringsAsFactors = FALSE)
  edge_df_with_NAs2 <- data.frame(from = c(letters, "a"),
                                  to = c(NA, letters),
                                  stringsAsFactors = FALSE)
  empty_vertex_df <- data.frame()

  expect_error(
    as.network(edge_df_with_NAs2),
    "The first two columns of `x` cannot contain `NA` values.", fixed = TRUE
  )
  expect_error(
    as.network(edge_df_with_NAs2),
    "The first two columns of `x` cannot contain `NA` values.", fixed = TRUE
  )

  expect_error(
    as.network(na.omit(edge_df_with_NAs1), vertices = empty_vertex_df, loops = TRUE),
    "`vertices` should contain at least one column and row.", fixed = TRUE
  )
# })


# test_that("bipartite networks work", {
  bip_edge_df <- data.frame(actor = c("a", "a", "b", "b", "c", "d", "d", "e"),
                            event = c("e1", "e2", "e1", "e3", "e3", "e2", "e3", "e1"),
                            an_edge_attr = letters[1:8],
                            stringsAsFactors = FALSE)
  bip_node_df <- data.frame(node_id = c("a", "e1", "b", "e2", "c", "e3", "d", "e"),
                            node_type = c("person", "event", "person", "event", "person",
                                          "event", "person", "person"),
                            color = c("red", "blue", "red", "blue", "red", "blue",
                                      "red", "red"),
                            stringsAsFactors = FALSE)
  
  expect_warning(
    as.network(bip_edge_df, vertices = bip_node_df, 
               bipartite = TRUE),
    "If `bipartite` is `TRUE`, edges are interpreted as undirected.", fixed = TRUE
  )
  
  expect_warning(
    as.network(bip_edge_df, directed = FALSE, vertices = bip_node_df, 
               bipartite = TRUE, loops = TRUE),
    "If `bipartite` is `TRUE`, `loops` must be `FALSE`.", fixed = TRUE
  )

  bip_g <- as.network(bip_edge_df, directed = FALSE, vertices = bip_node_df, 
                      loops = FALSE, bipartite = TRUE)
  
  expect_s3_class(
    bip_g,
    "network"
  )
  
  expect_true(
    is.bipartite(bip_g)
  )
  expect_false(
    has.loops(bip_g)
  )
  expect_false(
    is.directed(bip_g)
  )
  
  expect_identical(
    get.network.attribute(bip_g, "bipartite"),
    5L
  )
  
  expect_identical(
    get.vertex.attribute(bip_g, attrname = "node_type"),
    c(rep("person", 5), rep("event", 3))
  )
  
  expect_identical(
    get.vertex.attribute(bip_g, attrname = "vertex.names"),
    c("a", "b", "c", "d", "e", "e1", "e2", "e3")
  )
  
  expect_identical(
    get.edge.attribute(bip_g, attrname = "an_edge_attr"),
    letters[1:8]
  )
  
  # check if bipartite networks with isolates are caught 
  bip_isolates_node_df <- data.frame(
    node_id = c("a", "e1", "b", "e2", "c", "e3", "d", "e", "f", "g"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    as.network(x = bip_edge_df, directed = FALSE, vertices = bip_isolates_node_df,
               bipartite = TRUE),
    "`bipartite` is `TRUE`, but the `vertices` you provided contain names that are not present in `x`"
  )
  
  bip_isolates_node_df$is_actor <- grepl("^e\\d$", bip_isolates_node_df$node_id)
  expect_s3_class(
    as.network(x = bip_edge_df, directed = FALSE, vertices = bip_isolates_node_df,
               bipartite = TRUE),
    "network"
  )
  
  # check if nodes that appear in both of the first 2 `edge` columns are caught
  bip_confused_edge_df <- data.frame(
    actor = c("a", "a", "b", "b", "c", "d", "d", "e", "e1"),
    event = c("e1", "e2", "e1", "e3", "e3", "e2", "e3", "e1", "e2"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    as.network(x = bip_confused_edge_df, directed = FALSE, bipartite = TRUE),
    "`bipartite` is `TRUE`, but there are vertices that appear in both of the first two columns of `x`."
  )
# })

# test_that("hyper-edges work", {
  hyper_edge_df <- data.frame(
    from = I(list(1:4, 3:5, 4:7, 6:10)),
    to = I(list(1:4, 3:5, 4:7, 6:10)),
    value = as.double(5:8)
  )
  
  hyper_target_net <- network.initialize(10, directed = FALSE, hyper = TRUE, loops = TRUE)
  hyper_target_net <- add.edge(hyper_target_net, 1:4, 1:4, "value", list(5))
  hyper_target_net <- add.edge(hyper_target_net, 3:5, 3:5, "value", list(6))
  hyper_target_net <- add.edge(hyper_target_net, 4:7, 4:7, "value", list(7))
  hyper_target_net <- add.edge(hyper_target_net, 6:10, 6:10, "value", list(8))
  
  expect_identical(
    as.network(hyper_edge_df, directed = FALSE, hyper = TRUE, loops = TRUE),
    hyper_target_net
  )
  
  
  MtSHbyloc_edge_df <- data.frame(
    from = I(
      list(
        c(1, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27), 
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 26, 27)
      )
    ), 
    to = I(
      list(
        c(1, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27), 
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 26, 27)
      )
    )
  )
  MtSHbyloc_edge_df[] <- lapply(MtSHbyloc_edge_df, lapply, as.integer)
  MtSHbyloc_vertex_df <- data.frame(
    node_id = 1:27
  )
  
  data("emon")
  MtSHloc <- emon$MtStHelens %v% "Location"
  MtSHimat <- cbind(MtSHloc %in% c("L", "B"), MtSHloc %in% c("NL", "B"))
  MtSHbyloc <- network(MtSHimat, matrix = "incidence", hyper = TRUE,
                       directed = FALSE, loops = TRUE)
  expect_identical(
    as.network(MtSHbyloc_edge_df, directed = FALSE, vertices = MtSHbyloc_vertex_df,
               loops = TRUE, hyper = TRUE),
    MtSHbyloc
  )
  
  hyper_edges_with_NA <- data.frame(
    from = I(list(c(NA, "a", "b"))),
    to = I(list(c("c", "d")))
  )
  expect_error(
    as.network(hyper_edges_with_NA, hyper = TRUE),
    "`x`'s first two columns contain invalid values."
  )
# })  

# benchmarks =============================================================================
# suppressPackageStartupMessages(library(network))
# set.seed(831)
# 
# X <- seq_len(5000)
# 
# big_edge_df <- tibble::tibble(
#   from = sample(X),
#   to = sample(X),
#   attr = sample(letters, size = length(X), replace = TRUE)
# )
# big_node_df <- tibble::tibble(			
#   name = X,			
#   vattr = sample(letters, size = length(X), replace = TRUE),
# )
# 
# hyper_sources <- replicate(length(X), sample(X, size = 5), simplify = F)			
# hyper_targets <- replicate(length(X), sample(X, size = 5), simplify = F)
# 
# big_hyper_edge_df <- tibble::tibble(
#   from = hyper_sources,			
#   to = hyper_targets,			
#   attr = sample(letters, size = length(X), replace = TRUE)
# )			
# big_hyper_node_df <- tibble::tibble(			
#   name = X,			
#   vattr = sample(letters, size = length(X), replace = TRUE),			
# )
# 
# dplyr::glimpse(
#   bench::mark(
#     big_net = as.network(big_edge_df, vertices = big_node_df, loops = TRUE),
#     iterations = 5, filter_gc = FALSE
#   )
# )
# #> Observations: 1
# #> Variables: 13
# #> $ expression <bch:expr> [as.network(big_edge_df, vertices = big_node_df, loop…
# #> $ min        <bch:tm> 70.3ms
# #> $ median     <bch:tm> 104ms
# #> $ `itr/sec`  <dbl> 9.748656
# #> $ mem_alloc  <bch:byt> 194MB
# #> $ `gc/sec`   <dbl> 52.64274
# #> $ n_itr      <int> 5
# #> $ n_gc       <dbl> 27
# #> $ total_time <bch:tm> 513ms
# #> $ result     <list> [<2883, 837, w, FALSE, 3028, 4737, c, FALSE, 251, 186, c,…
# #> $ memory     <list> [<Rprofmem[11770 x 3]>]
# #> $ time       <list> [<104ms, 104.2ms, 97.1ms, 137.3ms, 70.3ms>]
# #> $ gc         <list> [<tbl_df[5 x 3]>]
# 
# dplyr::glimpse(
#   bench::mark(
#     big_hypernet = as.network(big_hyper_edge_df, vertices = big_hyper_node_df,
#                               hyper = TRUE, loops = TRUE),
#   iterations = 5, filter_gc = FALSE
#   )
# )
# #> Observations: 1
# #> Variables: 13
# #> $ expression <bch:expr> [<as.network(big_hyper_edge_df, vertices = big_hyper_…
# #> $ min        <bch:tm> 674ms
# #> $ median     <bch:tm> 710ms
# #> $ `itr/sec`  <dbl> 1.426628
# #> $ mem_alloc  <bch:byt> 820MB
# #> $ `gc/sec`   <dbl> 30.52985
# #> $ n_itr      <int> 5
# #> $ n_gc       <dbl> 107
# #> $ total_time <bch:tm> 3.5s
# #> $ result     <list> [<3823, 119, 1694, 688, 3452, 3061, 2332, 1978, 2172, 407…
# #> $ memory     <list> [<Rprofmem[21767 x 3]>]
# #> $ time       <list> [<718ms, 674ms, 682ms, 710ms, 720ms>]
# #> $ gc         <list> [<tbl_df[5 x 3]>]

