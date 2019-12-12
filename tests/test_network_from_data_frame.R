suppressPackageStartupMessages(library(network))
library(testthat)

# context("network_from_data_frame")

# test_that("network_from_data_frame works", {

  vertex_df <- data.frame(name = letters[1:5],
                          int_attr = seq_len(5),
                          chr_attr = LETTERS[1:5],
                          lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE),
                          stringsAsFactors = FALSE)
  vertex_df[["list_attr"]] <- replicate(5, LETTERS, simplify = FALSE)
  vertex_df[["df_list_attr"]] <- replicate(5, mtcars, simplify = FALSE)
  
  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                        to = c("a", "b", "a", "a", "b", "a"),
                        int_attr = seq_len(6),
                        chr_attr = LETTERS[1:6],
                        lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                        stringsAsFactors = FALSE)
  edge_df[["list_attr"]] <- replicate(6, LETTERS, simplify = FALSE)
  edge_df[["df_list_attr"]] <- replicate(6, mtcars, simplify = FALSE)
  
  
  expect_true(
    get.network.attribute(
      network_from_data_frame(edge_df),
      "directed"
    )
  )
  expect_false(
    get.network.attribute(
      network_from_data_frame(edge_df, directed = FALSE), 
      "directed"
    )
  )
  
  # without vertices
  expect_s3_class(
    network_from_data_frame(edge_df),
    "network"
  )
  expect_identical(
    get.vertex.attribute(network_from_data_frame(edge_df), "vertex.names"),
    c("b", "c", "d", "e", "a")
  )
  
  # check vertex and edge attributes =====================================================
  g <- network_from_data_frame(edge_df, vertices = vertex_df)
  
  #* vertices ============================================================================
  expect_identical(
    get.vertex.attribute(g, "vertex.names"),
    vertex_df[["name"]]
  )
  expect_identical(
    get.vertex.attribute(g, "int_attr"),
    vertex_df[["int_attr"]]
  )
  expect_identical(
    get.vertex.attribute(g, "chr_attr"),
    vertex_df[["chr_attr"]]
  )
  expect_identical(
    get.vertex.attribute(g, "lgl_attr"),
    vertex_df[["lgl_attr"]]
  )
  expect_identical(
    get.vertex.attribute(g, "list_attr", unlist = FALSE),
    vertex_df[["list_attr"]]
  )
  expect_identical(
    get.vertex.attribute(g, "df_list_attr", unlist = FALSE),
    vertex_df[["df_list_attr"]]
  )
  #* edges ===============================================================================
  expect_identical(
    get.edge.attribute(g, "int_attr"),
    edge_df[["int_attr"]]
  )
  expect_identical(
    get.edge.attribute(g, "chr_attr"),
    edge_df[["chr_attr"]]
  )
  expect_identical(
    get.edge.attribute(g, "lgl_attr"),
    edge_df[["lgl_attr"]]
  )
  # get.edge.attribute() returns list attributes nested one level...
  expect_identical(
    unlist(
      get.edge.attribute(g, "list_attr", unlist = FALSE),
      recursive = FALSE
    ),
    edge_df[["list_attr"]]
  )
  expect_identical(
    unlist(
      get.edge.attribute(g, "df_list_attr", unlist = FALSE), 
           recursive = FALSE
    ),
    edge_df[["df_list_attr"]]
  )
# })

# test_that("`multiple` arguments work", {
  
  df_with_parallel_edges <- data.frame(from = c("b", "c", "c", "d", "d", "e", "e"),
                                       to = c("a", "b", "a", "a", "b", "a", "a"),
                                       stringsAsFactors = FALSE)
  expect_error(
    network_from_data_frame(df_with_parallel_edges),
    "`multiple` is `FALSE`, but `edges` contains duplicates.\n\t- Index of first duplicate row: 7"  )
  expect_s3_class(
    network_from_data_frame(df_with_parallel_edges, multiple = TRUE),
    "network"
  )

  df_with_parallel_edges2 <- data.frame(from = c("b", "c", "c", "d", "d", "e", "a"),
                                       to = c("a", "b", "a", "a", "b", "a", "e"),
                                       stringsAsFactors = FALSE)
  expect_error(
    network_from_data_frame(df_with_parallel_edges2, directed = FALSE),
    "`multiple` is `FALSE`, but `edges` contains duplicates.\n\t- Index of first duplicate row: 7"  )
  expect_s3_class(
    network_from_data_frame(df_with_parallel_edges2, directed = FALSE, multiple = TRUE),
    "network"
  )
  
# })

# test_that("`loops` works", {
  
  df_with_loops <- data.frame(from = c("b", "c", "c", "d", "d", "e", "f"),
                              to = c("a", "b", "a", "a", "b", "a", "f"),
                              stringsAsFactors = FALSE)
  expect_error(
    network_from_data_frame(df_with_loops),
    "`loops` is `FALSE`, but `edges` contains loops."
  )
  expect_s3_class(
    network_from_data_frame(df_with_loops, loops = TRUE),
    "network"
  )
  
# })

# test_that("missing vertex names are caught", {
  
  vertex_df <- data.frame(name = letters[1:5],
                          stringsAsFactors = FALSE)

  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e", "f"),
                        to = c("a", "b", "a", "a", "b", "a", "g"),
                        stringsAsFactors = FALSE)

  expect_error(
    network_from_data_frame(edge_df, vertices = vertex_df),
    "The following vertices are in `edges`, but not in `vertices`:\n\t- f\n\t- g"
  )

# })

# test_that("duplicate vertex names are caught", {
  
  vertex_df <- data.frame(name = c("a", "a", "b", "c", "d", "e"),
                          stringsAsFactors = FALSE)

  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                        to = c("a", "b", "a", "a", "b", "a"),
                        stringsAsFactors = FALSE)

  expect_error(
    network_from_data_frame(edge_df, vertices = vertex_df),
    "The following vertex names are duplicated in `vertices`:\n\t- a"
  )

# })

# test_that("bad data frames are caught", {
  
  edge_df_with_NAs1 <- data.frame(from = c(letters, NA),
                                  to = c("a", letters))
  edge_df_with_NAs2 <- data.frame(from = c(letters, "a"),
                                  to = c(NA, letters))
  empty_vertex_df <- data.frame()

  expect_error(
    network_from_data_frame(edge_df_with_NAs2),
    "`edges` contains `NA` elements in its first two columns."
  )
  expect_error(
    network_from_data_frame(edge_df_with_NAs2),
    "`edges` contains `NA` elements in its first two columns."
  )

  expect_error(
    network_from_data_frame(na.omit(edge_df_with_NAs1), 
                                     vertices = empty_vertex_df),
    "`vertices` should contain at least one column and row."
  )
  
# })
  
# test_that("non data frames are caught", {
  expect_error(
    network_from_data_frame(as.list(edge_df)),
    "`edges` should be a data frame with at least two columns."
  )
  
  expect_error(
    network_from_data_frame(edge_df, vertices = as.list(vertex_df)),
    "If provided, `vertices` should be a data frame."
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
  
  
  bip_g <- network_from_data_frame(bip_edge_df, vertices = bip_node_df, 
                                   loops = TRUE,
                                   bipartite = TRUE)
  
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
    network_from_data_frame(edges = bip_edge_df, vertices = bip_isolates_node_df,
                            bipartite = TRUE)
    # TODO expect_error() isn't matching this error message
    # "Bipartite networks with isolates are not supported via `network_from_data_frame()` because it's ambiguous whether those vertices should be considered as \"actors\".\nThe following vertex names are in `vertices`, but not in `edges`:\n\t- f\n\t- g"
  )
  
  # check if nodes that appear in both of the first 2 `edge` columns are caught
  bip_confused_edge_df <- data.frame(
    actor = c("a", "a", "b", "b", "c", "d", "d", "e", "e1"),
    event = c("e1", "e2", "e1", "e3", "e3", "e2", "e3", "e1", "e2"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    network_from_data_frame(edges = bip_confused_edge_df, bipartite = TRUE),
    "`bipartite` is `TRUE`, but there are vertex names that appear in both of the first two columns of `edges`.\nThe following vertices appear in both columns:\n\t- e1"
  )
    
# })