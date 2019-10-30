context("network_from_data_frame")

test_that("network_from_data_frame works", {
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
    get.network.attribute(network_from_data_frame(edge_df), "directed")
  )
  expect_false(
    get.network.attribute(network_from_data_frame(edge_df, directed = FALSE), "directed")
  )
  
  # without vertices
  expect_s3_class(
    network_from_data_frame(edge_df),
    "network"
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
    unlist(get.edge.attribute(g, "list_attr", unlist = FALSE), recursive = FALSE),
    edge_df[["list_attr"]]
  )
  expect_identical(
    unlist(get.edge.attribute(g, "df_list_attr", unlist = FALSE), recursive = FALSE),
    edge_df[["df_list_attr"]]
  )
})

test_that("`multiple` arguments work", {
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
})

test_that("`loops` works", {
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
})

test_that("missing vertex names are caught", {
  vertex_df <- data.frame(name = letters[1:5],
                          stringsAsFactors = FALSE)
  
  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e", "f"),
                        to = c("a", "b", "a", "a", "b", "a", "g"),
                        stringsAsFactors = FALSE)
  
  expect_error(
    network_from_data_frame(edge_df, vertices = vertex_df),
    "The following vertices are in `edges`, but not in `vertices`:\n\t- f\n\t- g"
  )
  
})

test_that("duplicate vertex names are caught", {
  vertex_df <- data.frame(name = c("a", "a", "b", "c", "d", "e"),
                          stringsAsFactors = FALSE)
  
  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                        to = c("a", "b", "a", "a", "b", "a"),
                        stringsAsFactors = FALSE)
  
  expect_error(
    network_from_data_frame(edge_df, vertices = vertex_df),
    "The following vertex names are duplicated in `vertices`:\n\t- a"
  )
  
})

test_that("bad data frames are caught", {
  edge_df_with_NAs <- data.frame(from = c(letters, NA),
                            to = c(NA, letters))
  empty_vertex_df <- data.frame()
  
  expect_error(
    network_from_data_frame(edge_df_with_NAs),
    "`edges` contains `NA` elements in its first two columns."
  )
  
  expect_error(
    network_from_data_frame(na.omit(edge_df_with_NAs), vertices = empty_vertex_df),
    "`vertices` should contain at least one column and row."
  )
})

