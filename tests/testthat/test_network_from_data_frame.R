context("network_from_data_frame")

test_that("network_from_data_frame works", {
  vertex_df <- data.frame(name = letters[1:5],
                          int_attr = seq_len(5),
                          chr_attr = LETTERS[1:5],
                          lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE),
                          stringsAsFactors = FALSE)
  vertex_df[["df_list_attr"]] <- replicate(5, mtcars, simplify = FALSE)
  
  edge_df <- data.frame(from = c("b", "c", "c", "d", "d", "e"),
                        to = c("a", "b", "a", "a", "b", "a"),
                        int_attr = seq_len(6),
                        chr_attr = LETTERS[1:6],
                        lgl_attr = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                        stringsAsFactors = FALSE)
  edge_df[["df_list_attr"]] <- replicate(6, mtcars, simplify = FALSE)
  
  g <- network_from_data_frame(edge_df, directed = TRUE, vertices = vertex_df)
  
  # check vertices
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
    get.vertex.attribute(g, "df_list_attr", unlist = FALSE),
    vertex_df[["df_list_attr"]]
  )
  # check edges
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
  expect_identical(
    # get.edge.attribute() returns list attributes nested one level...
    lapply(get.edge.attribute(g, "df_list_attr", unlist = FALSE), `[[`, 1),
    edge_df[["df_list_attr"]]
  )
  
})