#' @importFrom statnet.common once
.warn_bipartite_vertex_reorder <- once(
  function() {
    warning(
      "`vertices` were not provided in the order required for bipartite networks. Reordering.",
      "\n\nThis is the first and last time you will be warned during this session.",
      call. = FALSE
    )
  }
)


.head <- function(x, n = 6) {
  n <- min(length(x), n)
  x[seq_len(n)]
}

.validate_edge_df <- function(edges, directed, hyper, loops, multiple, bipartite, ...) {
  # confirm edge data frame has valid dimensions
  if (ncol(edges) < 2L || nrow(edges) == 0L) {
    stop(
      "`x` should be a data frame with at least two columns and one row.",
      call. = FALSE
    )
  }

  el <- edges[, 1:2]
  sources <- edges[[1L]]
  targets <- edges[[2L]]

  # validate edge column types
  if (hyper) {
    # confirm that hyper-edges are list columns
    if (!is.list(sources) || !is.list(targets)) {
      stop(
        "If `hyper` is `TRUE`, the first two columns of `x` should be list columns.",
        call. = FALSE
      )
    }
    # first edge type is the `target_type`, against which all other values are tested
    target_type <- typeof(sources[[1L]])
    # confirm that target_type is itself valid
    if (any(is.na(sources[[1L]])) || target_type %in% c("NULL", "list")) {
      stop(
        "`x`'s first two columns contain invalid values.",
        "\n\t- `x[[1]][[1]]` is `NULL`, recursive, or it contains `NA` values.",
        call. = FALSE
      )
    }
    # Iterate through edge columns, testing that they're not `NA` and are of the same type
    # as `target_type`. `incompat_types` is a logical matrix of the test results.
    incompat_types <- vapply(
      el, function(.x) {
        vapply(.x, function(.y) any(is.na(.y)) || typeof(.y) != target_type, logical(1L))
      },
      logical(nrow(el))
    )
    # if any values are incompatible, throw error pointing user to the problem values
    if (any(incompat_types)) {
      incompat_rows <- row(incompat_types)[incompat_types]
      incompat_cols <- col(incompat_types)[incompat_types]
      stop(
        "The values in the first two columns of `x` must be of the same type and cannot be `NULL`, `NA`, or recursive values.",
        "\nThe following values are incompatible:",
        paste(
          "\n\t-",
          sprintf("`x[%d, %d]`", .head(incompat_rows), .head(incompat_cols))
        ),
        call. = FALSE
      )
    }
  } else { # for non-hyper edges...
    # ... confirm edge columns are atomic vectors
    if (!is.atomic(sources) || !is.atomic(targets)) {
      stop(
        "If `hyper` is `FALSE`, the first two columns of `x` should be atomic vectors.",
        call. = FALSE
      )
    }
    # confirm that edge columns are of the same type
    if (typeof(sources) != typeof(targets)) {
      stop(
        "The first two columns of `x` must be of the same type.",
        call. = FALSE
      )
    }
    # confirm edge columns don't contain `NA`s
    if (any(is.na(el))) {
      stop(
        "The first two columns of `x` cannot contain `NA` values.",
        call. = FALSE
      )
    }
  }

  # if `loops` is `FALSE`, confirm that edge columns don't contain loops
  if (!loops) {
    # if hyper, test if each intersection's length is not 0
    if (hyper) {
      loop_rows <- which(
        mapply(
          function(.x, .y) length(intersect(.x, .y)) != 0L,
          sources, targets,
          USE.NAMES = FALSE
        )
      )
    } else { # if not hyper...
      # ... test via simple vector comparison
      loop_rows <- which(sources == targets)
    }
    # if loops are found, throw error pointing user to the edge rows that contain them
    if (length(loop_rows) > 0L) {
      stop(
        "`loops` is `FALSE`, but `x` contains loops.",
        "\nThe following values are affected:",
        paste("\n\t-", sprintf("`x[%d, 1:2]`", .head(loop_rows))),
        call. = FALSE
      )
    }
  }

  # TODO does network support bipartite hypergraphs?
  if (!hyper && bipartite) {
    # check for intersection between edge columns
    confused_nodes <- intersect(sources, targets)
    # if there's an intersection, throw error informing users which nodes are in both columns
    if (length(confused_nodes) > 0L) {
      stop(
        "`bipartite` is `TRUE`, but there are vertices that appear in both of the",
        " first two columns of `x`.\n",
        "The following vertices appear in both columns:",
        paste("\n\t-", .head(confused_nodes)),
        call. = FALSE
      )
    }
  }

  # TODO does network support multiplex hypergraphs?
  if (!hyper && !multiple) {
    if (directed) {
      test_el <- el
    } else {
      test_el <- t(apply(el, 1L, sort))
    }

    if (anyDuplicated(test_el) != 0L) {
      parallel_edges <- which(duplicated(test_el))
      stop(
        "`multiple` is `FALSE`, but `x` contains parallel edges.\n",
        "The following rows in `x` are duplicated:",
        paste("\n\t-", sprintf("`x[%d, ]`", .head(parallel_edges))),
        call. = FALSE
      )
    }
  }
}


.validate_vertex_df <- function(vertices, el_vert_ids) {
  # confirm `vertices` is a data frame
  if (!is.data.frame(vertices)) {
    stop(
      "If provided, `vertices` should be a data frame.",
      call. = FALSE
    )
  }
  # confirm `vertices` has valid dimensions
  if (nrow(vertices) == 0L || ncol(vertices) == 0L) {
    stop(
      "`vertices` should contain at least one column and row.",
      call. = FALSE
    )
  }

  vertex_ids <- vertices[[1L]]
  if (!is.atomic(vertex_ids)) {
    stop(
      "The first column of `vertices` must be an atomic vector.",
      call. = FALSE
    )
  }
  # confirm vertex IDs match type used in edges
  if (typeof(vertex_ids) != typeof(el_vert_ids)) {
    stop(
      "The first column of `vertices` must be the same type as the value with which",
      " they are referenced in `x`'s first two columns.",
      call. = FALSE
    )
  }
  # check for vertex names that are in the edges, but are missing from `vertices`
  missing_vertex_names <- setdiff(el_vert_ids, vertex_ids)
  if (length(missing_vertex_names) != 0L) {
    stop(
      "The following vertices are in `x`, but not in `vertices`:",
      paste("\n\t-", .head(missing_vertex_names)),
      call. = FALSE
    )
  }
  # check if any of the `vertices` have duplicate names
  if (anyDuplicated(vertex_ids) != 0L) {
    stop(
      "The following vertex names are duplicated in `vertices`:",
      paste("\n\t-", .head(vertex_ids[duplicated(vertex_ids)])),
      call. = FALSE
    )
  }
}


.prep_bipartite_vertices <- function(vertices, el_vert_ids, bipartite_col) {
  # use "is_actor" column if provided
  if (bipartite_col %in% names(vertices)) {
    # check if `"is_actor"` column is valid
    if (!is.logical(vertices[[bipartite_col]]) || any(is.na(vertices[[bipartite_col]]))) {
      stop(
        sprintf(
          paste0(
          '`bipartite` is `TRUE` and vertex types are specified via a column in `vertices` named `"%s"`.',
          '\n\t- If provided, all values in `vertices[["%s"]]` must be `TRUE` or `FALSE`.'
          ),
          bipartite_col, bipartite_col
        )
      )
    }
    # actors (`TRUE`) go before non-actors (`FALSE`)
    vertex_order <- order(vertices[[bipartite_col]], decreasing = TRUE)
  } else { # if no "is_actor" column is provided...
    vertex_ids <- vertices[[1L]]
    # ... check for isolates...
    isolates <- setdiff(vertex_ids, el_vert_ids)
    # ... and throw error informing user of which vertices are isolates
    if (length(isolates) > 0L) {
      stop(
        sprintf(
          "`bipartite` is `TRUE`, but the `vertices` you provided contain names that are not present in `x` (i.e. you have isolates).",
          "\nIf you have isolates, `vertices` must have a `logical` column named \"%s\" indicating each vertex's type.",
          "\nThe following vertex names are in `vertices`, but not in `x`:",
          bipartite_col
        ),
        paste("\n\t-", .head(isolates))
      )
    }
    # if there are no isolates, follow order of vertices as they appear in the edges
    vertex_order <- match(el_vert_ids, vertex_ids)
  }

  if (!identical(vertices[[1L]], vertices[[1L]][vertex_order])) {
    .warn_bipartite_vertex_reorder()
  }
  # reorder the vertex rows to match the actor/non-actor order of the final network
  vertices[vertex_order, ]
}


.distribute_vec_attrs <- function(x) {
  lapply(x, function(.x) {
    if (is.atomic(.x)) {
      lapply(.x, `attributes<-`, attributes(.x))
    } else {
      .x
    }
  })
}

.prep_edge_attrs <- function(edges) {
  edge_attr_names <- names(edges)[-(1:2)]

  init_vals_eval <- .distribute_vec_attrs(edges[, edge_attr_names, drop = FALSE])

  list(
    names_eval = rep(list(as.list(edge_attr_names)), times = nrow(edges)),
    vals_eval = .mapply(list, init_vals_eval, NULL)
  )
}

.prep_vertex_attrs <- function(vertices) {
  vertices[-1L] <- .distribute_vec_attrs(vertices[-1L])
  vertices
}


#' @rdname network
#'
#' @param vertices If \code{x} is a \code{data.frame}, \code{vertices} is an optional
#' \code{data.frame} containing the vertex attributes. The first column is assigned
#' to the \code{"vertex.names"} and additional columns are used to set vertex attributes
#' using their column names. If \code{bipartite} is \code{TRUE}, a \code{logical} column
#' named \code{"is_actor"} (or the name of a column specified using the
#' \code{bipartite_col} parameter) can be provided indicating which vertices
#' should be considered as actors. If not provided, vertices referenced in the
#' first column of \code{x} are assumed to be the network's actors. If your
#' network has isolates (i.e. there are vertices referenced in \code{vertices}
#' that are not referenced in \code{x}), the \code{"is_actor"} column is required.
#'
#' @param bipartite_col \code{character(1L)}, default: \code{"is_actor"}.
#' The name of the \code{logical} column indicating which vertices should be
#' considered as actors in bipartite networks.
#'
#' @examples
#' # networks from data frames ===========================================================
#' #* simple networks ====================================================================
#' simple_edge_df <- data.frame(
#'   from = c("b", "c", "c", "d", "a"),
#'   to = c("a", "b", "a", "a", "b"),
#'   weight = c(1, 1, 2, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#' simple_edge_df
#'
#' as.network(simple_edge_df)
#'
#' # simple networks with vertices =======================================================
#' simple_vertex_df <- data.frame(
#'   name = letters[1:5],
#'   residence = c("urban", "rural", "suburban", "suburban", "rural"),
#'   stringsAsFactors = FALSE
#' )
#' simple_vertex_df
#'
#' as.network(simple_edge_df, vertices = simple_vertex_df)
#'
#' as.network(simple_edge_df,
#'   directed = FALSE, vertices = simple_vertex_df,
#'   multiple = TRUE
#' )
#'
#' #* splitting multiplex data frames into multiple networks =============================
#' simple_edge_df$relationship <- c(rep("friends", 3), rep("colleagues", 2))
#' simple_edge_df
#'
#' lapply(split(simple_edge_df, f = simple_edge_df$relationship),
#'   as.network,
#'   vertices = simple_vertex_df
#' )
#'
#' #* bipartite networks without isolates ================================================
#' bip_edge_df <- data.frame(
#'   actor = c("a", "a", "b", "b", "c", "d", "d", "e"),
#'   event = c("e1", "e2", "e1", "e3", "e3", "e2", "e3", "e1"),
#'   actor_enjoyed_event = rep(c(TRUE, FALSE), 4),
#'   stringsAsFactors = FALSE
#' )
#' bip_edge_df
#'
#' bip_node_df <- data.frame(
#'   node_id = c("a", "e1", "b", "e2", "c", "e3", "d", "e"),
#'   node_type = c(
#'     "person", "event", "person", "event", "person",
#'     "event", "person", "person"
#'   ),
#'   color = c(
#'     "red", "blue", "red", "blue", "red", "blue",
#'     "red", "red"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' bip_node_df
#'
#' as.network(bip_edge_df, directed = FALSE, bipartite = TRUE)
#' as.network(bip_edge_df, directed = FALSE, vertices = bip_node_df, bipartite = TRUE)
#'
#' #* bipartite networks with isolates ===================================================
#' bip_nodes_with_isolates <- rbind(
#'   bip_node_df,
#'   data.frame(
#'     node_id = c("f", "e4"),
#'     node_type = c("person", "event"),
#'     color = c("red", "blue"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' # indicate which vertices are actors via a column named `"is_actor"`
#' bip_nodes_with_isolates$is_actor <- bip_nodes_with_isolates$node_type == "person"
#' bip_nodes_with_isolates
#'
#' as.network(bip_edge_df,
#'   directed = FALSE, vertices = bip_nodes_with_isolates,
#'   bipartite = TRUE
#' )
#'
#' #* hyper networks from data frames ====================================================
#' hyper_edge_df <- data.frame(
#'   from = c("a/b", "b/c", "c/d/e", "d/e"),
#'   to = c("c/d", "a/b/e/d", "a/b", "d/e"),
#'   time = 1:4,
#'   stringsAsFactors = FALSE
#' )
#' tibble::as_tibble(hyper_edge_df)
#'
#' # split "from" and "to" at `"/"`, coercing them to list columns
#' hyper_edge_df$from <- strsplit(hyper_edge_df$from, split = "/")
#' hyper_edge_df$to <- strsplit(hyper_edge_df$to, split = "/")
#' tibble::as_tibble(hyper_edge_df)
#'
#' as.network(hyper_edge_df,
#'   directed = FALSE, vertices = simple_vertex_df,
#'   hyper = TRUE, loops = TRUE
#' )
#'
#' # convert network objects back to data frames =========================================
#' simple_g <- as.network(simple_edge_df, vertices = simple_vertex_df)
#' as.data.frame(simple_g)
#' as.data.frame(simple_g, unit = "vertices")
#'
#' bip_g <- as.network(bip_edge_df,
#'   directed = FALSE, vertices = bip_node_df,
#'   bipartite = TRUE
#' )
#' as.data.frame(bip_g)
#' as.data.frame(bip_g, unit = "vertices")
#'
#' hyper_g <- as.network(hyper_edge_df,
#'   directed = FALSE, vertices = simple_vertex_df,
#'   hyper = TRUE, loops = TRUE
#' )
#' as.data.frame(hyper_g)
#' as.data.frame(hyper_g, unit = "vertices")
#' @export as.network.data.frame
#' @export
as.network.data.frame <- function(x,
                                  directed = TRUE,
                                  vertices = NULL,
                                  hyper = FALSE,
                                  loops = FALSE,
                                  multiple = FALSE,
                                  bipartite = FALSE,
                                  bipartite_col = "is_actor",
                                  ...) {
  # validate network type args
  invalid_network_args <- vapply(
    list(
      directed = directed, hyper = hyper, loops = loops,
      multiple = multiple, bipartite = bipartite
    ),
    function(.x) is.na(.x) || !is.logical(.x),
    logical(1L)
  )
  if (any(invalid_network_args)) {
    stop(
      "The following arguments must be either `TRUE` or `FALSE`:",
      paste("\n\t-", names(invalid_network_args)[invalid_network_args])
    )
  }

  if (length(bipartite_col) != 1L || !is.character(bipartite_col) || is.na(bipartite_col)) {
    stop("`bipartite_col` must be a single, non-`NA` `character` value.")
  }

  # handle incompatible network type args
  if (bipartite && directed) {
    warning("If `bipartite` is `TRUE`, edges are interpreted as undirected.")
    directed <- FALSE
  }
  if (bipartite && loops) {
    warning("If `bipartite` is `TRUE`, `loops` must be `FALSE`.")
    loops <- FALSE
  }
  if (hyper && !directed && !loops) {
    warning("If `hyper` is `TRUE` and `directed` is `FALSE`, `loops` must be `TRUE`.")
    loops <- TRUE
  }
  if (hyper && bipartite) {
    stop("Both `hyper` and `bipartite` are `TRUE`, but bipartite hypergraphs are not supported.")
  }

  # validate edges
  .validate_edge_df(
    edges = x, directed = directed, hyper = hyper, loops = loops,
    multiple = multiple, bipartite = bipartite
  )

  # create variable containing vertex IDs in the order they appear in the edges
  vertex_ids_in_el <- unique(unlist(x[, 1:2], use.names = FALSE))

  # create reference variables to minimize bracket spam
  sources <- x[[1L]]
  targets <- x[[2L]]

  # validate vertices
  if (!is.null(vertices)) {
    .validate_vertex_df(vertices, el_vert_ids = vertex_ids_in_el)
  }

  # if vertices aren't provided, use the order in which they appear in the edges
  if (is.null(vertices)) {
    vertex_names <- vertex_ids_in_el
  } else { # if vertices are provided, use that order
    if (bipartite) {
      # if bipartite, first reorder vertices so actors come before non-actors
      vertices <- .prep_bipartite_vertices(vertices,
                                           el_vert_ids = vertex_ids_in_el,
                                           bipartite_col = bipartite_col)
    }
    vertex_names <- vertices[[1L]]
  }

  # out_sources/out_targets consist of the numerical indices to add to the final network
  out_sources <- lapply(sources, match, vertex_names)
  out_targets <- lapply(targets, match, vertex_names)

  # prep edge attributes
  if (ncol(x) == 2L) {
    edge_attrs <- list(names_eval = NULL, vals_eval = NULL)
  } else {
    edge_attrs <- .prep_edge_attrs(x)
  }

  # start building the network to return
  out <- network.initialize(
    n = length(vertex_names),
    directed = directed,
    hyper = hyper,
    loops = loops,
    multiple = multiple,
    bipartite = if (bipartite) length(unique(sources)) else FALSE
  )

  # add edges (and any edge attributes)
  out <- add.edges.network(
    x = out,
    tail = out_sources,
    head = out_targets,
    names.eval = edge_attrs[["names_eval"]],
    vals.eval = edge_attrs[["vals_eval"]],
    ...
  )

  # set vertex attributes
  if (is.null(vertices)) {
    # if vertices aren't provided, set "vertex.names" as the values used in edges
    out <- set.vertex.attribute(out, attrname = "vertex.names", value = vertex_names)
  } else if (ncol(vertices) == 1L) {
    out <- set.vertex.attribute(out, attrname = "vertex.names", value = vertices[[1L]])
  } else {
    out <- set.vertex.attribute(
      x = out,
      attrname = c(
        "vertex.names", # first column is always "vertex.names"
        names(vertices)[-1L]
      ),
      value = .prep_vertex_attrs(vertices)
    )
  }

  out
}

.is_atomic_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}

.all_are_atomic_scalars <- function(x) {
  all(vapply(x, .is_atomic_scalar, logical(1L), USE.NAMES = FALSE))
}

.is_vectorizable <- function(x) {
  vapply(x, .all_are_atomic_scalars, logical(1L), USE.NAMES = FALSE)
}

.vectorize_safely <- function(x) {
  to_vectorize <- .is_vectorizable(x)

  x[to_vectorize] <- lapply(x[to_vectorize], function(.x) {
    `attributes<-`(unlist(.x, use.names = FALSE), attributes(.x[[1L]]))
  })

  x
}


.as_edge_df <- function(x, attrs_to_ignore, na.rm, ...) {
  if (network.edgecount(x) == 0L) {
    empty_edge_df <- structure(
      list(.tail = logical(), .head = logical(), .na = logical()),
      row.names = integer(),
      class = "data.frame"
    )
    if ("na" %in% attrs_to_ignore) {
      empty_edge_df <- empty_edge_df[, c(".tail", ".head")]
    }
    return(empty_edge_df)
  }

  vertex_names <- network.vertex.names(x)

  el_list <- list(
    .tail = lapply(x[["mel"]], function(.x) vertex_names[.x[["outl"]]]),
    .head = lapply(x[["mel"]], function(.x) vertex_names[.x[["inl"]]])
  )

  # list.edge.attributes() sorts, meaning we can't test round-trips
  edge_attr_names <- unique(
    unlist(lapply(x[["mel"]], function(.x) names(.x[["atl"]])),
      use.names = FALSE
    )
  )
  names(edge_attr_names) <- edge_attr_names

  # extract attributes as-is (lists)
  edge_attrs <- lapply(
    edge_attr_names,
    function(.x) get.edge.attribute(x, .x, unlist = FALSE)
  )
  # if not `TRUE`, "na" is assumed `FALSE` (in the event of `NULL`s or corrupted data)
  edge_attrs[["na"]] <- !vapply(
    edge_attrs[["na"]], isFALSE, logical(1L),
    USE.NAMES = FALSE
  )

  # skip `base::as.data.frame()`'s auto-unlisting behavior
  out <- structure(
    c(el_list, edge_attrs),
    row.names = seq_along(el_list[[1L]]),
    class = "data.frame"
  )

  if (na.rm) {
    # drop NA edge rows
    out <- out[!out[["na"]], ]
    # reset `rownames()` so they're sequential in returned object
    rownames(out) <- NULL
  } else if (!is.hyper(x)) {
    # replace empty ".tail" and ".head" with `NA` so that the columns can be safely
    # vectorized for non-hyper edges when `na.rm` is `FALSE`
    out[1:2] <- lapply(out[1:2], lapply, function(.x) if (length(.x)) .x else NA)
  }

  cols_to_keep <- c(".tail", ".head", setdiff(names(edge_attrs), attrs_to_ignore))
  out <- out[cols_to_keep]

  # if not hyper, `unlist()` ".tail" and ".head"
  if (!is.hyper(x)) {
    out[1:2] <- lapply(out[1:2], unlist, use.names = FALSE)
  }

  # safely vectorize non-edgelist columns
  cols_to_vectorize <- setdiff(names(out), c(".tail", ".head"))
  if (length(cols_to_vectorize)) {
    out[cols_to_vectorize] <- .vectorize_safely(out[cols_to_vectorize])
  }

  out
}


.as_vertex_df <- function(x, attrs_to_ignore, na.rm, ...) {
  if (network.size(x) == 0L) {
    empty_vertex_df <- structure(
      list(vertex.names = logical(), na = logical()),
      class = "data.frame", row.names = integer()
    )
    if ("na" %in% attrs_to_ignore) {
      empty_vertex_df <- empty_vertex_df[, "vertex.names", drop = FALSE]
    }
    return(empty_vertex_df)
  }
  # list.vertex.attributes() sorts the result, meaning we can't test round-trips
  vertex_attr_names <- unique(unlist(lapply(x[["val"]], names), use.names = FALSE))

  vertex_attrs <- lapply(
    `names<-`(vertex_attr_names, vertex_attr_names),
    function(.x) get.vertex.attribute(x, .x, unlist = FALSE)
  )
  vertex_attrs[["na"]] <- lapply(
    vertex_attrs[["na"]],
    function(.x) if (is.null(.x)) TRUE else .x
  )

  out <- structure(
    vertex_attrs,
    row.names = seq_len(network.size(x)),
    class = "data.frame"
  )

  if (!"vertex.names" %in% names(out)) {
    out[["vertex.names"]] <- network.vertex.names(x)
  }

  if (na.rm) {
    out <- out[!vapply(out[["na"]], isTRUE, logical(1L), USE.NAMES = FALSE), ]
    rownames(out) <- NULL
  }

  out_cols <- c(
    "vertex.names",
    setdiff(names(out), c("vertex.names", attrs_to_ignore))
  )

  .vectorize_safely(out[, out_cols, drop = FALSE])
}


#' Coerce a Network Object to a \code{data.frame}
#'
#' The \code{as.data.frame} method coerces its input to a \code{data.frame} containing
#' \code{x}'s edges or vertices.
#'
#' @param x an object of class \code{network}
#' @param ...  additional arguments
#' @param unit whether a \code{data.frame} of edge or vertex attributes
#' should be returned.
#' @param na.rm logical; ignore missing entries when constructing the data frame?
#' @param attrs_to_ignore character; a vector of attribute names to exclude from
#' the returned \code{data.frame} (Default: \code{"na"})
#'
#' @export as.data.frame.network
#' @export
as.data.frame.network <- function(x, ..., unit = c("edges", "vertices"),
                                  na.rm = TRUE,
                                  attrs_to_ignore = "na") {
  if (inherits(x, "network", which = TRUE) != length(class(x))) {
    warning( # nocov start
      '`x` may not correctly inherit from class "network".',
      sprintf("\n\t- `class(x)`: `%s", deparse(class(x)))
    )        # nocov end
  }

  switch(match.arg(unit, c("edges", "vertices")),
    edges = .as_edge_df(
      x,
      attrs_to_ignore = attrs_to_ignore,
      na.rm = na.rm,
      ...
    ),
    vertices = .as_vertex_df(
      x,
      attrs_to_ignore = attrs_to_ignore,
      na.rm = na.rm,
      ...
    ),
    # `match.arg()` used, so this should never be reached...
    stop('`unit` must be one of `"edges"` or `"vertices".') # nocov
  )
}
