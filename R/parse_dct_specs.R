#' Parse DCT specifications
#'
#' This function parses DCT specifications into a visual
#' representation as a [DiagrammeR::DiagrammeR] graph and
#' Markdown documents with the instructions for creating
#' measurement instruments or manipulations, and for coding
#' measurement instruments, manipulations, or aspects of
#' a construct.
#'
#' This function is called by [process_dir()]; it is normally not
#' necessary to call this function directly.
#'
#' @param dctSpecs A list of lists of DCT specifications (class
#' `dctRawSpecListSet`), a list of DCT specifications (class
#' `dtcRawSpecList`) or a DCT specification (class `dtcRawSpec`).
#' @param arrowDirection The direction of the arrows in the visual representation
#' of the distributed construct taxonomy; either `forward`, `back`, `both`, or `none`.
#' @param x The parsed `parsed_dct` object.
#' @param ... Any other arguments are passed to the print command.
#'
#' @return An object with the [DiagrammeR::DiagrammeR] graph stored
#' in `output$graph` and the instructions in `output$instr`.
#' @rdname parse_dct_specs
#' @examples extractedSpecs <- extract_dct_specs(text=unlist(strsplit(dct::example_dct_spec, '\n')));
#' parse_dct_specs(extractedSpecs);
#' @export
parse_dct_specs <- function(dctSpecs,
                            arrowDirection = "forward") {

  res <- list(input = as.list(environment()));

  ###--------------------------------------------------------------------------
  ### Start parsing and organising the DCT specifications
  ###--------------------------------------------------------------------------

  ### If we have specification lists from multiple files,
  ### flatten them by removing the file level
  if ("dctRawSpecListSet" %in% class(dctSpecs)) {
    dctSpecs <-
      structure(unlist(dctSpecs,
                       recursive=FALSE),
                class="dtcRawSpecList");
  } else if ("dtcRawSpec" %in% class(dctSpecs)) {
    dctSpecs <-
      structure(list(dctSpecs),
                class="dtcRawSpecList");
  } else if (!("dtcRawSpecList" %in% class(dctSpecs))) {
    stop("Only provide an object of class `dtcRawSpec`,
         `dtcRawSpecList`, or `dctRawSpecListSet`!.");
  }

  ### Extract 'special' variables: identifier and parents
  dctSpecIds <-
    purrr::map_chr(dctSpecs, 'id');
  dctSpecParentList <-
    lapply(purrr::map(dctSpecs, 'parent'),
           unlist);
  names(dctSpecParentList) <-
    dctSpecIds;

  ### Get a list of unique identifiers to build the node dataframe
  dctSpecUniqueIds <-
    unique(dctSpecIds);

  # Combine all specified parents in vectors for each id
  dctSpecParents <- list();
  for (currentId in dctSpecUniqueIds) {
    if (!is.null(dctSpecParentList[[currentId]])) {
      dctSpecParents[[currentId]] <-
        unname(dctSpecParentList[[currentId]]);
    }
  }

  ### Order chronologically
  dctSpecs <-
    dctSpecs[order(unlist(purrr::map(dctSpecs,
                                     'datetime')))];

  ###--------------------------------------------------------------------------
  ### Prepare node and edge dataframes for DiagrammeR graph
  ###--------------------------------------------------------------------------

  ### create pre node df that we will then fill with info from
  ### the DCT specs
  node_df <-
    data.frame(id = seq_along(dctSpecUniqueIds),
               type = rep("construct", length(dctSpecUniqueIds)),
               dct_id = dctSpecUniqueIds);

  ### Note that the node_df_id's are also the row numbers, so we
  ### can create a named vector to conveniently access these row numbers
  id2row <- structure(seq_along(dctSpecUniqueIds),
                      names=dctSpecUniqueIds);

  for (dctSpec in dctSpecs) {
    for (element in (setdiff(names(dctSpec), c('id',
                                               'name',
                                               'parent')))) {
      if (is.null(unlist(dctSpec[[element]]))) {
        ### Nothing provided
      } else if (length(unlist(dctSpec[[element]])) == 1) {
        if (length(dctSpec[[element]]) > 1) {
          node_df[id2row[dctSpec$id], element] <-
            dctSpec[[element]][[1]] %||% "";
      } else if (length(dctSpec[[element]]) == 0) {
        node_df[id2row[dctSpec$id], element] <-
          "";
      } else {
        node_df[id2row[dctSpec$id], element] <-
          dctSpec[[element]] %||% "";
      }

      } else {
        flattenedList <-
          unlist(dctSpec[[element]]);
        names(flattenedList) <-
          paste0(element,
                 "_",
                 names(flattenedList));
        names(flattenedList) <-
          gsub("\\.",
               "_",
               names(flattenedList));
        for (currentName in names(flattenedList)) {
          node_df[id2row[dctSpec$id], currentName] <-
            flattenedList[currentName];
        }
      }
    }
  }

  ### Ensure column order is correct
  node_df <- node_df[, c('id',
                         'type',
                         'label',
                         setdiff(names(node_df),
                                 c('id',
                                   'type',
                                   'label')))];

  ### Create edge dataframe with arrows to parent(s)
  edge_df <-
    DiagrammeR::create_edge_df(from = id2row[rep(names(dctSpecParents),
                                                 purrr::map_dbl(dctSpecParents, length))],
                               to = id2row[unname(unlist(dctSpecParents))],
                               rel = "changes");

  edge_df <- edge_df[stats::complete.cases(edge_df), ];

  ###--------------------------------------------------------------------------
  ### Generate basic DiagrammeR graph
  ###--------------------------------------------------------------------------

  ### Combine node and edge dataframes into a graph
  dctGraph <- DiagrammeR::create_graph(nodes_df = node_df,
                                       edges_df = edge_df);

  ### Set attributes for rendering
  dctGraph <-
    dct::apply_graph_theme(dctGraph,
                           c("layout", "dot", "graph"),
                           c("rankdir", "LR", "graph"),
                           c("outputorder", "nodesfirst", "graph"),
                           c("fixedsize", "false", "node"),
                           c("shape", "box", "node"),
                           c("style", "rounded,filled", "node"),
                           c("color", "#000000", "node"),
                           c("color", "#888888", "edge"),
                           c("dir", arrowDirection, "edge"),
                           c("fillcolor", "#FFFFFF", "node"));

  ###--------------------------------------------------------------------------
  ### Generate completeness DiagrammeR graph
  ###--------------------------------------------------------------------------

  node_df$completeness <-
    paste0(node_df$label, "\n",
           "Def: ", node_df$def, "\n",
           "Measure (dev): ", node_df$measure_dev, "\n",
           "Change (dev): ", node_df$manipulate_dev, "\n",
           "Measure (code): ", node_df$measure_code, "\n",
           "Change (code): ", node_df$manipulate_code, "\n",
           "Aspect (code): ", node_df$aspect_code);

  completeness_node_df <-
    node_df;

  completeness_node_df$label <-
    completeness_node_df$completeness;

  ### Combine node and edge dataframes into a graph
  completeness_dctGraph <- DiagrammeR::create_graph(nodes_df = completeness_node_df,
                                       edges_df = edge_df);

  ### Set attributes for rendering
  completeness_dctGraph <-
    dct::apply_graph_theme(completeness_dctGraph,
                           c("layout", "dot", "graph"),
                           c("rankdir", "LR", "graph"),
                           c("outputorder", "nodesfirst", "graph"),
                           c("fixedsize", "false", "node"),
                           c("shape", "box", "node"),
                           c("style", "rounded,filled", "node"),
                           c("color", "#000000", "node"),
                           c("color", "#888888", "edge"),
                           c("dir", arrowDirection, "edge"),
                           c("fillcolor", "#FFFFFF", "node"));

  ###--------------------------------------------------------------------------
  ### Overviews with instructions for developing measurement instruments, for
  ### developing manipulations, for coding measurement instruments, for coding
  ### manipulations, and for coding aspects
  ###--------------------------------------------------------------------------

  measure_dev <-
    generate_instruction_overview(node_df,
                                  type="measure_dev",
                                  title = "Instructions for the development of measurement instruments");

  measure_code <-
    generate_instruction_overview(node_df,
                                  type="measure_code",
                                  title = "Instructions for the coding of existing measurment instruments");

  manipulate_dev <-
    generate_instruction_overview(node_df,
                                  type="manipulate_dev",
                                  title = "Instructions for the development of manipulations");

  manipulate_code <-
    generate_instruction_overview(node_df,
                                  type="manipulate_code",
                                  title = "Instructions for the coding of existing manipulations");

  aspect_code <-
    generate_instruction_overview(node_df,
                                  type="aspect_code",
                                  title = "Instructions for the coding of aspects");

  ###--------------------------------------------------------------------------
  ### Return result
  ###--------------------------------------------------------------------------

  ### Add relevant intermediate steps and the final results to the res object
  res$intermediate <- list(dctSpecs = dctSpecs,
                           nodes = node_df,
                           edges = edge_df);
  res$output <- list(basic_graph = dctGraph,
                     completeness_graph = completeness_dctGraph,
                     instr = list(measure_dev = measure_dev,
                                  measure_code = measure_code,
                                  manipulate_dev = manipulate_dev,
                                  manipulate_code = manipulate_code,
                                  aspect_code = aspect_code));

  ### Set class and return
  return(structure(res,
                   class="parsed_dct"));

}

#' @rdname parse_dct_specs
#' @method print parsed_dct
#' @export
print.parsed_dct <- function(x, ...) {
  cat("Processed", length(x$intermediate$dctSpecs),
      "specifications, containing", nrow(x$intermediate$nodes),
      "distinct constructs. Graph and instructions for",
      "developing measurement instruments and manipulations and for",
      "coding measurement instruments, manipulations, and aspects",
      "are now available in the returned object, if you stored it.");
  invisible(x);
}


#' @rdname parse_dct_specs
#' @method plot parsed_dct
#' @export
plot.parsed_dct <- function(x, ...) {
  DiagrammeR::render_graph(x$output$basic_graph);
}
