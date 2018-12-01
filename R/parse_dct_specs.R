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
#' @param x The parsed `parsed_dct` object.
#' @param ... Any other arguments are passed to the print command.
#'
#' @return An object with the [DiagrammeR::DiagrammeR] graph stored
#' in `output$graph` and the instructions in `output$instr`.
#' @rdname parse_dct_specs
#' @examples extractedSpecs <- extract_dct_specs(text=unlist(strsplit(dct::example_dct_spec, '\n')));
#' parse_dct_specs(extractedSpecs);
#' @export
parse_dct_specs <- function(dctSpecs) {

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
  ### Generate DiagrammeR graph
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
        node_df[id2row[dctSpec$id], element] <-
          dctSpec[[element]];
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

  ### Combine node and edge dataframes into a graph
  dctTree <- DiagrammeR::create_graph(nodes_df = node_df,
                                      edges_df = edge_df);

  ### Set attributes for rendering
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "layout", "dot", "graph");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "rankdir", "LR", "graph");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "outputorder", "nodesfirst", "graph");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "fixedsize", "false", "node");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "shape", "box", "node");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "style", "rounded,filled", "node");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "color", "#000000", "node");
  dctTree <-
    DiagrammeR::add_global_graph_attrs(dctTree, "fillcolor", "#FFFFFF", "node");

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
  res$output <- list(graph = dctTree,
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
print.parsed_dct <- function(x, ...) {
  cat("Processed", length(x$intermediate$dctSpecs),
      "specifications, containing", nrow(x$intermediate$nodes),
      "distinct constructs. Graph and instructions for",
      "developing measurement instruments and manipulations and for",
      "coding measurement instruments, manipulations, and aspects",
      "are now available in the returned object, if you stored it.");
  invisible(x);
}
