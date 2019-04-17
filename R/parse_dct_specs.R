parse_dct_specs <- function(dctSpecs,
                            arrowDirection = "forward") {

  res <- list(input = as.list(environment()));

  ###--------------------------------------------------------------------------
  ### Start parsing and organising the DCT specifications
  ###--------------------------------------------------------------------------

  ### yum::load_yaml_fragments always returns a list where each element
  ### corresponds to one YAML fragment. Because we normally pass a 'select'
  ### argument (the `dctContainer` argument to this function), only the objects
  ### in that list with that name will be preserved.
  ###
  ### Therefore, we remove two layers of the list to simply get a list of all
  ### DCT specifications.
  # dctSpecs <-
  #   unlist(unlist(dctSpecs,
  #                 recursive=FALSE),
  #          recursive=FALSE);
  ###
  ### This bit above became redundant because we used yum's simplification

  names(dctSpecs) <-
    purrr::map_chr(dctSpecs,
                   'id');

  ### Extract 'special' variables: identifier and parents
  dctSpecIds <-
    purrr::map_chr(dctSpecs, 'id');







  ### If in the 'rel' list, a 'type' is causal_influences_unspecified,
  ### also add that as a parent.








  dctSpecParentList <-
    lapply(purrr::map(dctSpecs, 'parentId'),
           unlist);
  names(dctSpecParentList) <-
    dctSpecIds;

  ### Get a list of unique identifiers to build the node dataframe
  dctSpecUniqueIds <-
    unique(dctSpecIds);

  ### Combine all specified parents in vectors for each id
  dctSpecParents <- list();
  for (currentId in dctSpecUniqueIds) {
    if (!is.null(dctSpecParentList[[currentId]])) {
      dctSpecParents[[currentId]] <-
        unname(dctSpecParentList[[currentId]]);
    }
  }

  ### Order chronologically
  dctSpecDates <-
    unlist(purrr::map(dctSpecs,
                      'datetime'));
  if (length(dctSpecDates) == length(dctSpecs)) {
    dctSpecs <- dctSpecs[dctSpecDates];
  }

  ###--------------------------------------------------------------------------
  ### Prepare node and edge dataframes for DiagrammeR graph
  ###--------------------------------------------------------------------------

  ### Create pre node df that we will then fill with info from
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

      if (!is.null(names(dctSpec[[element]]))) {
        if (('instruction' %in% names(dctSpec[[element]])) && !is.null(dctSpec[[element]][['instruction']])) {
          node_df[id2row[dctSpec$id],
                  paste0(element,
                         "_instruction")] <-
            dctSpec[[element]]['instruction'];
        }
        ### The fields are named
        if (length(dctSpec[[element]]) == 1) {
          ### Just one field; simply set it with its name
          node_df[id2row[dctSpec$id],
                  paste0(element,
                         "_",
                         names(dctSpec[[element]]))] <-
            dctSpec[[element]][[1]] %||% "";
        } else {
          # ### Multiple names fields; check whether they're all single values
          # if (all(unlist(lapply(dctSpec[[element]], length))==1)) {
          #   print(element);
          #   print(names(dctSpec[[element]]));
          #   print(dctSpec[[element]]);
          #   node_df[id2row[dctSpec$id],
          #           paste0(element,
          #                  "_",
          #                  names(dctSpec[[element]]))] <-
          #     unlist(dctSpec[[element]]);
          # } else {
          #   ### This is the most complicated version; we need to collapse
          #   ### deeper elements
          # }
        }
      } else if (is.null(unlist(dctSpec[[element]]))) {
        node_df[id2row[dctSpec$id], element] <-
          "";
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

  edge_df <-
    edge_df[stats::complete.cases(edge_df), ];

  ###--------------------------------------------------------------------------
  ### Generate basic DiagrammeR graph
  ###--------------------------------------------------------------------------

  ### Combine node and edge dataframes into a graph
  dctGraph <- DiagrammeR::create_graph(nodes_df = node_df,
                                       edges_df = edge_df);

  ### Set attributes for rendering
  dctGraph <-
    apply_graph_theme(dctGraph,
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

  node_df$fullInstructions <-
    paste0(node_df$label, "\n",
           "Definition: ", node_df$def_def, "\n",
           "Measure (dev): ", node_df$measure_dev_instruction, "\n",
           "Change (dev): ", node_df$manipulate_dev_instruction, "\n",
           "Aspect (elicit): ", node_df$manipulate_elicit_instruction, "\n",
           "Measure (code): ", node_df$measure_code_instruction, "\n",
           "Change (code): ", node_df$manipulate_code_instruction, "\n",
           "Aspect (code): ", node_df$aspect_code_instruction);

  node_df$fullInstructions <-
    sanitize_for_DiagrammeR(node_df$fullInstructions);

  node_df$completeness <-
    paste0(node_df$label, "\n",
           "Definition: ", ifelse(is.null(node_df$def_def) | is.na(node_df$def_def),
                                  "-",
                                  "Included"), "\n",
           "Measure (dev): ", ifelse(is.null(node_df$measure_dev_instruction) | is.na(node_df$measure_dev_instruction),
                                     "-",
                                     "Included"), "\n",
           "Change (dev): ", ifelse(is.null(node_df$manipulate_dev_instruction) | is.na(node_df$manipulate_dev_instruction),
                                    "-",
                                    "Included"), "\n",
           "Aspect (dev): ", ifelse(is.null(node_df$aspect_dev_instruction) | is.na(node_df$aspect_dev_instruction),
                                     "-",
                                     "Included"), "\n",
           "Measure (code): ", ifelse(is.null(node_df$measure_code_instruction) | is.na(node_df$measure_code_instruction),
                                      "-",
                                      "Included"), "\n",
           "Change (code): ", ifelse(is.null(node_df$manipulate_code_instruction) | is.na(node_df$manipulate_code_instruction),
                                     "-",
                                     "Included"), "\n",
           "Aspect (code): ", ifelse(is.null(node_df$aspect_code_instruction) | is.na(node_df$aspect_code_instruction),
                                     "-",
                                     "Included"));

  completeness_node_df <-
    node_df;

  completeness_node_df$label <-
    completeness_node_df$completeness;

  ### Combine node and edge dataframes into a graph
  completeness_dctGraph <-
    DiagrammeR::create_graph(nodes_df = completeness_node_df,
                             edges_df = edge_df);

  ### Set attributes for rendering
  completeness_dctGraph <-
    apply_graph_theme(completeness_dctGraph,
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

  aspect_dev <-
    generate_instruction_overview(node_df,
                                  type="aspect_dev",
                                  title = "Instructions for the elicitation of aspects");

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
                                  aspect_dev = aspect_dev,
                                  aspect_code = aspect_code));

  ### Set class and return
  return(structure(res,
                   class="dct_specs"));

}
