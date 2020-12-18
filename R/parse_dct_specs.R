#' Parse DCT specifications
#'
#' This function parses DCT specifications; it's normally
#' called by [load_dct_dir()] or [load_dct_specs()], so you
#' won't have to use it directly.
#'
#' @param dctSpecs The DCT specifications (a list).
#' @param headingLevel The heading level for Markdown output.
#' @param hyperlink_ucids,urlPrefix Passed on to the
#' [generate_instruction_overview()] and [generate_construct_overview()]
#' functions.
#'
#' @return The object of parsed DCT specifications.
#' @export
parse_dct_specs <- function(dctSpecs,
                            headingLevel = 2,
                            hyperlink_ucids = "Markdown",
                            urlPrefix = "#",
                            sortDecreasing=FALSE) {

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

  if ((length(dctSpecs) == 1) && names(dctSpecs[[1]]) == "dct") {
    dctSpecs <- dctSpecs[[1]];
  }

  ### Extract identifiers
  dctSpecIds <-
    unlist(lapply(dctSpecs, function(x) return(x$id)));

  names(dctSpecs) <-
    dctSpecIds;

  ### If in the 'rel' list, a 'type' is causal_influences_unspecified,
  ### also add that as a parent.

  # dctSpecParentList <-
  #   lapply(purrr::map(dctSpecs, 'parentId'),
  #          unlist);
  # names(dctSpecParentList) <-
  #   dctSpecIds;

  ### Get a list of unique identifiers to build the node dataframe
  dctSpecUniqueIds <-
    unique(dctSpecIds);

  ### Combine all specified parents in vectors for each id
  # dctSpecParents <- list();
  # for (currentId in dctSpecUniqueIds) {
  #   if (!is.null(dctSpecParentList[[currentId]])) {
  #     dctSpecParents[[currentId]] <-
  #       unname(dctSpecParentList[[currentId]]);
  #   }
  # }

  ### Order chronologically
  dctSpecDates <-
    unlist(lapply(dctSpecs, function(x) return(x$datetime)));

  if (length(dctSpecDates) == length(dctSpecs)) {
    dctSpecs <- dctSpecs[dctSpecDates];
  }

  ###--------------------------------------------------------------------------
  ### Do some cleaning up in case shorthands are used
  ###--------------------------------------------------------------------------

  dctSpecs <-
    lapply(dctSpecs,
           function(spec) {
             ### Specifying only one character value is shorthand for
             ### specifying an instruction only.
             for (currentCheck in c('measure_dev',
                                    'measure_code',
                                    # 'manipulate_dev',
                                    # 'manipulate_code',
                                    'aspect_dev',
                                    'aspect_code')) {
               if (!is.null(spec[[currentCheck]]) &&
                   !is.list(spec[[currentCheck]]) &&
                   is.character(spec[[currentCheck]])) {
                 if (length(spec[[currentCheck]]) == 1) {
                   spec[[currentCheck]] <- list(instruction = spec[[currentCheck]]);
                 } else {
                   stop("Only one instruction text can be provided!");
                 }
               }
             }
             if (is.list(spec$definition) &&
                 "definition" %in% tolower(names(spec$definition))) {
               spec$definition <- list(definition = spec$definition$definition);
             } else if (!is.null(spec$definition) &&
                        !is.list(spec$definition) &&
                        is.character(spec$definition)) {
               if (length(spec$definition) == 1) {
                 spec$definition <- list(definition = spec$definition);
               } else {
                 stop("Only one definition can be provided!");
               }
             }
             return(spec);
           });

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
        if (('instruction' %in% names(dctSpec[[element]])) &&
            !is.null(dctSpec[[element]][['instruction']])) {
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
          # ### Multiple named fields; check whether they're all single values
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

  ### In the definition and instructions, replace every
  ### single line break with double line breaks
  for (i in grep('_instruction', names(node_df), value=TRUE)) {
    node_df[, i] <-
      gsub("\\n",
           "\n\n",
           node_df[, i]);
  }

  ### Add a label column if necessary
  if (!('label' %in% names(node_df))) {
    node_df$label <- node_df$id;
  }
  ### Fill any empty labels
  node_df$label <-
    ifelse(is.na(node_df$label) | (nchar(node_df$label) == 0),
           node_df$id,
           node_df$label);

  ### Ensure column order is correct
  node_df <- node_df[, c('id',
                         'type',
                         'label',
                         setdiff(names(node_df),
                                 c('id',
                                   'type',
                                   'label')))];

  ###
  ### Process relationships
  ###

  edge_df_input <- data.frame();
  for (dctSpec in dctSpecs) {
    if (('rel' %in% names(dctSpec)) && (!is.null(dctSpec$rel))) {
      if (is.list(dctSpec)) {
        rel <- dctSpec[['rel']];
        dct_id <- dctSpec[['id']];
      } else {
        rel <- dctSpec['rel'];
        dct_id <- dctSpec['id'];
      }
      ### 'Homogenize' input (sometimes people
      ### specify only one relationship, without
      ### the dash in YAML).

      if (!is.null(names(rel))) {
        rel <- list(rel);
      }

      edge_df_input <-
        rbind(edge_df_input,
              matrix(unlist(lapply(rel,
                               function(x) {
                                 res <-
                                   list(unname(id2row[dct_id]),
                                        unname(id2row[x$id]),
                                        x$type);
                                 return(res);
                               })),
                     ncol=3, byrow=TRUE));
    }
  }

  edge_df_input <-
    as.data.frame(edge_df_input,
                  stringsAsFactors=FALSE);

  if (nrow(edge_df_input) > 0) {
    names(edge_df_input) <-
      c('from',
        'to',
        'rel');
  } else {
    edge_df_input <- data.frame(from = numeric(),
                                to = numeric(),
                                rel = character());
  }

  ###--------------------------------------------------------------------------
  ### Generate basic DiagrammeR graph
  ###--------------------------------------------------------------------------

  if (requireNamespace("DiagrammeR", quietly = TRUE)) {

    if (nrow(edge_df_input) > 0) {

      ### Create DiagrammeR edge dataframe - note that for some
      ### odd reason, in the 'edge_df_input' dataframe, the columns
      ### become factors, so we have to make sure we get the original
      ### values.
      edge_df <-
        DiagrammeR::create_edge_df(from = as.numeric(as.character(edge_df_input$from)),
                                   to = as.numeric(as.character(edge_df_input$to)),
                                   rel = as.character(edge_df_input$rel));

      edge_df <-
        edge_df[stats::complete.cases(edge_df), ];

      ### Combine node and edge dataframes into a graph
      dctGraph <- DiagrammeR::create_graph(nodes_df = node_df,
                                           edges_df = edge_df);

    } else {

      edge_df <- edge_df_input;

      ### Create graph without edges
      dctGraph <- DiagrammeR::create_graph(nodes_df = node_df);

    }

    ### Set attributes for rendering
    dctGraph <-
      apply_graph_theme(dctGraph,
                        c("layout", "dot", "graph"),
                        c("rankdir", "LR", "graph"),
                        c("outputorder", "nodesfirst", "graph"),
                        c("fixedsize", "false", "node"),
                        c("shape", "box", "node"),
                        c("style", "filled", "node"),
                        c("color", "#000000", "node"),
                        c("color", "#000000", "edge"),
                        c("dir", "forward", "edge"),
                        c("fillcolor", "#FFFFFF", "node"));


    if (nrow(edge_df_input) > 0) {

      ### -------------------------------------------------------------------------
      ### Set edge specifications for different relationship types
      ### -------------------------------------------------------------------------

      setEdgeConditionally <- function(gr,
                                       edf,
                                       relVal,
                                       ...) {
        relationships <- as.character(edf$rel);
        if (relVal %in% relationships) {
          suppressMessages(gr <-
                             DiagrammeR::clear_selection(gr));
          suppressMessages(gr <-
                             DiagrammeR::select_edges(gr,
                                                      conditions = rel == relVal));
          for (currentSetting in list(...)) {
            suppressMessages(gr <-
                               do.call(DiagrammeR::set_edge_attrs_ws,
                                       list(gr, currentSetting[1], currentSetting[2])));
              #DiagrammeR::set_edge_attrs_ws(gr, currentSetting[1], currentSetting[2]));
          }
          suppressMessages(gr <- DiagrammeR::clear_selection(gr));
        }
        return(gr);
      }

      ### Causal

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_unspecified",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", ""));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_positive",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", "+"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_negative",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", "-"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_unknown",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", "?"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_product",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", "*"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_sum",
                                       c("style", "solid"),
                                       c("dir", "forward"),
                                       c("label", "Sum"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "causal_influences_correlates",
                                       c("style", "solid"),
                                       c("dir", "both"),
                                       c("label", ""));

      ### Semantic

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "semantic_type_of",
                                       c("style", "dotted"),
                                       c("dir", "forward"),
                                       c("label", "Type of"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "semantic_value_of",
                                       c("style", "dotted"),
                                       c("dir", "forward"),
                                       c("label", "Value of"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "semantic_has_attribute",
                                       c("style", "dotted"),
                                       c("dir", "forward"),
                                       c("label", "Has attribute"));

      ### Structural

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "structural_part_of",
                                       c("style", "dashed"),
                                       c("dir", "forward"),
                                       c("label", "Part of"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "structural_has_start",
                                       c("style", "dashed"),
                                       c("dir", "forward"),
                                       c("label", "has start"));

      dctGraph <- setEdgeConditionally(gr  = dctGraph, edf = edge_df,
                                       relVal = "structural_has_end",
                                       c("style", "dashed"),
                                       c("dir", "forward"),
                                       c("label", "has end"));

    }

  } else {
    edge_df <- edge_df_input;
    dctGraph <- paste0("The DiagrammeR package is not installed, so no ",
                       "DiagrammeR graph is created.");
  }

  if (nrow(node_df) > 0) {

    ###--------------------------------------------------------------------------
    ### Generate completeness DiagrammeR graph
    ###--------------------------------------------------------------------------

    node_df$fullInstructions <-
      paste0(node_df$label, "\n",
             "Definition: ", node_df$definition_definition, "\n",
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
             "Definition: ", ifelse(is.null(node_df$definition_definition) | is.na(node_df$definition_definition),
                                    "-",
                                    "Included"), "\n",
             "Measure (dev): ", ifelse(is.null(node_df$measure_dev_instruction) |
                                         is.na(node_df$measure_dev_instruction) |
                                         (nchar(node_df$measure_dev_instruction) == 0),
                                       "-",
                                       "Included"), "\n",
             "Change (dev): ", ifelse(is.null(node_df$manipulate_dev_instruction) |
                                        is.na(node_df$manipulate_dev_instruction) |
                                        (nchar(node_df$manipulate_dev_instruction) == 0),
                                      "-",
                                      "Included"), "\n",
             "Aspect (dev): ", ifelse(is.null(node_df$aspect_dev_instruction) |
                                        is.na(node_df$aspect_dev_instruction) |
                                        (nchar(node_df$aspect_dev_instruction) == 0),
                                       "-",
                                       "Included"), "\n",
             "Measure (code): ", ifelse(is.null(node_df$measure_code_instruction) |
                                          is.na(node_df$measure_code_instruction) |
                                          (nchar(node_df$measure_code_instruction) == 0),
                                        "-",
                                        "Included"), "\n",
             "Change (code): ", ifelse(is.null(node_df$manipulate_code_instruction) |
                                         is.na(node_df$manipulate_code_instruction) |
                                         (nchar(node_df$manipulate_code_instruction) == 0),
                                       "-",
                                       "Included"), "\n",
             "Aspect (code): ", ifelse(is.null(node_df$aspect_code_instruction) |
                                         is.na(node_df$aspect_code_instruction) |
                                         (nchar(node_df$aspect_code_instruction) == 0),
                                       "-",
                                       "Included"));
  }

  completeness_node_df <-
    node_df;

  completeness_node_df$label <-
    completeness_node_df$completeness;

  if (requireNamespace("DiagrammeR", quietly = TRUE)) {

    if (nrow(completeness_node_df) > 0) {
      if (nrow(edge_df) > 0) {
        ### Combine node and edge dataframes into a graph
        completeness_dctGraph <-
          DiagrammeR::create_graph(nodes_df = completeness_node_df,
                                   edges_df = edge_df);
      } else {
        ### Combine node and edge dataframes into a graph
        completeness_dctGraph <-
          DiagrammeR::create_graph(nodes_df = completeness_node_df);
      }
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
                          c("dir", "none", "edge"),
                          c("fillcolor", "#FFFFFF", "node"));
    } else {
      completeness_dctGraph <-
        paste0("No nodes specified to include in the ",
               "DiagrammeR completeness graph.");
    }

  } else {
    completeness_dctGraph <-
      paste0("The DiagrammeR package is not installed, so no ",
             "DiagrammeR completeness graph is created.");
  }

  ###--------------------------------------------------------------------------
  ### Overview with all definitions
  ###--------------------------------------------------------------------------

  definition_overview <-
    generate_definitions_overview(node_df,
                                  headingLevel=headingLevel,
                                  hyperlink_ucids = hyperlink_ucids,
                                  urlPrefix = urlPrefix,
                                  sortDecreasing = sortDecreasing);

  ###--------------------------------------------------------------------------
  ### Overviews with instructions for developing measurement instruments, for
  ### developing manipulations, for coding measurement instruments, for coding
  ### manipulations, and for coding aspects
  ###--------------------------------------------------------------------------

  measure_dev <-
    generate_instruction_overview(node_df,
                                  type="measure_dev",
                                  headingLevel=headingLevel,
                                  hyperlink_ucids = hyperlink_ucids,
                                  urlPrefix = urlPrefix,
                                  sortDecreasing = sortDecreasing);

  measure_code <-
    generate_instruction_overview(node_df,
                                  type="measure_code",
                                  headingLevel=headingLevel,
                                  hyperlink_ucids = hyperlink_ucids,
                                  urlPrefix = urlPrefix,
                                  sortDecreasing = sortDecreasing);
#
#   manipulate_dev <-
#     generate_instruction_overview(node_df,
#                                   type="manipulate_dev",
#                                   headingLevel=headingLevel,
#                                   hyperlink_ucids = hyperlink_ucids,
#                                   urlPrefix = urlPrefix,
#                                   sortDecreasing = sortDecreasing);
#
#   manipulate_code <-
#     generate_instruction_overview(node_df,
#                                   type="manipulate_code",
#                                   headingLevel=headingLevel,
#                                   hyperlink_ucids = hyperlink_ucids,
#                                   urlPrefix = urlPrefix,
#                                   sortDecreasing = sortDecreasing);

  aspect_dev <-
    generate_instruction_overview(node_df,
                                  type="aspect_dev",
                                  headingLevel=headingLevel,
                                  hyperlink_ucids = hyperlink_ucids,
                                  urlPrefix = urlPrefix,
                                  sortDecreasing = sortDecreasing);

  aspect_code <-
    generate_instruction_overview(node_df,
                                  type="aspect_code",
                                  headingLevel=headingLevel,
                                  hyperlink_ucids = hyperlink_ucids,
                                  urlPrefix = urlPrefix,
                                  sortDecreasing = sortDecreasing);

  ###--------------------------------------------------------------------------
  ### Overviews per construct, basically a neatly formatted version of the DCT
  ### specification in YAML
  ###--------------------------------------------------------------------------

  construct_overviews <-
    lapply(dctSpecs,
           generate_construct_overview,
           headingLevel=headingLevel,
           hyperlink_ucids = hyperlink_ucids,
           urlPrefix = urlPrefix,
           sortDecreasing = sortDecreasing);

  ###--------------------------------------------------------------------------
  ### Return result
  ###--------------------------------------------------------------------------

  ### Add relevant intermediate steps and the final results to the res object
  res$intermediate <- list(dctSpecs = dctSpecs,
                           nodes = node_df,
                           edges = edge_df);
  res$output <- list(basic_graph = dctGraph,
                     completeness_graph = completeness_dctGraph,
                     construct_overviews = construct_overviews,
                     defs = definition_overview,
                     instr = list(measure_dev = measure_dev,
                                  measure_code = measure_code,
                                  # manipulate_dev = manipulate_dev,
                                  # manipulate_code = manipulate_code,
                                  aspect_dev = aspect_dev,
                                  aspect_code = aspect_code));

  ### Set class and return
  return(structure(res,
                   class="dct_specs"));

}
