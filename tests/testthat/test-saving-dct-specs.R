context("saving dct specifications")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("a dct specification can be saved", {

  # exampleDir <-
  #   file.path(system.file(package="psyverse"),
  #             "extdata");

  dctObject <-
    dct_object(prefix="prfx", label = "lbl", definition="def");

  dctYAML <-
    dct_object_to_yaml(dctObject);

  cat(dctYAML);

  tmpFile <- tempfile(fileext = ".dct");

  save_to_yaml(
    dctObject,
    file = tmpFile
  );

  res <- load_dct_specs(file=tmpFile);

  testthat::expect_equal(res$output$completeness_graph$last_node, 1);

});

