context("loading dct specifications from files and directories")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("a dct specification from a file with a single dct specification is loaded correctly", {

  exampleDir <-
    file.path(system.file(package="psyverse"),
              "extdata");

  res <- load_dct_specs(file.path(exampleDir,
                                  'intention_73dnt604.dct'));

  testthat::expect_equal(res$output$completeness_graph$last_node, 1);

});

###-----------------------------------------------------------------------------

testthat::test_that("dct specifications from a file with multiple dct specification are loaded correctly", {

  # exampleDir <-
  #   file.path(system.file(package="psyverse"),
  #             "extdata");
  #
  # res <- load_dct_specs(file.path(exampleDir,
  #                                 'attitude_73dnt5zc.dct'));
  #
  # testthat::expect_equal(res$output$completeness_graph$last_node, 1);

});


###-----------------------------------------------------------------------------

testthat::test_that("a directory with dct specifications is loaded correctly", {

  exampleDir <-
    file.path(system.file(package="psyverse"),
              "extdata");

  res <- load_dct_dir(exampleDir);

  testthat::expect_equal(res$output$completeness_graph$last_node, 5);

});

