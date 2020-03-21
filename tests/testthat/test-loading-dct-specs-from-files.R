context("loading dct specifications from files and directories")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

test_that("a dct specification from a file with a single dct specification is loaded correctly", {

  res <- load_dct_specs(here::here('tests',
                                   'testthat',
                                   'intention_73dnt604.dct'));

  testthat::expect_equal(res$output$completeness_graph$last_node, 1);

});

###-----------------------------------------------------------------------------

test_that("dct specifications from a file with multiple dct specification are loaded correctly", {

  res <- load_dct_specs(here::here('tests',
                                   'testthat',
                                   'attitude_73dnt5zc.dct'));

  testthat::expect_equal(res$output$completeness_graph$last_node, 1);

});


###-----------------------------------------------------------------------------

test_that("a directory with dct specifications is loaded correctly", {

  res <- load_dct_dir(here::here('tests',
                                 'testthat'));

  testthat::expect_equal(res$output$completeness_graph$last_node, 4);

});

