context("converting TOQs to SOQs")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("A TOQ spec can be converted to a SOQ spec", {

  tmpFile <- tempfile(pattern = "eq60eng_7rs8g3bd_",
                      fileext = ".yml");

  soq <-
    psyverse::toq_to_soq(
      paste0(
        "https://docs.google.com/spreadsheets/d/",
        "1temqfgkUqWypzjsvvLKMkjR707An1Vt-jzK96WBUsPQ"
      ),
      returnYAML = TRUE
    );

  writeLines(
    soq,
    tmpFile
  );

  cat(tmpFile);

  testthat::expect_equal(file.exists(tmpFile), TRUE);

  tmpFile <- tempfile(pattern = "bfi10eng_7sp9mjx3_",
                      fileext = ".yml");

  soq <-
    psyverse::toq_to_soq(
      paste0(
        "https://docs.google.com/spreadsheets/d/",
        "1atGTOs9RGTISDxiSH0IfSH5fDPYaDhnJEdbXqWqDRto"
      ),
      returnYAML = TRUE
    );

  writeLines(
    soq,
    tmpFile
  );

  cat(tmpFile);

  testthat::expect_equal(file.exists(tmpFile), TRUE);

});

