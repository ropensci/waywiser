# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(waywiser)

sf::sf_extSoftVersion()
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  sf::sf_proj_network(TRUE)
}

test_check("waywiser")
