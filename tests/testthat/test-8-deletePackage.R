if (interactive()) {library(testthat); Sys.setenv(NOT_CRAN = "true")}
# set_mock_environment()

context("deleteRepo")

# make baseline repo ------------------------------------------------------

{
  repo_root <- file.path(tempdir(), "miniCRAN", Sys.Date())
  if (file.exists(repo_root)) unlink(repo_root, recursive = TRUE)
  dir.create(repo_root, recursive = TRUE, showWarnings = FALSE)

  revolution <- MRAN("2019-04-01")
  if (!is.online(revolution, tryHttp = FALSE)) {
    # Use http:// for older versions of R
    revolution <- sub("^https://", "http://", revolution)
  }
  rvers <- "3.5"

  #types <- c("win.binary", "mac.binary", "source")
  types <- c("win.binary", "source")

  names(types) <- types
  pdb <- list()
  pkgList <- list()
}

# Delete packages from repo ----------------------------------------------------

# LVGP imports randtoolbox, which depends on rngWELL
# LVGP imports lhs, which imports Rcpp
# RcppArmadillo also imports Rcpp
#
#       RcppArmadillo
#       /
#   Rcpp -  lhs  -  LVGP
#                    /
# rngWELL - randoolbox

pkg_lvgp <- "LVGP"
pkg_rcpparma <- "RcppArmadillo"
pkgsAdd <- c(pkg_lvgp, pkg_rcpparma)
pkg_rtbox <- "randtoolbox"
pkg_lhs <- "lhs"
pkg_well <- "rngWELL"
pkg_rcpp <- "Rcpp"

mock_write_packages_delete <- function(dir, type = "source") {
  db <- structure(c(
    "lhs", "LVGP", "randtoolbox", "Rcpp", "RcppArmadillo", "rngWELL",
    "1.0.1", "2.1.5", "1.17.1", "1.0.1", "0.9.300.2.0", "0.10-5",
    "R (>= 3.4.0)", "R (>= 3.4.0), stats (>= 3.2.5), parallel (>= 3.2.5)",
    "rngWELL (>= 0.10-1)", "R (>= 3.0.0)", "R (>= 3.3.0)", "R (>= 3.0.0)",
    NA, NA, NA, NA, NA, NA,
    "GPL-3", "GPL-2", "BSD_3_clause + file LICENSE",
    "GPL (>= 2)", "GPL (>= 2)", "BSD_3_clause + file LICENSE",
    "i386, x64", NA, "i386, x64", "i386, x64", "i386, x64", "i386, x64",
    "testthat, DoE.base, knitr, rmarkdown, covr", NA, NA,
    "RUnit, inline, rbenchmark, knitr, rmarkdown, pinp, pkgKitten (>= 0.1.2)",
    "RUnit, Matrix, pkgKitten, reticulate, rmarkdown, knitr, pinp, slam",
    NA, "Rcpp", "lhs(>= 0.14), randtoolbox(>= 1.17)", NA, "methods, utils",
    "Rcpp (>= 0.11.0), stats, utils, methods", NA, "Rcpp", NA, NA, NA, "Rcpp", NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  ),
  .Dim = c(6L, 11L),
  .Dimnames = list(NULL, c("Package", "Version", "Depends", "Enhances",
                           "License", "Archs", "Suggests", "Imports",
                           "LinkingTo", "Priority", "License_is_FOSS")))
  mock_write_packages(dir, type, db)
}

pkg_type <- names(types)[1]
for (pkg_type in names(types)) {

  restoreTestRepo <- function(pkgs) {
    with_mock(
      download_packages = mock_download_packages,
      write_packages = mock_write_packages_delete,
      .env = "miniCRAN",
      {
        addPackage(pkgs, path = repo_root, repos = revolution, type = pkg_type,
                   quiet = TRUE, Rversion = rvers)
      })

    expect_true(
      all(
        pkgs %in% pkgAvail(repo_root,
                           type = pkg_type,
                           Rversion = rvers,
                           quiet = TRUE)[, "Package"]
      )
    )
  }

  context(sprintf(" - Delete packages from repo (%s)", pkg_type))

  test_that(sprintf(
    "deletePackage deletes %s files",
    pkg_type), {

      skip_on_cran()
      skip_if_offline(revolution)

      pdb <<- lapply(types, pkgAvail, repos = revolution, Rversion = rvers, quiet = TRUE)
      pkgListAdd <- pkgDep(pkgsAdd, availPkgs = pdb[[pkg_type]],
                           repos = revolution,
                           type  = pkg_type,
                           suggests = FALSE,
                           Rversion = rvers)

      restoreTestRepo(pkgsAdd)
      # Test #1: delete only listed packages, leave all deps as is
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkgsAdd, path = repo_root, type = pkg_type,
                        Rversion = rvers, deps = FALSE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: RcppArmadillo, LVGP
      # Remain: Rcpp, rngWELL, lhs, randtoolbox
      expect_true(
        all(c(
          !(pkgsAdd %in% pdb),
          pkg_lhs %in% pdb, pkg_rtbox %in% pdb,
          pkg_well %in% pdb, pkg_rcpp %in% pdb)
        )
      )

      restoreTestRepo(pkgsAdd)
      # Test #2: try deleting package that is required by others
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkg_lhs, path = repo_root, type = pkg_type, Rversion = rvers,
                        deps = TRUE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: none
      # Removed as deps: none
      # Remain: RcppArmadillo, LVGP, Rcpp, rngWELL, lhs, randtoolbox
      expect_true(
        all(c(
          pkgsAdd %in% pdb,
          pkg_lhs %in% pdb, pkg_rtbox %in% pdb,
          pkg_well %in% pdb, pkg_rcpp %in% pdb)
        )
      )

      restoreTestRepo(pkgsAdd)
      # Test #3: delete packages and first-tier dependencies
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkgsAdd, path = repo_root, type = pkg_type, Rversion = rvers,
                        deps = TRUE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: RcppArmadillo, LVGP
      # Removed as deps: Rcpp, lhs, randtoolbox
      # Remain: rngWELL
      expect_true(
        all(c(
          !(pkgsAdd %in% pdb),
          !(pkg_lhs %in% pdb), !(pkg_rtbox %in% pdb),
          pkg_well %in% pdb, !(pkg_rcpp %in% pdb))
        )
      )

      restoreTestRepo(pkgsAdd)
      # Test #4: delete packages and first-tier dependencies, but the dependency is required
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkg_rcpparma, path = repo_root, type = pkg_type, Rversion = rvers,
                        deps = TRUE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: RcppArmadillo
      # Removed as deps: none (Rcpp is held as required by others)
      # Remain: LVGP, Rcpp, rngWELL, lhs, randtoolbox
      expect_true(
        all(c(
          !(pkg_rcpparma %in% pdb), pkg_lvgp %in% pdb,
          pkg_rtbox %in% pdb, pkg_lhs %in% pdb,
          pkg_well %in% pdb, pkg_rcpp %in% pdb)
        )
      )

      restoreTestRepo(pkgsAdd)
      # Test #5: delete packages and recursive dependencies
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkgsAdd, path = repo_root, type = pkg_type, Rversion = rvers,
                        deps = TRUE, recursive = TRUE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: RcppArmadillo, LVGP
      # Removed as deps: Rcpp, rngWELL, lhs, randtoolbox
      # Remain: none
      expect_true(
        all(c(
          !(pkgsAdd %in% pdb),
          !(pkg_rtbox %in% pdb), !(pkg_lhs %in% pdb),
          !(pkg_well %in% pdb), !(pkg_rcpp %in% pdb))
        )
      )

      restoreTestRepo(pkgsAdd)
      # Test #6: delete packages and all reverse recursive dependencies
      with_mock(
        download_packages = mock_download_packages,
        write_packages = mock_write_packages,
        .env = "miniCRAN",
        {
          deletePackage(pkg_rcpp, path = repo_root, type = pkg_type, Rversion = rvers,
                        deps = TRUE, recursive = TRUE, reverse = TRUE)
          pdb <- pkgAvail(repo_root,
                          type = pkg_type,
                          Rversion = rvers,
                          quiet = TRUE)[, "Package"]
        })
      # Removed: Rcpp
      # Removed as revdeps (can't work without Rcpp): RcppArmadillo, LVGP, lhs
      # Remain: rngWELL, randtoolbox
      expect_true(
        all(c(
          !(pkgsAdd %in% pdb), !(pkg_rcpparma %in% pdb),
          !(pkg_lvgp %in% pdb), !(pkg_lhs %in% pdb),
          pkg_well %in% pdb, pkg_rtbox %in% pdb)
        )
      )
    })
}
