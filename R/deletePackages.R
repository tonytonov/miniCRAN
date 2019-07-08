#' Delete packages from a miniCRAN repository.
#'
#' Arguments \code{which}, \code{recursive} and \code{reverse} are passed over to
#' \code{tools::package_dependencies} and allow to specify the deletion behaviour.
#'
#' @inheritParams addPackage
#' @param deps logical indicating whether the package reverse dependencies should
#' be removed (default `TRUE`).
#' @inheritParams tools::package_dependencies
#' @export
#' @family update repo functions
deletePackage <- function(pkgs = NULL, path = NULL,
                          type = "source", Rversion = R.version,
                          writePACKAGES = TRUE, deps = TRUE,
                          which = c("Depends", "Imports", "LinkingTo", "Suggests"),
                          recursive = FALSE, reverse = FALSE) {
  if (is.null(path) || is.null(pkgs)) stop("path and pkgs must both be specified.")

  sapply(type, function(t) {
    repoPath <- file.path(path, repoPrefix(t, Rversion))
    db <- pkgAvail(repos = path, type = t, Rversion = Rversion)

    toRemove <- pkgs
    if (deps) {
      pdb <- tools::package_dependencies(db = db, which = which,
                                         recursive = recursive,
                                         reverse = reverse)
      depends <- unique(unlist(lapply(pkgs, function(pkg) {
        if (!is.null(pdb[[pkg]])) pdb[[pkg]] else character()
      })))
      toRemove <- c(toRemove, depends)
    }

    # Checking if any of the packages in the removal list are required by other packages
    pdb_needed <- tools::package_dependencies(
      db = db,
      which = c("Depends", "Imports", "LinkingTo", "Suggests"),
      recursive = FALSE,
      reverse = FALSE)

    needed <- unique(unlist(pdb_needed[!names(pdb_needed) %in% toRemove]))
    toPurge <- toRemove[!toRemove %in% needed]

    if (length(toPurge)) {
      purgePackage(toPurge, db, t, repoPath)
    }
  })

  if (writePACKAGES) invisible(updateRepoIndex(path = path, type = type, Rversion = Rversion))
}

# Given a vector of package names, reconstruct file names and remove these files
purgePackage <- function(pkgs, db, type, repoPath) {
  w <- which(db[, "Package"] %in% pkgs)

  if (length(w)) {
    toRemove <- paste0(db[w, "Package"], "_", db[w, "Version"], pkgFileExt(type))
    file.remove(file.path(repoPath, toRemove))
  }
}
