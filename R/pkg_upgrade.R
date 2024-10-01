#' Upgrade all installed packages
#'
#' @return A [character] vector of upgraded packages
#' @export

pkg_upgrade <- function() {
  if (!pingr::is_online()) {
    cli::cli_abort("Internet connection is unavailable.")
  }

  package_list <- pkgdepends::lib_status()
  package_list <- package_list[!package_list$remotetype %in% c(NA, "local"), ]

  packages <- package_list$remotepkgref
  packages <- packages[!is.na(packages)]

  package_proposal <- pkgdepends::new_pkg_installation_proposal(packages)
  package_proposal$solve()
  package_proposal$stop_for_solution_error()

  package_list <- package_proposal$get_solution()$data
  packages <- package_list[
    package_list$lib_status %in% c("new", "update"),
  ]$ref

  if (length(packages) == 0) {
    cli::cli_alert_success("All packages up to date!")
    return(invisible(packages))
  }

  cli::cli_h1("Upgrading packages")

  tryCatch(
    package_proposal$install(),
    error = function(e) {
      cli::cli_alert_danger("Upgrading all packages at once failed.")

      cli::cli_inform(e$parent$stdout)

      cli::cli_alert_info("Trying sequential installation...")

      lapply(
        sample(packages),
        function(package) {
          cli::cli_h3("Installing {package}...")
          tryCatch(
            pak::pkg_install(package, ask = FALSE),
            error = function(e) {
              cli::cli_inform(e$parent$stdout)
            }
          )
        }
      )
    }
  )

  invisible(packages)
}
