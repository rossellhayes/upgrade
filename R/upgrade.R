#' Upgrade all installed packages
#'
#' @param daily If `TRUE`, exit early if `upgrade()` has already been run today
#'
#' @return Invisibly returns a list of updated packages
#' @export

upgrade <- function(daily = FALSE) {
  cache <- NULL

  if (daily) {
    date <- as.character(Sys.Date())
    cache <- fs::path(rappdirs::user_cache_dir("uptodate"), "last_update.txt")

    if (fs::file_exists(cache)) {
      if (daily) {
        cache_date <- readLines(cache)

        if (identical(date, cache_date)) {
          force_upgrade_call <- format(
            rlang::call_modify(
              rlang::call_match(rlang::current_call(), upgrade),
              daily = FALSE
            )
          )

          cli::cli_inform(c(
            "v" = "Packages have already been upgraded today.",
            "*" = "Use {.code {force_upgrade_call}} to upgrade again today."
          ))

          return(invisible(NULL))
        }
      }
    } else {
      fs::dir_create(fs::path_dir(cache))
      fs::file_create(cache)
    }
  }

  result <- pkg_upgrade()

  writeLines(date, cache)

  invisible(result)
}
