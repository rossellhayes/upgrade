bind_rows <- function(tables) {
  all_names <- unique(unlist(lapply(tables, names)))

  tables <- lapply(
    tables,
    function(table) {
      table[setdiff(all_names, names(table))] <- NA
      table
    }
  )

  do.call(rbind, tables)
}
