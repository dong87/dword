.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  dword_attach()

  x <- dword_conflicts()
  msg(dword_conflict_message(x), startup = TRUE)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
