#' Generate 5 folders name "/input", "/output", "/R", "/doc", "/fig"
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' gen_dir()
gen_dir <- function(...) {
  dir_names <- c("/input", "/output", "/R", "/doc", "/fig")
  for (i in 1:length(dir_names)) {
    dir.create(paste0(getwd(), dir_names[i]))
  }
}

