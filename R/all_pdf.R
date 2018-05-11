#' Output all the pdfs in a folder
#'
#' @param dest path of folder
#'
#' @return
#' @export
#'
#' @examples
all_pdf <- function(dest = dest) {
  # list dests in a directory
  dest <- list.dirs(dest)

  # make a vector of PDF file names
  myfiles <- list.files(path = dest, pattern = ".pdf", full.names = TRUE)
  myfiles_PDF <- list.files(path = dest, pattern = ".PDF", full.names = TRUE)
  myfiles <- c(myfiles, myfiles_PDF)
  return(myfiles)
}
