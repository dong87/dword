#' Processing txts transfered from pdfs, add authors and years.
#'
#' @param dest The destination
#' @param mytxtfiles If there exists a txt object, you can use it directly.
#'
#' @return
#' @export
#'
#' @examples
db_txt <- function(dest = NULL, mytxtfiles = NULL) {
  if (is.null(dest) & is.null(mytxtfiles))
    stop("You must specify the path of txt files!")
  ifelse(is.null(dest), mytxtfiles <- mytxtfiles, mytxtfiles <- list.files(path = list.dirs(dest),
                                                                           pattern = "txt", full.names = TRUE))
  # if Chinese characters were not recognized
  Sys.setlocale(category = "LC_ALL", locale = "chs")

  # rm the files with latin

  # notice the path length
  length_name <- mytxtfiles[[1]] %>% strsplit("/") %>% unlist %>% length
  a <- mytxtfiles %>% strsplit("/") %>% lapply("[[", length_name) %>% unlist %>%
    strsplit(" - ")
  c1 <- a %>% lapply("[[", 1) %>% unlist
  mytxtfiles <- mytxtfiles[!grepl("\\?", c1)]

  length_name <- mytxtfiles[[1]] %>% strsplit("/") %>% unlist %>% length
  a <- mytxtfiles %>% strsplit("/") %>% lapply("[[", length_name) %>% unlist %>%
    strsplit(" - ")
  c1 <- a %>% lapply("[[", 1) %>% unlist
  c2 <- a %>% lapply("[[", 2) %>% unlist

  # combine the incomplete sents
  txt <- lapply(mytxtfiles, readLines) %>% lapply(function(i) paste0(i, collapse = " ")) %>%
    lapply(function(i) gsub("et al.","et al", i))
  # paste Auther & Year together for the name
  names(txt) <- paste0("(", c1, ", ", c2, ")")
  return(txt)
}
