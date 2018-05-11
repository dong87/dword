#' Processing txts transfered from pdfs, add authors and years.
#'
#' @param dest The destination
#' @param mytxtfiles If there exists a txt object, you can use it directly.
#'
#' @return
#' @export
#'
#' @examples
#' db_sci <- list(dest = "PATH/Mendeley_PDF/", myfiles = NULL, sents = NULL) # build a empty list comprising 3 elements. Assign path of the files saved by mendeley
#' db_sci$myfiles <- db_sci$dest %>% all_pdf # save all the pdf file names in this element
#' pdf_2_txt(db_sci$myfiles, pdf2text_path="PATH/pdftotext.exe") # pdf 2 txt
#' db_sci$sents <- db_sci$dest %>% db_txt %>% db_sent # processing the txt transfered from pdf files and spliting into single sentences

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
