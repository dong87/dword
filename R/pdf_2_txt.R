#' Transefer pdf files to plain txt
#'
#' Give the pdf file path and name to this function and wait a while (depends on the file numbers), the plain txts will be yielded.
#' @param myfiles pdf file path and name
#'
#' @return
#' @export
#'
#' @examples
#' filenames <- "D:/R/Mendeley_PDF/Nature Genetics/filename.pdf"
#' pdf_2_txt(filenames)

pdf_2_txt <- function(myfiles) {
  txt <- lapply(myfiles, function(i) system(paste("./exe/pdftotext.exe", paste0("\"",
                                                                                i, "\"")), wait = FALSE))
  msg(cli::rule(center = crayon::bold("Plz waiting for all PDFs transfered to TXTs!")),
      startup = TRUE)
}
