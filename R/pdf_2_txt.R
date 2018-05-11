#' Transefer pdf files to plain txt
#'
#' Give the pdf file path and name to this function and wait a while (depends on the file numbers), the plain txts will be yielded.
#' @param myfiles pdf file path and name
#' @param pdf2text_path path of pdftotext.exe
#'
#' @return
#' @export
#'
#' @examples
#' filenames <- "D:/R/Mendeley_PDF/Nature Genetics/filename.pdf"
#' pdf2text_path <- "C:/PATH/pdftotext.exe"
#' pdf_2_txt(filenames, pdf2text_path)

pdf_2_txt <- function(myfiles, pdf2text_path) {
  txt <- lapply(myfiles, function(i) system(paste(pdf2text_path, paste0("\"",
                                                                                i, "\"")), wait = FALSE))
  msg(cli::rule(center = crayon::bold("Plz waiting for all PDFs transfered to TXTs!")),
      startup = TRUE)
}
