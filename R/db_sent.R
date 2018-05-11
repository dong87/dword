#' Sliping sentences from txt object.
#'
#' @param txt A txt object containg characters.
#' @param minL Filter the sentence by length longer than minL.
#' @param maxL Filter the sentence by length shorter than maxL.
#'
#' @return
#' @export
#'
#' @examples
#' db_sci <- list(dest = "PATH/Mendeley_PDF/", myfiles = NULL, sents = NULL) # build a empty list comprising 3 elements. Assign path of the files saved by mendeley
#' db_sci$myfiles <- db_sci$dest %>% all_pdf # save all the pdf file names in this element
#' pdf_2_txt(db_sci$myfiles, pdf2text_path="PATH/pdftotext.exe") # pdf 2 txt
#' db_sci$sents <- db_sci$dest %>% db_txt %>% db_sent # processing the txt transfered from pdf files and spliting into single sentences

db_sent <- function(txt = txt, minL = 50, maxL = 800) {
  # break txt into sents require(quanteda)
  msg(cli::rule(center = crayon::bold("Breaking sents now, this may take a while!")),
      startup = TRUE)
  sent <- lapply(txt, function(i) {
    quanteda::tokens(i, what = "sentence")
  }) %>% lapply(unlist)
  # add author and published year behind sent
  sent <- lapply(names(sent), function(i) paste(sent[[i]], i)) %>% unlist
  sent <- sent[nchar(sent) > minL & nchar(sent) < maxL] %>% unique
  return(sent)
}
