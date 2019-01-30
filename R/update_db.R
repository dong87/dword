#' Updata a database
#'
#' @param db A database
#'
#' @return
#' @export
#'
#' @examples
update_db <- function(db, pdf2text_path) {
  myfiles_new <- list.files(path = list.dirs(db$dest), pattern = "(*.pdf)|(*.PDF)", full.names = TRUE)

  # find new files
  myfiles <- setdiff(myfiles_new, db$myfiles)
  if (length(myfiles) == 0)
    stop("Your db is the newest!")
  # rm the files with latin myfiles <- myfiles[-grepl('\\?', myfiles)] transfer
  # pdf to txt
  pdf_2_txt(myfiles, pdf2text_path)
  # Sys.sleep(100)
  msg(cli::rule(center = crayon::bold("If ERROR: cannot open file..., plz wait a while & try it again!")),
      startup = TRUE)
  # read TXT
  mytxtfiles <- paste0(substr(myfiles, 1, nchar(myfiles) - 4), ".txt")
  txt_new <- db_txt(mytxtfiles = mytxtfiles)
  sents_new <- db_sent(txt_new)
  # db$txts <- c(db$txts,txt_new)
  db$sents <- c(sents_new, db$sents)
  db$myfiles <- myfiles_new
  return(db)
  msg(cli::rule(center = crayon::bold("Your db have been updated!")), startup = TRUE)
}
