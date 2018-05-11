#' Get word's frequency from a txt object
#'
#' @param txt A txt object
#' @param filepath Assign a output path. The default is "word_freq.csv" under root folder.
#' @param stop_word Character vector.
#'
#' @return
#' @export
#'
#' @examples
word_freq <- function(txt, filepath= "word_freq.csv", stop_word=NULL) {
  txt <- txt
  stop_w <- stop_word
  len_txt <- length(txt)
  if (len_txt < 5000) {
    elapsed_time <- system.time({
      txts <- lapply(txt, function(i) {
        quanteda::tokens(i, what = "word", remove_numbers = T,
                         remove_punct = T, remove_symbols = T)
      }) %>% lapply(., as.character) %>% unlist %>% tolower %>% .[nchar(.)>2] %>% .[nchar(.)<25] %>%
        table %>% as.data.frame %>% .[!.[,1]%in%stop_w,]
    })
    write_csv(txts, filepath)
    msg(
      cli::rule(
        center = paste0(crayon::bold("The output file "), crayon::blue(filepath),
                        crayon::bold(" was saved!"))
      ),
      startup = TRUE
    )
    msg(
      cli::rule(
        center = paste0(crayon::bold("Number "), crayon::red(dim(txts)[1]), " --- ",
                        crayon::bold("Time "), crayon::green(round(elapsed_time[3],2), "s"))
      ),
      startup = TRUE
    )
  }
  else  {
    no_cycle <- floor(length(txt)/5000)
    word_exist <- data.frame(.=NA, Freq=NA)
    for (i in 0:no_cycle) {
      elapsed_time <- system.time({
        txts <- lapply(txt[(5000*i+1):(5000*(i+1))], function(i) {
          quanteda::tokens(i, what = "word", remove_numbers = T,
                           remove_punct = T, remove_symbols = T)
        }) %>% lapply(., as.character) %>% unlist %>% tolower %>% .[nchar(.)>2] %>% .[nchar(.)<25] %>%
          table %>% as.data.frame %>% .[!.[,1]%in%stop_w,]
      })
      txts$. <- as.character(txts$.)
      word_exist$. <- as.character(word_exist$.)
      word_exist <- merge(txts, word_exist, by.x = ".", by.y = ".", all = T)
      names(word_exist) <- c(".", "Freq.x", "Freq.y")
      word_exist$Freq.x[is.na(word_exist$Freq.x)] <- 0
      word_exist$Freq.y[is.na(word_exist$Freq.y)] <- 0
      word_exist$Freq.x <- word_exist$Freq.x+word_exist$Freq.y
      word_exist <- word_exist[,-3]
      word_exist <- word_exist[order(word_exist$., decreasing = F),]
      write_csv(word_exist, filepath)
      msg(
        cli::rule(
          center = paste0(crayon::bold("Completed "), crayon::blue(round(i/no_cycle*100,2)), " % --- ",
                          crayon::bold("Number "), crayon::red(dim(word_exist)[1]), " --- ",
                          crayon::bold("Time left "), crayon::green(round(elapsed_time[3]*(no_cycle-i)/60,2), "min"))
        ),
        startup = TRUE
      )
    }
    msg(
      cli::rule(
        center = paste0(crayon::bold("The output file "), crayon::blue(filepath),
                        crayon::bold(" was saved!"))
      ),
      startup = TRUE
    )
  }
}
