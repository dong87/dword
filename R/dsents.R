dsents0 <- function(namesToExtract, sents = sents, save_find = save_find, output = output,
                    rm_pattern = rm_pattern) {
  options(warn=-1)
  # surporting regular formular index reflecting target sents were found or not
  elapsed_time <- system.time({
    namesFound <- stringr::str_extract_all(sents, regex(namesToExtract, ignore_case=TRUE)) %>%
      map(function(i) if(identical(i, character(0))) NA else i) %>% # replace character(0) by NA
      lapply("[[", 1) %>% unlist
    sentList <- split(sents, list(namesFound)) %>% unlist
    if (is.null(sentList))
      stop(msg(cli::rule(center = crayon::bold("Please check that the spelling is correct!")),
               startup = TRUE))
    namesFound <- stringr::str_extract_all(sentList, regex(namesToExtract, ignore_case=TRUE)) %>%
      map(function(i) if(identical(i, character(0))) NA else i) %>% # replace character(0) by NA
      lapply("[[", 1) %>% unlist
    # replace namesToExtract with quoting''
    sentList2 <- list()
    for (i in 1:length(sentList)) {
      sentList2[[i]] <-  sentList[i] %>% str_replace_all(namesFound[i], paste0("**", namesFound[i], "**"))
    }
    # add '- ' ahead each sent
    sentList <- sentList2 %>% map(function(i) paste0("- ", i)) %>% unlist
    # summary references
    refer_split <- sentList %>% str_split("\\(")
    refer <- NULL
    for (i in 1:length(refer_split)) {
      refer[i] <- refer_split[[i]][length(refer_split[[i]])]
    }
    refer <- refer %>% unique %>% str_replace_all("\\)", "") #%>% as.data.frame
    namesfound <- namesFound %>% str_to_lower %>% unique %>% str_sort

    # add a title for the output file
    if(!dir.exists("output")) dir.create("output")
    sink(paste0("./output/", output, ".txt"))
    cat("---\n",
        "title: ", output, "\n",
        "date: ", paste(Sys.time()), "\n",
        "NO_sents: ", length(sentList), "\n",
        "NO_references: ", length(refer), "\n",
        "---\n", sep = "")
    sink()
    if (save_find == T)
      write.table(namesfound, file = paste0("./output/", output, ".txt"), col.names = F,
                  row.names = F, quote = F, append = T)
    write.table(sentList, file = paste0("./output/", output, ".txt"), col.names = F,
                row.names = F, quote = F, append = T)
  })
  msg(cli::rule(center = paste0(crayon::bold("Number "), crayon::red(length(sentList)),
                                " --- ", crayon::bold("Time "), crayon::green(round(elapsed_time[3], 2),
                                                                              "s"))), startup = TRUE)
  file.show(paste0("./output/", output, ".txt"))

  namesfound <- namesFound %>% str_to_lower %>% gsub(rm_pattern, "", .) %>% table %>%
    as.data.frame %>% `colnames<-`(c("Word", "freq")) %>% arrange(desc(freq))
  wcloud <- wordcloud2::wordcloud2(namesfound)
  htmlwidgets::saveWidget(wcloud, paste0(output, ".html"), selfcontained = F)
  return(namesfound)
}

#' Extract sentences accoding a keyword or a pattern (regular expression)
#'
#' @param words Keywords or pattern
#' @param sents A sentence object.
#' @param save_find Logic. Save the search pattern or not.
#' @param output Assign a save name.
#' @param rm_pattern If you use a regular expresion as condition, sepcial pattern is allowed to be removed.
#'
#' @return
#' @export
#'
#' @examples
#' gen_dir() # creat folders
#' data(demo)
#' #' ### single word with a variable tail
#' word <- "facilit"; nf <- dsents(words = paste0("\\b", word, "\\w*"), sents = db_sci$sents, output = word, rm_pattern = "")
#' ### 后词固定+替换
#'word <- "estimates"; nf <- dsents(words = paste0("\\w*", word), sents = db_sci$sents, output = word, rm_pattern = word)
#' ### 前词固定+替换
#'word <- "highly"; nf <- dsents(words = paste0(word, " \\w*ed"), sents = db_sci$sents, output = word,
#'                               rm_pattern = paste0(word, " "))
dsents <- function(words, sents, save_find = T, output = "output", rm_pattern = "") {
  # 这个函数中用的argument最好要和下面嵌套的函数不一致，或者在嵌套中把所有的=去掉

  patts <- list()
  # sents <- lapply(words, function(i) dsents0(i, sents, save_find, output,
  # rm_pattern))
  if (length(words) > length(output))
    stop(length(words), "needs", length(words), "output name")
  for (i in 1:length(words)) {
    patts[[i]] <- dsents0(words[i], sents, save_find, output[i], rm_pattern[i])
  }
  return(patts)
}
