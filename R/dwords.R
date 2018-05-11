lemma_word <- function(word, path_TreeTagger) {
  treetag(file = word, treetagger = "manual", format = "obj", TT.tknz = FALSE,
          lang = "en", debug = TRUE, TT.options = list(path = path_TreeTagger, preset = "en"))
}


word_tag <- function(txt, tag_list) {
  N <- filter(txt, tag %in% tag_list) %>% select(lemma) %>% arrange(lemma) %>%
    unique
  return(N)
}

#' Extract words according to Noun, Verb, Adjective and Adverb.
#'
#' @param txt A destination that contains txt files.
#' @param path_TreeTagger Path of TreeTagger, TreeTagger is used to lemma words.
#' @param savename Save name
#'
#'
#' @return
#' @export
#'
#' @examples
#' gen_dir() # creat folders
#' data(demo)
#' txts <- dwords(db_sci$sents, path_TreeTagger="PATH/TreeTagger", "outname")
dwords <- function(txt, path_TreeTagger, savename = "result") {
  tag <- list(N = c("NN", "NNS"), ADJ = c("JJ", "JJR", "JJS"), ADV = c("RB", "RBR",
                                                                       "RBS"), V = c("VV", "VVD", "VVG", "VVN", "VVP", "VVZ"))
  txt <- lapply(txt, function(i) {
    quanteda::tokens(i, what = "word", remove_numbers = T, remove_punct = T,
                     remove_symbols = T)
  }) %>% lapply(., as.character) %>% unlist %>% tolower %>% unique %>% gsub("([0-9]\\w*)|(@.*)|(<.*)",
                                                                            "", .)
  msg(cli::rule(center = crayon::bold("Lemmatization now, plz waiting for a while!")),
      startup = TRUE)
  lem_txt <- lemma_word(txt, path_TreeTagger) %>% as.data.frame
  word <- lapply(tag, function(i) {
    word_tag(lem_txt, i)
  })
  names(word) <- paste0(names(tag), "_", savename)
  out <- lapply(names(word), function(i) write.table(word[i], file = paste0("./output/",
                                                                            i, ".txt"), row.names = FALSE, col.names = FALSE, quote = F, append = T))
}
