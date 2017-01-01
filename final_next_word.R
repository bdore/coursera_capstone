load("ngram_data_freq1_keep.RData")

uniProb_fn <- function(x, bigram = NULL, notFoundResults = 5){
  print(paste("uniProb function:", x))
  nxt_word_df <- ngramlist[[2]][grep(paste("^", x, " ", sep = ""), ngramlist[[2]]$ngram), c("ngram","Freq")]
  nxt_word_list <- c("will", "said", "just", "one", "like") #most common words
  if (identical(nxt_word_df$Freq, integer(0))){
    return(nxt_word_list)
  }else{
    uniProb <- arrange(nxt_word_df, desc(Freq))[1:notFoundResults,]
    # not_found <- sum(is.na(uniProb$Freq))
    uniProb <- uniProb[complete.cases(uniProb$ngram),]
    # # print(uniProb)
    # # print(not_found)
    # # print(word(uniProb$ngram, -1))
    # if(not_found < 5 & not_found != 0){
    #   # uniProb_list <- na.exclude(uniProb$ngram)
    #   uniProb <- word(uniProb$ngram, -1)
    #   uniProb <- append(uniProb, nxt_word_list[1:not_found])
    # } else {
    #   
    # }
    uniProb <- word(uniProb$ngram, -1)
    return(uniProb)
  }
}

biProb_fn <- function(x, notFoundResults = 5){
  print(paste("biProb function:", x))
  nxt_word_df <- ngramlist[[3]][grep(paste("^", x, " ", sep = ""), ngramlist[[3]]$ngram), c("ngram","Freq")]
  # if no trigrams are found, look for unigrams (backoff)
  if (identical(nxt_word_df$Freq, integer(0))){
    # print(x)
    x <- word(x, -1)
    # print(x)
    uniProb_fn(x)
  }else{
    biProb <- arrange(nxt_word_df, desc(Freq))[1:5,]
    biProb <- word(na.exclude(biProb$ngram), -1)
    return(biProb)
  }
}

triProb_fn <- function(x){
  print(paste("triProb function:", x))
  nxt_word_df <- ngramlist[[4]][grep(paste("^", x, " ", sep = ""), ngramlist[[4]]$ngram), c("ngram","Freq")]
  # if no trigrams are found, look for bigrams (backoff)
  if (identical(nxt_word_df$Freq, integer(0))){
    # print(x)
    x <- word(x, 2, 3)
    # print(x)
    biProb_fn(x)
  }else{
    triProb <- arrange(nxt_word_df, desc(Freq))[1:5,]
    triProb <- word(na.exclude(triProb$ngram), -1)
    return(triProb)
  }
}

ngramProb <- function(x){
  x <- as.character(x)
  x <- removeWords(x, stopwords("english"))
  x <- stripWhitespace(x)
  x <- str_trim(x)
  nspaces <- str_count(x, " ")
  if (nspaces == 0){
    # print(paste("uniprob :", x))
    prob <- uniProb_fn(x)
  } else {
    y <- unlist(strsplit(x,split=" "))
    if (nspaces == 1){
      # print(paste("biprob :", x))
      prob <- biProb_fn(x)
    }
    if (nspaces >= 2){
      # print(paste("triprob :", x))
      x <- word(x, -3, -1)
      prob <- triProb_fn(x)
    }
  }
  print(prob)
}