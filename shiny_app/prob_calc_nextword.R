load("ngramlist.RData")

uniProb_fn <- function(x, bigram = NULL, notFoundResults = 5){
  nxt_word_df <- ngramlist[[2]][grep(paste("^", x, " ", sep = ""), ngramlist[[2]]$ngram), c("ngram","Freq")]
  nxt_word_list <- c("will", "said", "just", "one", "like") #most common words
  if (identical(nxt_word_df$Freq, integer(0))){
    return(nxt_word_list)
  }else{
    uniProb <- arrange(nxt_word_df, desc(Freq))[1:notFoundResults,]
    uniProb <- uniProb[complete.cases(uniProb$ngram),]
    uniProb <- word(uniProb$ngram, -1)
    return(uniProb)
  }
}

biProb_fn <- function(x, notFoundResults = 5){
  nxt_word_df <- ngramlist[[3]][grep(paste("^", x, " ", sep = ""), ngramlist[[3]]$ngram), c("ngram","Freq")]
  # if no bigrams are found, look for unigrams (backoff)
  if (identical(nxt_word_df$Freq, integer(0))){
    
    x <- word(x, -1)
    
    uniProb_fn(x)
  }else{
    biProb <- arrange(nxt_word_df, desc(Freq))[1:5,]
    biProb <- word(na.exclude(biProb$ngram), -1)
    return(biProb)
  }
}

triProb_fn <- function(x){
  nxt_word_df <- ngramlist[[4]][grep(paste("^", x, " ", sep = ""), ngramlist[[4]]$ngram), c("ngram","Freq")]
  # if no trigrams are found, look for bigrams (backoff)
  if (identical(nxt_word_df$Freq, integer(0))){
    
    x <- word(x, 2, 3)
    
    biProb_fn(x)
  }else{
    triProb <- arrange(nxt_word_df, desc(Freq))[1:5,]
    triProb <- word(na.exclude(triProb$ngram), -1)
    return(triProb)
  }
}

ngramProb <- function(x){
  start.time <- Sys.time()
  x <- as.character(x)
  x <- removeWords(x, stopwords("english"))
  x <- stripWhitespace(x)
  x <- str_trim(x)
  nspaces <- str_count(x, " ")
  if (nspaces == 0){
    
    prob <- uniProb_fn(x)
  } else {
    y <- unlist(strsplit(x,split=" "))
    if (nspaces == 1){
      
      prob <- biProb_fn(x)
    }
    if (nspaces >= 2){
      
      x <- word(x, -3, -1)
      prob <- triProb_fn(x)
    }
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  result <- c(prob, time.taken)
  print(result)
}
