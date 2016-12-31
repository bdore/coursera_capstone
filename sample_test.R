setwd("~/Desktop/Capstone")
#start.time <- Sys.time()
library(tm)
library(slam)
library(dplyr)
library(quanteda)
library(data.table)
library(stringr)

#total lines in each file
files <- c("en_US.news.txt", "en_US.twitter.txt", "en_US.blogs.txt")
#files <- c("en_US.news.txt")
totalLines <- c()
for (file in files){
  con <- file(file, "r")
  numLines <- length(readLines(con, skipNul = T, warn = F))
  totalLines <- append(totalLines, numLines)
  close(con)
}
x <- data.frame(files, totalLines)

#list of filenames
fileNames <- c()
for (i in 1:length(files)){
  fileName <- gsub("^en_US.", "", files[i])
  fileName <- gsub(".txt", "", fileName)
  fileNames[i] <- fileName
}

#list of dataframes to hold word frequencis for each file
dataFrameList <- list()
for (i in 1:length(fileNames)){
  dataFrameList[[i]] <- assign(paste("wordFreq",fileName,sep=""), 
                               data.frame(file=fileNames[i], word="",freq=0))
}

numReadLines <- list(list(),list(),list())
ngramlist <- list(data.frame(uni_tokens = NA, Freq = NA), 
                  data.frame(bi_tokens = NA, Freq = NA), 
                  data.frame(tri_tokens = NA, Freq = NA),
                  data.frame(quadri_tokens = NA, Freq = NA))

load("ngramlist_003_lines_sampled.RData")
lines_sampled_train <- lines_sampled

lines_sampled_test <- list(list(),list(),list())

for (j in 1:length(files)){
  #print(files[j])
  numLines <- x[(x$files == files[j]), "totalLines"]
  #You have to set seed every time you want to get a reproducible random result
  
  #takes x% random numbers of lines from total number of lines
  #line_to_read <- sample.int(numLines, numLines*0.0001)
  line_to_read <- sample.int(numLines, 1)
  print(line_to_read)
  print(length(numReadLines[[j]]))
  
  while(length(numReadLines[[j]]) < round(numLines*0.0001)){
   
     while(is.element(line_to_read, lines_sampled_train[[j]])){
      line_to_read <- sample.int(numLines, 1)
      # print(line_to_read)
      # print("line number conflict")
    }
    
    # print("line number ok")
    numReadLines[[j]] <- append(line_to_read, numReadLines[[j]])
    line_to_read <- sample.int(numLines, 1)
    # lines_sampled_test <- append(lines_sampled_test, numReadLines[[j]]) 
  }
  lines_sampled_test[[j]] <- numReadLines[[j]]
  
  print(paste("Total lines:",length(numReadLines[[j]])))
  print(files[j])
  
  text <- data.frame(linenum = numReadLines[[j]], text = NA)
  for(i in 1:length(numReadLines[[j]])){
    if(i %% 300 == 0){
      print(i)  
    }
    #reads each line
    # print(i)
    # print(numReadLines[[j]][i])
    # line <- numReadLines[[j]][i]
    # print(paste("Line:", line))
    line <- as.numeric(numReadLines[[j]][i])
    # print(line)
    text[i,2] <- as.character(fread(files[j], skip = line-1, 
                                    nrows = 1, sep="\n", header=F, encoding = "UTF-8"))
    # print(text[i,2])
  }
  text <- text[,2]
  text <- iconv(text, "", "ASCII", "byte")
  #creates corpus for the file
  source <- VCorpus(VectorSource(text), readerControl = list(language = "en"))
  
  #transforming 
  source <- tm_map(source, content_transformer(tolower))
  source <- tm_map(source, removeWords, stopwords("english"))
  source <- tm_map(source, removeNumbers) 
  source <- tm_map(source, removePunctuation)
  source <- tm_map(source, stripWhitespace)
  
  tdm <- TermDocumentMatrix(source) #creates term-document matrix
  v <- sort(row_sums(tdm),decreasing=TRUE) #sum counts for all terms and sort 
  dataFrameList[[j]] <- data.frame(word = names(v), freq=v) #creates a dataframe with word frequency
  dataFrameList[[j]]$file <- fileNames[j]
  dataFrameList[[j]] <- arrange(dataFrameList[[j]], desc(freq))
  print(head(dataFrameList[[j]],10))
  
  source <- corpus(source)
  uni_tokens <- quanteda::tokenize(source, what = c("word"), concatenator = " ", ngrams = 1)
  bi_tokens <- quanteda::tokenize(source, what = c("word"), concatenator = " ", ngrams = 2)
  tri_tokens <- quanteda::tokenize(source, what = c("word"), concatenator = " ", ngrams = 3)
  quadri_tokens <- quanteda::tokenize(source, what = c("word"), concatenator = " ", ngrams = 4)
  
  uni_tokens <- unlist(unname(uni_tokens), recursive = FALSE)
  bi_tokens <- unlist(unname(bi_tokens), recursive = FALSE)
  tri_tokens <- unlist(unname(tri_tokens), recursive = FALSE)
  quadri_tokens <- unlist(unname(quadri_tokens), recursive = FALSE)
  
  uni_ngramFreq <- data.frame(table(uni_tokens))
  bi_ngramFreq <- data.frame(table(bi_tokens))
  tri_ngramFreq <- data.frame(table(tri_tokens))
  quadri_ngramFreq <- data.frame(table(quadri_tokens))
  
  # uni_ngramFreq <- uni_ngramFreq[grep("^[a-z]", uni_ngramFreq$uni_tokens),]
  # bi_ngramFreq <- bi_ngramFreq[grep("^[a-z]", bi_ngramFreq$bi_tokens),]
  # tri_ngramFreq <- tri_ngramFreq[grep("^[a-z]", tri_ngramFreq$tri_tokens),]
  # quadri_ngramFreq <- quadri_ngramFreq[grep("^[a-z]", quadri_ngramFreq$quadri_tokens),]
  # 
  # uni_ngramFreq <- uni_ngramFreq[grep("[a-z]$", uni_ngramFreq$uni_tokens),]
  # bi_ngramFreq <- bi_ngramFreq[grep("[a-z]$", bi_ngramFreq$bi_tokens),]
  # tri_ngramFreq <- tri_ngramFreq[grep("[a-z]$", tri_ngramFreq$tri_tokens),]
  # quadri_ngramFreq <- quadri_ngramFreq[grep("[a-z]$", quadri_ngramFreq$quadri_tokens),]
  
  ngramlist[[1]] <-  rbind(ngramlist[[1]], uni_ngramFreq)
  ngramlist[[2]] <-  rbind(ngramlist[[2]], bi_ngramFreq)
  ngramlist[[3]] <-  rbind(ngramlist[[3]], tri_ngramFreq)
  ngramlist[[4]] <-  rbind(ngramlist[[4]], quadri_ngramFreq)
}

# save(ngramlist, file="ngramlist_003_small_sample.RData")

for(i in 1:length(ngramlist)){
  colnames(ngramlist[[i]]) <- c("ngram", "Freq")
  
  # matches single characters at beggining, middle and end of n-grams
  # ngramlist[[i]] <- ngramlist[[i]][grep("(^[a-z]${1}| [a-z]{1} |^[a-z]{1} | [a-z]${1})", ngramlist[[i]]$ngram, invert = T),]
  
  # matches leftover character from encooding
  # ngramlist[[i]] <- ngramlist[[i]][grep("?", ngramlist[[i]]$ngram, invert = T),]
  
  # aggregate duplicated rows
  ngramlist[[i]] <- aggregate(Freq ~ ngram, data = ngramlist[[i]], FUN = sum)
  
  # keeps only n-grams that begin and end with letters
  ngramlist[[i]] <- ngramlist[[i]][grep("^[a-z]$", ngramlist[[1]]$ngram, invert = T),]
  
  # matches single characters at beggining, middle and end of n-grams
  ngramlist[[i]] <- ngramlist[[i]][grep("(^[a-z]${1}| [a-z]{1} |^[a-z]{1} | [a-z]${1})", ngramlist[[i]]$ngram, invert = T),]
  
  # ngramlist[[i]] <- ngramlist[[i]][ngramlist[[i]]$Freq != 1,]
  # if(i >= 2){
  #   ngramlist[[i]]$predictedWord <- sapply(ngramlist[[i]]$ngram, word, -1)
  # }
}