
library(tm)
library(slam)
library(dplyr)
library(quanteda)
library(data.table)
library(stringr)

#total lines in each file
files <- c("en_US.news.txt", "en_US.twitter.txt", "en_US.blogs.txt")

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

lines_sampled <- c()

for (j in 1:length(files)){
  
  numLines <- x[(x$files == files[j]), "totalLines"]
  
  set.seed(2016)
  #takes x% random numbers of lines from total number of lines 
  numReadLines[[j]] <- sample.int(numLines, round(numLines*0.03))
  lines_sampled <- append(lines_sampled, numReadLines[[j]])
  print(length(numReadLines[[j]]))
  print(files[j])
  text <- data.frame(linenum = numReadLines[[j]], text = NA)
  for(i in 1:length(numReadLines[[j]])){
    if(i %% 300 == 0){
      print(i)  
    }
    #reads each line
    line <- as.numeric(numReadLines[[j]][i])
    text[i,2] <- as.character(fread(files[j], skip = line-1, 
                                    nrows = 1, sep="\n", header=F, encoding = "UTF-8"))
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
  
  ngramlist[[1]] <-  rbind(ngramlist[[1]], uni_ngramFreq)
  ngramlist[[2]] <-  rbind(ngramlist[[2]], bi_ngramFreq)
  ngramlist[[3]] <-  rbind(ngramlist[[3]], tri_ngramFreq)
  ngramlist[[4]] <-  rbind(ngramlist[[4]], quadri_ngramFreq)
}

for(i in 1:length(ngramlist)){
  colnames(ngramlist[[i]]) <- c("ngram", "Freq")
  
  # aggregate duplicated rows
  ngramlist[[i]] <- aggregate(Freq ~ ngram, data = ngramlist[[i]], FUN = sum)
  
  # keeps only n-grams that begin and end with letters
  ngramlist[[i]] <- ngramlist[[i]][grep("^[a-z]", ngramlist[[i]]$ngram),]
  
  ngramlist[[i]] <- ngramlist[[i]][grep("[a-z]$", ngramlist[[i]]$ngram),]
  
  # matches single characters at beggining, middle and end of n-grams
  ngramlist[[i]] <- ngramlist[[i]][grep("(^[a-z]${1}| [a-z]{1} |^[a-z]{1} | [a-z]${1})", ngramlist[[i]]$ngram, invert = T),]
  
}

save(ngramlist, file="ngramlist.RData")
