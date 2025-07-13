# Moayad Almohanna (s2647405), Sayantan Bal (s2692364), Hector Avila (s2682021)
# We all sat together on the project during non-class hours and worked through all the tasks pushing and pulling the code on Github, so the work was roughly evenly divided among all 3


# setwd("/Users/hecto/OneDrive/Desktop/All/Edinburgh/1_Semester/Extended_statisticall_programming/R/Assigments/Practical_1_Markov_Ulysses/")## comment out of submitted

a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("
#4 Splitting punctuation from words 
split_punct<-function(text, punctuation){
  index_words_p<-grep(paste0("\\", punctuation), text)
  if (length(index_words_p)>0){
    new_text <- rep("",length(text)+length(index_words_p))
    index_punctuation<-index_words_p+ 1:length(index_words_p)
    new_text[index_punctuation] <- punctuation
    new_text[-index_punctuation] <- gsub(paste0("\\", punctuation), '', text)
    
    return(new_text)
  }else{
    return(text)
  }
  
}
#5 call the functions for the specified punctuation
a_1<-split_punct(text = a, punctuation = '.')
a_1<-split_punct(text = a_1, punctuation = ',')
a_1<-split_punct(text = a_1, punctuation = ':')
a_1<-split_punct(text = a_1, punctuation = ';')
a_1<-split_punct(text = a_1, punctuation = '!')
a_1<-split_punct(text = a_1, punctuation = '?')


#6
# Convert all words to lowercase and then find unique words
lower_a <- tolower(a_1)
unique_words <- unique(lower_a)

# Use match function to find indices
index_vector <- match(lower_a, unique_words)

# Count occurrences using tabulate function
word_count <- tabulate(index_vector)
names(word_count)<-unique_words

# Find out threshold for retaining ≈ 1000 words

sorted_counts <- sort(word_count, decreasing = TRUE)
threshold <- sorted_counts[min(1000, length(sorted_counts))]  # Adjust for small sample size

# Get the most common words
common_word_indices <- which(word_count >= threshold)
b <- unique_words[common_word_indices]

#7 Matrices of common word token sequences
index_vector_common <- match(lower_a, b)
mlag <- 4
n <- length(index_vector_common)
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)
for (i in 0:mlag) {
  M[, i + 1] <- index_vector_common[(1 + i):(n - mlag + i)]
}
#10 a finding most often capitalized words
main_text = a_1

# Convert words to lowercase and find capitalized words
lowercase_words = tolower(main_text)
capitalized_words = grepl("^[A-Z]", main_text)
word_counts = data.frame(word = lowercase_words, capitalized = capitalized_words)

capitalized_words_counts = aggregate(capitalized ~ word, data = word_counts, FUN = sum)
total_words_counts = aggregate(capitalized ~ word, data = word_counts, FUN = length)
word_info = merge(capitalized_words_counts, total_words_counts, by = "word")
colnames(word_info) = c("word", "capitalized_words_counts", "total_words_counts")
counts = word_info$capitalized_words_counts / word_info$total_words_counts
threshold = 0.5
often_capitalized_words = word_info[counts >= threshold, "word"]

for (i in 1:length(b)) {
  if (tolower(b[i]) %in% often_capitalized_words) {
    b[i] = paste0(toupper(substring(b[i], 1, 1)), tolower(substring(b[i], 2)))
  }
}

#8

p_order_model<-function(sentence, M, p_order){
  #this function will take as input: 
  # sentence: Is the sequence of tokens (index of that word in the vector b (most common words)) 
  # M: is the matrix with n-mlag rows and mlag+1 columns created in task 7
  # p_order: is the order of the model, if you want to filter the firs 4 columns and sample the number 5
  # then the p_oder is 4
  # oUTPUT:
  # the output will be the token for the next word in the phrase
  
  set_words<-M
  # the matrix is filtered out so the i th column is equal to the i th element of sentence
  # and the NA's are removed
  for( i in 1:p_order){
    filter_na<-(!is.na(set_words[, 1]))
    filter_sentence<-(set_words[, 1]==sentence[i])
    #in every step the first column is dropped having a final matrix dim(M)-i
    set_words<-set_words[filter_na&filter_sentence, 2:(dim(M)[2]-i+1), drop=FALSE]
    #if there are not more or equal than 2 non NA records in the filtered matrix
    #then we break the loop and return -1. If there are no records after filtering
    #the first i<p_order columns then wont be records filtering the p_order columns
    flag<-sum(!is.na(set_words[, 1]))
    if (flag<2){
      return(-1)
      break}
  }
  #sample from the non NA rows the next token
  set_words<-set_words[!is.na(set_words[, 1]), 1]
  return(sample(set_words,1))
}
phrase_function<-function(nw=50, b, index_vector_common, M){
  #Input:
  #      nw: how many words you want in the phrase
  #      b: vector of most common words
  #      index_vector_common: A vector mapping each word in the text to its  
  #                           corresponding index in the unique words vector b.
  #      M: Matrix created in task 7
  #Function:
  #     The function will return a phrase created using an pth-order Markov model
  #Output:
  #     phrase of nw words
  
  
  #sample the first word using the index the word has in b (unique most common words)
  index_1<-which(!is.na(index_vector_common))
  word<-sample(index_vector_common[index_1],1)
  #create the variable that will contain the p+1-words
  word_p_sequence<-c()
  word_p_sequence[1]<-word
  #create the vector to store the final sentence
  phrase<-rep("", nw)
  phrase[1]<-b[word]
  
  #k is going to be used as a counter that will help to allocate the words after a puntuaction 
  k=2
  # we iterate to obtain the nw words
  for (i in 2:nw){
    #we create an auxiliar that will be used if the p order model doesn't work
    word_p_sequence_aux<-word_p_sequence
    for (j in mlag:1) if (i>j){
      #call the p_order_model function to sample the next word from a j order model
      word_sample<-p_order_model(word_p_sequence_aux, M, j)
      #break the loop if we find a word
      if(word_sample!=-1){break}else{
        word_p_sequence_aux<-word_p_sequence_aux[2:length(word_p_sequence_aux)]
      }
    }
    # Concatenate punctuation to previous word without space. 
    # Increment word index (k) only when non-punctuation word is encountered.
    if (b[word_sample] %in% c('.', ',', ':', ';', '!', '?')){
      phrase[[k-1]]<-paste0(phrase[[k-1]], b[word_sample])
    }else{
      phrase[[k]]<-b[word_sample]
      k=k+1
    }
    #new word is added to the word_p_sequence until the length is the same of mlag 
    #when is the same length, the first element is dropped and a new sample world is added 
    #at the end
    if(length(word_p_sequence) < mlag){
      word_p_sequence[i]<-word_sample
    }else{
      word_p_sequence<-word_p_sequence[-1]
      word_p_sequence[mlag]<-word_sample
      
    }
  }
  #the phrase is printed
  cat(phrase)
  return(phrase)
}

#9
#a new phrase is created using the common frequencies
phrase2<-rep("", 50)
index_1<-which(!is.na(index_vector_common))
aux<-index_vector_common[index_1]
word_sample<- sample(aux,1)
phrase2[[1]]<-b[word_sample]
k=2
for (i in 2:50){
  word_sample<- sample(aux,1)
  if (b[word_sample] %in% c('.', ',', ':', ';', '!', '?')){
    phrase2[[k-1]]<-paste0(phrase2[[k-1]], b[word_sample])
  }else{
    phrase2[[k]]<-b[word_sample]
    k=k+1
  }
}
phrase<-phrase_function(b=b, index_vector_common = index_vector_common, M = M)

######Phrase using the common frequencies
cat(phrase2)