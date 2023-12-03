setwd("D:\\OneDrive - Universiti Sains Malaysia\\USM\\Y3S1\\351 data analytics\\assg1\\Assignment01_Data")

file_paths <- character()  # Initialize an empty character vector to store file paths
words<-c("analytics","insight","of")

#store all file_paths in vector
for (i in 1:10) {
  file_path <- paste0("Q2_Part_", sprintf("%02d", i), ".txt")
  file_paths <- c(file_paths, file_path)
}

countOccurence<- function(word,lines){
  occur<-gregexpr(word,lines)
  print(occur)
}

for(word in words){
  count<-0
  
  #read the files
  for (file_path in file_paths) {
    lines <- readLines(file_path)
    
    for(line in lines){
      
      # Find occurrences of the word in the line
      matches <- gregexpr(word, line)
      
      # Get positions of matches
      # positions <- regmatches(line, matches)
      
      count <- length(matches[[1]][1:length(matches[[1]])])  # Get the count of occurrences
      print(count)
    }
    print("total number of word ",word," is: ",count)
  }
}