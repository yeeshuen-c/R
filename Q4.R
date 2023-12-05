setwd("D:\\OneDrive - Universiti Sains Malaysia\\USM\\Y3S1\\351 data analytics\\assg\\Assignment01_Data")

csvFile<-read.csv("tracks_features.csv")

itrRow<- ceiling(nrow(csvFile) / 250000)
print(itrRow)
itrCol<- ceiling(ncol(csvFile) / 3)
print(itrCol)

fileNCol<-0

for(i in 1:itrRow){
  for(j in 1:itrCol){
    
    fromRow<-(i-1)*250000 +1
    toRow<- fromRow +250000-1
    fromCol<-(j-1)*3 +1
    toCol<- fromCol +3-1
    
    writeCsv<- csvFile[fromRow:toRow,fromCol:toCol]
    
    fileNCol<-fileNCol +1 
    fileName<- paste0("spotify_0",fileNCol,".csv")
    
    write.csv(writeCsv,fileName,row.names=FALSE)
  }
  # fileNCol<-fileNCol
}

# fileNames<- paste0("spotify_0",1:40,".csv")
# df_list<-list()
# 
# for(file in fileNames){
#   splittedCsv<- read.csv(file)
#   df_list[[file]] <- splittedCsv
# }
# 
# readCount<-0
# for(i in 1:itrRow){
#   for(j in 1:itrCol){
#     readCount<-readCount +1
#     fileNames<- paste0("spotify_0",readCount,".csv")
#     
#   }
# }

data_list <- list()

# Loop through the file ranges and read the CSV files
for (i in 1:5) {
  file_range <- ((8 * (i - 1)) + 1 ): (8 * i)    #precedence issue
  print(file_range)
  file_names <- paste0("spotify_0", file_range, ".csv")
  
  # Read CSV files and append data frames to the list
  file_data <- lapply(file_names, read.csv)
  combined_data <- do.call(cbind, file_data)     #bind by column for each row
  
  data_list[[i]] <- combined_data 
}

# Combine all the data frames into a single data frame
complete <- do.call(rbind, data_list)            #bind by row for all files in data_list

# complete <- do.call(cbind, df_list)
print(nrow(complete))
print(ncol(complete))

write.csv(complete, "complete.csv", row.names = FALSE)