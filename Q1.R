setwd("D:\\OneDrive - Universiti Sains Malaysia\\USM\\Y3S1\\351 data analytics\\assg1\\Assignment01_Data")

file1 <- readLines("Q1_CCS592.txt")
file2 <- readLines("Q1_CDS501.txt")
file3 <- readLines("Q1_CDS506.txt")
file4 <- readLines("Q1_CDS512.txt")
file5 <- readLines("Q1_CDS521.txt")
file6 <- readLines("Q1_CDS523.txt")

# Q1.A
files<- list(
  CCS592=length(file1),
  CDS501=length(file2),
  CDS506=length(file3),
  CDS512=length(file4),
  CDS521=length(file5),
  CDS523=length(file6)
)
max<-0
min<-Inf

max_file<-""
min_file<-""

for(fileName in names(files)){
  # file_name<-paste0("file",i)
  fileLength<- files[[fileName]]

  if(fileLength > max){
    max<-fileLength
    max_file<-fileName
  }

  if(fileLength < min){
    min<-fileLength
    min_file<-fileName
  }
}

print(paste("highest number of students",max_file,", students: ",max))
print(paste("lowest number of students",min_file,", students: ",min))


#Q1.B
allFiles <- c(file1, file2, file3, file4, file5, file6)

# Find unique/distinct lines
distinct_students <- unique(allFiles)

# Display the distinct lines
print(paste(" distinct students can be identified from these six courses are ",length(distinct_students)," students."))


#Q1.C
listCourse <- function(stuName){
  
  fileLines<- list(
    CCS592=file1,
    CDS501=file2,
    CDS506=file3,
    CDS512=file4,
    CDS521=file5,
    CDS523=file6
  )
  
  existence<-character()
  nexistence<-character()
  
  for(fileNameC in names(fileLines)){
    fileLine<- fileLines[[fileNameC]]
    # print(fileLine)
    
    if(any(grepl(stuName,fileLine))){
      # cat("The string '", stuName, "' exists in the file:", fileNameC, "\n")
      existence <-c(existence,fileNameC)
    } else {
      # cat("The string '", stuName, "' does not exist in the file:", fileNameC, "\n")
    }
  }
  
  if(length(existence)==0){
    cat("\n student ",stuName," doen't register for any course.\n")
  }else{
    cat("\n student ",stuName," register for courses:\n")
    cat(existence, sep = ",")
  }
}
studentName<-"NAME003"
listCourse(studentName)

#Q1.D
#union
files_D <- c(file4, file5)
distinctStuD <- unique(files_D)
print("The students who register for CDS512, and CDS521 are:\n")
print(length(distinctStuD))

#intersect
distinct_I<-character()
for(student in files_D){
  if(any(grepl(student,file4)) && any(grepl(student,file5))){ 
    distinct_I<-c(distinct_I,student)
  }
}
print("The students who register for CDS512, and CDS521 are:\n")
print(distinct_I)
print(length(unique(distinct_I)))

#Q1.E
stu_E<-character()
for(student in files_D){
  if(any(grepl(student,file4)) && !any(grepl(student,file5))){ 
    stu_E<-c(stu_E,student)
  }
}
print("The students who register for CDS512, and CDS521 are:\n")
print(stu_E)
print(paste("A total of: ",length(unique(stu_E))))

#Q1.F
stu_F<-character()
for(student in files_D){
  if(!any(grepl(student,file4)) && any(grepl(student,file5))){ 
    stu_F<-c(stu_F,student)
  }
}
print("The students who register for CDS512, and CDS521 are:\n")
print(stu_F)
print(paste("A total of: ",length(unique(stu_F))))

#Q1.G
stu_G<-character()
for(student in allFiles){
  if(!any(grepl(student,file4)) && !any(grepl(student,file5))){ 
    stu_G<-c(stu_G,student)
  }
}
print("The students who register for CDS512, and CDS521 are:")
print(stu_G)
print(paste("A total of: ",length(unique(stu_G))))

#Q1.H
stuThree<-character()
for(student in distinct_students){
  count<-0
  
  for(file in allFiles){
    if(any(grepl(student, file))){
      count<-count+1
    }
  }
  if(count==3){
    stuThree<-c(stuThree,student)
  }
}
cat("the student(s) who register for three courses are:\n",stuThree)