library(readxl)
data1 <- read_excel("Excel_lesson1.xlsx", sheet = "First Attempt")
data2 <- read_excel("Excel_lesson1.xlsx", sheet = "Courses")
data3<-read_excel("Excel_lesson1.xlsx",sheet = "Demo")
data4<-read_excel("Excel_lesson1.xlsx",sheet = "Codebook")
rm(data)
IPEDdata<-read.csv("effy2022.csv")
##the ipde data that I read is 12-Month Enrollment2022##
