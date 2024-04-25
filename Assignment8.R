library(tidyverse)
# Question one
file <- list.files("data/sch-test/by-school",
                   full.names =T,
                   pattern= "niagara|bend-gate")
df_bind <- tibble()
for (i in file) {
  file <- read.csv(i)
  file <- mutate(file,relative_path=i)
  df_bind <- bind_rows(df_bind,file)
  
}
# Question two
df_two <- haven::read_dta("data/hsls-small.dta") 
#Qusetion three
df_three <- df_two %>%
  select(stu_id, x4evratndclg, x1paredexpct, x1stuedexpct, x4hs2psmos) %>%
  filter(!is.na(x4evratndclg),!is.na(x1paredexpct),
         !is.na(x1stuedexpct))
Q3function <- function(id){
  student <- df_three %>% filter(id==stu_id,
                                 x1paredexpct != 11,
                                 x1stuedexpct != 11) 
  college_status <- student %>% pull(x4evratndclg)
  parent_expect <- student%>% pull(x1paredexpct)
  student_expect <- student %>% pull(x1stuedexpct)
  moths_between <- student %>% pull(x4hs2psmos)
  if (college_status>0) {
    return(paste("Student",id,"went to college.",
                 "This student had", moths_between, "months between high school and going to college."))
  } else {
    if (parent_expect>2) {
      return(paste("Student",id,"did not go to college,but their parents expected them to."))
    } else {
      return(paste("Student",id,"did not go to college,and their parents did not expect them to."))
    }
  }
}
Q3function(10004)


