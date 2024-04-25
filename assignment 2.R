install.packages("tidyverse")
library(tidyverse)
##question one##
df<-read_csv(file.path("data","hsls-small.csv"))
df%>%select(x1txmtscor)
dfquestion1<-df|>select(stu_id,x1txmtscor,x1sex)
dfquestion1|>count(x1txmtscor)
dfquestion1<-dfquestion1|>
  mutate(x1sex=ifelse(x1sex==-9,NA,x1sex))
dfquestion1<-dfquestion1|>
  mutate(x1txmtscor=ifelse(x1txmtscor==-8,NA,x1txmtscor))
print(dfquestion1,n=27)
dfquestion1_cut<-dfquestion1|>filter(!is.na(x1txmtscor))|>
  filter(!is.na(x1sex))
dfquestion1_cut|>summarize(mean(x1txmtscor))
dfquestion1_cut|>group_by(x1sex)|>
  summarize(mean(x1txmtscor))
##我将性别中的—9归为na，将数学成绩的-8归为na.在剔除缺失数据后，男女的平均成绩一致
##均为51.1##c
##question two##
library(tidyverse)
dfquestion2<-df|>select(stu_id,x1poverty185,x1famincome)
dfquestion2_cut<-dfquestion2|>filter(x1poverty185==1)
dfquestion2_cut<-dfquestion2_cut|>
  mutate(x1famincome=ifelse(x1famincome%in%c(-8,-9),NA,x1famincome))
dfquestion2_cut|>summarize(median(x1famincome))
## 2 means that Family income > $15,000 and <= $35,000##
##question three##
dfquestion3<-df|>select(stu_id,x4hscompstat,x1region)
dfquestion3<-dfquestion3|>
  mutate(x4hscompstat=ifelse(x4hscompstat==-8,NA,x4hscompstat))
dfquestion3<-dfquestion3|>filter(!is.na(x4hscompstat))
total_studnets<-nrow(dfquestion3)
ged_students<-sum(dfquestion3$x4hscompstat==2)
percentag_ged_students<-(ged_students/total_studnets)*100
##第四个问题就是搞定groupby 分类即可
region_difference<-dfquestion3|>
  group_by(x1region)|>
  summarize(ged_percentage_region=sum(x4hscompstat==2)/n()*100)
##question four##
##What percentage of students ever attended a post-secondary institution by February 2016?##
dfquestion4<-df|>
  select(stu_id,x1region,x1famincome,x4evratndclg)|>
  mutate(x4evratndclg=ifelse(x4evratndclg==-8,NA,x4evratndclg))|>
  filter(!is.na(x4evratndclg))
total_stu_number<-nrow(dfquestion4)
post_sec_students<-sum(dfquestion4$x4evratndclg==1)
percentage_postsec_students<-(post_sec_students/total_stu_number)*100
##Give the cross tabulation for both family incomes above/below $35,000 and region##
##family income 应该是>3##
## This means you should have percentages for 8 groups: above/below $35k within each region##
dfquestion4_cut<-dfquestion4|>
  mutate(x1famincome=ifelse(x1famincome%in%c(-8,-9),NA,x1famincome))|>
  filter(!is.na(x1famincome))|>
  mutate(new_incomegroup=ifelse(x1famincome<=2,"Below35k","Above35k"))
region_per_postsec<-dfquestion4_cut|>
  group_by(x1region,new_incomegroup)|>
  summarize(postsec_students_region=sum(x4evratndclg==1)/n()*100)
print(region_per_postsec)
