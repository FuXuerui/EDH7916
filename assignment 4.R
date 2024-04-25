##Assignment 4 data visualization
##Rui Xu
##Finish date: 2024 February 10th

##question one]]
##How does student socioeconomic status differ between students ]]
##who ever attended college and those who did not]]
library(tidyverse)
df_hs <- haven::read_dta(file.path("data", "hsls-small.dta"))
df_hs|>count(x4evratndclg)
plot_question1<-df_hs|>
  select(x1ses,x4evratndclg)|>
  drop_na()|>
 mutate(x4evratndclg=factor(x4evratndclg)) 
head(plot_question1)
plot_ques_one<-ggplot(plot_question1)+
  geom_histogram(aes(x=x1ses,
                   fill=x4evratndclg),alpha=0.5,
                 color="black")
print(plot_ques_one)

##question two]]
##How do educational expectations (of both students and parents) ]]
##differ by high school completion status]]
df_ques_two<-df_hs|>
  select(stu_id,x1stuedexpct,x1paredexpct,x4hscompstat)|>
  drop_na()|>
  mutate(hscomstat=ifelse(x4hscompstat<3,1,0),
         hscomstat=factor(hscomstat))|>
  select(-x4hscompstat)|>
  filter(x1stuedexpct!=11&x1paredexpct!=11)|>
pivot_longer(cols=c(x1stuedexpct,x1paredexpct),
             names_to = "expet_type",
             values_to = "eduational_expectation")
head(df_ques_two)
reslut_ques_two<-ggplot(data = df_ques_two,
       mapping = aes(x = eduational_expectation, y = hscomstat,fill=expet_type)) +
  facet_wrap(~ expet_type) +
  geom_boxplot()
print(reslut_ques_two)
##Many thanks to Matt's suggsetions]]

##question three]]
##What is the relationship between student socioeconomic status ]]
##and math test score?]]
df_ques_three<-df_hs|>
  select(stu_id,x1ses,x1txmtscor)|>
  drop_na()
  plot_ques_three_relationship<-
    ggplot(data = df_ques_three,mapping = aes(x = x1ses, y = x1txmtscor) ) +
    geom_point(color=alpha("blue",0.3))+
    geom_smooth(method=lm,color="red")
  print(plot_ques_three_relationship)
 
 
    
