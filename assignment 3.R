##Rui Xu]]
##Assignment 3]]
##Finsished date February 4th,2024]]

##question one]]
##in one chained pipe (I don’t need to see intermediate steps);]]
library(tidyverse)

df <- read.csv(file.path("data", "hsls-small.csv")) |>
 filter(!x1txmtscor == -8, !x1sex == -9)

df_cut_one111 <- df |>
  group_by(x1region) |>
  summarize(average_testscore = mean(x1txmtscor))|>
  ##Compute the average test score by region]]
  left_join(df,df_cut_one111,by="x1region")|>
  ##Join back into the full data frame.]]
  mutate(score_difference=x1txmtscor-average_testscore)|>
##Compute the difference between each student’s test score and that of the region.]]
  group_by(x1region)|>
  summarize(mean_diff_region=mean(score_difference))
##Finally, return the mean of these differences by region.]]


##question two]]
##Compute the average test score by region and family income level and ]]
##join it back to the full data frame.]]
df_cut_two<-df|>
  filter(!x1famincome%in%c(-8,-9))|>
  group_by(x1region,x1famincome)|>
  summarize(aver_score=mean(x1txmtscor))|>
  left_join(df,df_cut_two,by=c("x1region","x1famincome"))
##Many thanks to Matt's suggestions and help. Therefore I can understand the 
##logic of function of "left_join"]]
##Despite the various methods I've tried, when using the variable I add back to the original data,]]
##the joined back data is always rendered on the left side.]]
print(df_cut_two)

##question three]]
##part one Select the following variables from the full data set]]
df_long<-df|>
  select(stu_id,x1stuedexpct,x1paredexpct,x4evratndclg)|>
  ##part two,From this reduced data frame, ]]
  ##reshape the data frame so that it is long in educational expectations]]
  pivot_longer(cols=c(x1stuedexpct,x1paredexpct),
               names_to = "expet_type",
               values_to = "eduational_expectation")

