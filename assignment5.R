##assinment 5
##Rui Xu
##Finish date 2024 February 15th
library(tidyverse)
df <- read.csv(file.path("data", "ggplot2-challenge-data.csv"))
df <- df |>
  mutate(CONTROL = factor(CONTROL,
                          levels = c(1, 2, 3),
                          labels = c("public", "no-profit", "for profit")))|>
  rename(`Instruction` = `inst_prop`,
         `Students Service` = `serv_prop`,
         `Research` = `rsch_prop`)

df_long <- df |>
  pivot_longer(cols = c("Instruction", "Research","Students Service"), 
               names_to = "Type of Spending",                 
               values_to = "proportion")|>
  mutate(`Type of Spending` = factor(`Type of Spending`,
                                     levels=c("Instruction", "Students Service", "Research")))
##the mutate and factor function advice is from matt's suggestion, so that I
##can adjust the position between students service and research]]
p <- ggplot(data = df_long) +
  geom_density(mapping = aes(x = proportion,
                             fill=`Type of Spending`),
               alpha = 0.66) +
  scale_fill_manual(values = c("#FA4616","#0021a5","#22884C")) +
  ##Thanks for matt's advice. I got the Color Hex from uf's website]]
  facet_wrap(~ CONTROL,strip.position = "bottom",nrow = 2)+
  theme_light()+
  theme(legend.position = c(0.8, 0.15), legend.justification = c(0.5, 0),
        legend.box.just = "top") +
  guides(fill = guide_legend(override.aes = list(ncol = 3)))+
  labs(title = "Ratio of Spending on Instruction,Student Service,and Research",
       subtitle = "Differences Across public, No-profit,and For profit colleges",
       x = "proportion of Spending",
       y="density" )
p






