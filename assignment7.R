# Assignment 7
# Question one
# Finished date 2024,February,28
# Rui Xu
library(tidyverse)
library(lubridate)
df <- read_csv(file.path("data", "hd2007.csv")) |>
  rename_all(tolower)
df_ques1 <- df %>%
  mutate(chfnm_lower = str_to_lower(chfnm)) %>%
  filter(str_detect(chfnm_lower, "^dr\\.\\s")) %>%
  distinct(chfnm_lower)
result_ques1 <- nrow(df_ques1)
print(result_ques1)

# Question two
df_ques2 <- df %>%
  mutate(chfnm_lower = str_to_lower(chfnm)) %>%
  filter(str_detect(chfnm_lower, "ph\\.?d\\.?|doctor of philosophy$")) %>%
  distinct(chfnm_lower)
result_ques2 <- nrow(df_ques2)
print(result_ques2)

# Question three
df2 <- read_csv(file.path("data", "ic2007mission.csv"))
# How many repeat their institutional name in their mission statement?
df_ques3_1 <- df %>%
  left_join(df2, by = "unitid") %>%
  select(unitid, instnm, stabbr, control, mission) %>%
  mutate(instnm_lower = str_to_lower(instnm)) %>%
  filter(str_detect(str_to_lower(mission), instnm_lower)) %>%
  filter(!is.na(mission))
result_ques3_1 <- nrow(df_ques3_1)
print(result_ques3_1)
# How many use the word *civic*?
df_ques3_2 <- df %>%
  left_join(df2, by = "unitid") %>%
  select(unitid, instnm, stabbr, control, mission) %>%
  mutate(instnm_lower = str_to_lower(instnm)) %>%
  mutate(mission_lower = str_to_lower(mission)) %>%
  filter(str_detect(mission_lower, "civic"))
result_ques3_2 <- nrow(df_ques3_2)
print(result_ques3_2)
# Which top 3 states have the most schools with
# mission statements that use the word *future*?
df_ques3_3 <- df %>%
  left_join(df2, by = "unitid") %>%
  select(unitid, instnm, stabbr, control, mission) %>%
  mutate(mission_lower = str_to_lower(mission)) %>%
  filter(str_detect(mission_lower, "future")) %>%
  count(stabbr) %>%
  arrange(desc(n)) %>%
  top_n(3)
print(df_ques3_3)
# Which type of schools (public, private-non-profit, private-for-profit) are
# most likely to use the word *skill* in their mission statement?
df_ques3_4 <- df %>%
  left_join(df2, by = "unitid") %>%
  select(unitid, instnm, stabbr, control, mission) %>%
  mutate(mission_lower = str_to_lower(mission)) %>%
  filter(str_detect(mission_lower, "skill")) %>%
  group_by(control) %>%
  summarise(number_type_school = n()) %>%
  top_n(1)
print(df_ques3_4)
# Question four
df_ques4 <- df %>%
  filter(deathyr %in% c(2007, 2008)) %>%
  filter(closedat != -2) %>%
  select(unitid, instnm, closedat) %>%
  mutate(
    closedat_fix = if_else(
      str_length(closedat) == 4,
      paste("01-01", closedat, sep = "-"),
      str_replace(closedat, "-", "-01-")
    ),
    closedat_fix_dt = mdy(closedat_fix)
  )
# Which has been closed for the longest time? How many months has it been from
# its close date to the beginning of this current month (1 February 2020)?
result_ques4 <- df_ques4 %>%
  mutate(tiem_since_close_now = ymd("2020-02-01") - closedat_fix_dt) %>%
  arrange(desc(tiem_since_close_now))
# How many days were there between the first school to close and the last?
days_difference <- result_ques4 %>%
  slice(82) %>%
  pull(closedat_fix_dt) -
  result_ques4 %>%
  slice(1) %>%
  pull(closedat_fix_dt)
days_difference
