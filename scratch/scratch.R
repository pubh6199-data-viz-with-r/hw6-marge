# optional, save your exploration code here
library(tidyverse)
library(dplyr)

clean_fogzone <- Guttmacher_2009_Survey_of_contraceptive_knowledge_Fog_Zone_ %>% 
  select(gender,age4grps,race,riskr,abstonly,protect,curmethrr,pregknowr,totknowr,nummthdsr,wprotect,mprotect) %>% 
  rename(
    Current_most_effective_method = curmethrr,
    Pregnancy_knowledge = pregknowr,
    BC_method_knowledge = totknowr,
    BC_methods_heard_about = nummthdsr,
    Age = age4grps,
    Women_protection_status = wprotect,
    Men_protection_status = mprotect,
    Protection_status = protect
  ) %>% 
  mutate(
    gender = case_when(
      gender == 1 ~ "Female",
      gender == 2 ~ "Male",
      TRUE ~ NA_character_
    ),
    race = case_when(
      race == 1 ~ "Hispanic",
      race == 2 ~ "Non-Hispanic White",
      race == 3 ~ "Non-hispanic Black",
      race == 4 ~ "Asian",
      race == 5 ~ "Other",
      TRUE ~ NA_character_
    ),
    Age = case_when(
      Age == 1 ~ "18-19",
      Age == 2 ~ "20-21",
      Age == 3 ~ "22-24",
      Age == 4 ~ "25-29",
      TRUE ~ NA_character_
    ),
    riskr = case_when(
      riskr == 1 ~ "No",
      riskr == 2 ~ "Yes",
      TRUE ~ NA_character_
    ), 
    abstonly = case_when(
      abstonly == 1 ~ "Abstinence Only",
      abstonly == 2 ~ "Abstinence Focused",
      abstonly == 3 ~ "Contraceptive Focused",
      is.na(abstonly) ~ "No sex ed/ don't know",
      TRUE ~ NA_character_
    ), 
    Protection_status = case_when(
      Protection_status == 1 ~ "Well protected",
      Protection_status == 2 ~ "Unprotected",
      Protection_status == 3 ~ "Uncertain status",
      Protection_status == 7 ~ "No Sex",
      TRUE ~ NA_character_
    ),
    Women_protection_status = case_when(
      Women_protection_status == 1 ~ "perfect pill use",
      Women_protection_status == 2 ~ "perfect condom use",
      Women_protection_status == 3 ~ "perfect depo use",
      Women_protection_status == 4 ~ "long-acting method",
      Women_protection_status == 5 ~ "skipped pills",
      Women_protection_status == 6 ~ "missed condom use",
      Women_protection_status == 7 ~ "depo late",
      Women_protection_status == 8 ~ "other method",
      Women_protection_status == 9 ~ "no method - likely at risk",
      Women_protection_status == 17 ~ "no method - unkown risk",
      Women_protection_status == 19 ~ "no sex ever/last 12 mo",
      TRUE ~ NA_character_
    ),
    Men_protection_status = case_when(
      Men_protection_status == 1 ~ "use horm/larc",
      Men_protection_status == 2 ~ "perfect condom use",
      Men_protection_status == 3 ~ "inconsistent condom use",
      Men_protection_status == 4 ~ "withdrawl",
      Men_protection_status == 5 ~ "other/unknown",
      Men_protection_status == 6 ~ "no method - risk likely",
      Men_protection_status == 7 ~ "no method - unknown risk",
      Men_protection_status == 8 ~ "no sex ever/ last 12 mo",
      TRUE ~ NA_character_
    ), 
    Current_most_effective_method = case_when(
      Current_most_effective_method == 1 ~ "long-acting alone",
      Current_most_effective_method == 2 ~ "long-acting + condom",
      Current_most_effective_method == 3 ~ "hormonal alone",
      Current_most_effective_method == 4 ~ "hormonal + condoms",
      Current_most_effective_method == 5 ~ "condoms",
      Current_most_effective_method == 6 ~ "withdrawl/NFP",
      Current_most_effective_method == 7 ~ "other/unknown",
      Current_most_effective_method == 17 ~ "no method - risk unknown",
      Current_most_effective_method == 18 ~ "no method - likely at risk",
      Current_most_effective_method == 19 ~ "no sex ever/ last 12 mo",
      TRUE ~ NA_character_
    ),
    Pregnancy_knowledge = case_when(
      Pregnancy_knowledge == 1 ~ "0-3 correct",
      Pregnancy_knowledge == 4 ~ "4 correct",
      Pregnancy_knowledge == 5 ~ "5 correct",
      Pregnancy_knowledge == 6 ~ "6 correct",
      TRUE ~ NA_character_
    ),
    BC_method_knowledge = case_when(
      BC_method_knowledge == 1 ~ "0-7 correct",
      BC_method_knowledge == 2 ~ "8-11 correct",
      BC_method_knowledge == 3 ~ "12-15 correct",
      BC_method_knowledge == 4 ~ "16-18 correct",
      BC_method_knowledge == 5 ~ "19+ correct",
      TRUE ~ NA_character_
    ),
    BC_methods_heard_about = case_when(
      BC_methods_heard_about == 1 ~ "0-6 methods",
      BC_methods_heard_about == 2 ~ "7-10 methods",
      BC_methods_heard_about== 3 ~ "11-13 methods",
      BC_methods_heard_about == 4 ~ "14-16 methods",
      TRUE ~ NA_character_
    )
  )
