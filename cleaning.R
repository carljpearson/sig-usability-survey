#load libraries
library(tidyverse)
library(here)

data_raw <- readxl::read_xlsx("/Users/carlpearson/Documents/Kubernetes/Data/Excel/Kubernetes Research Survey.xlsx")

#job function
colnames(data_raw)[13:35] <- paste0("job_function_",as.character(data_raw[1,c(13:35)]))

#data center
colnames(data_raw)[39:42] <- paste0("data_center_",as.character(data_raw[1,c(39:42)]))

#activities
colnames(data_raw)[44:55] <- paste0("activities_",as.character(data_raw[1,c(44:55)]))

#containers
colnames(data_raw)[56:59] <- paste0("container",as.character(data_raw[1,c(56:59)]))

#kubernetes
colnames(data_raw)[60:64] <- paste0("kubernetes_",as.character(data_raw[1,c(60:64)]))

#learn
colnames(data_raw)[65:72] <- paste0("learn_",as.character(data_raw[1,c(65:72)]))

#other
colnames(data_raw)[43] = "number_of_machines"
colnames(data_raw)[78] = "other_solutions"


df <- data_raw %>%
  rename(
    location="What is your geographic location?",
    company_size="What is the size of your company/organization?",
    role= "Which of these best describes your role?"  ,
    role_other_text = "...37",
    #number_of_machines = "How many machines are in your fleet (including VMs and bare metal)?",
    nps="How likely is it that you would recommend Kubernetes to a friend or colleague?",
    nps_open = "What are the primary reasons for the score you gave Kubernetes?",
    tech_explanation = "How would you explain to a technical colleague or friend what you are trying to achieve by evaluating/using Kubernetes?"    ,
    non_tech_explanation = "How would you explain to a non-technical colleague or friend what you are trying to achieve by evaluating/using Kubernetes?",
    #other_solutions="What other solutions did you try/consider before trying/deciding on Kubernetes? If Kubernetes wasn’t available to you, what would you have done instead?",
    follow_up = "Would it be ok for someone from the open source community to follow up with you about your responses? If so, please provide your name and email. They will be used solely for follow up.",
    email = "...87"
  ) %>%
  select(
    -"IP Address",
    -follow_up,
    -email
  )

df <- df[2:nrow(df),]

write_csv(df,here("data_cleaned.csv"))
