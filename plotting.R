library(tidyverse)
library(here)
library(ggthemes)
library(ggsci)
library(scales)
theme_set(ggthemes::theme_tufte(base_family = "sans"))

df <- read_csv(here("data_cleaned.csv"))

df <- df %>%
  mutate(
    number_of_machines = factor(number_of_machines, levels=c( #properly order factors
      "1-5",
      "6-20",
      "21-50",
      "51-100",
      "101-200",
      "201-500",
      "501-1000",
      "1001-2000",
      "2001-5000",
      "5000+"
      
    ))
    
  )

#location
df %>%
  ggplot(aes(location,fill=location)) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-.5,size=4.5) +
  coord_cartesian(ylim=c(0,60)) +
  #labels
  #theming
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  )+
  scale_x_discrete(labels = wrap_format(20)) +
  scale_fill_manual(values = rep("royalblue",50))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/location.png",device = "png",width=7,height=4,bg="transparent")



#org type
df %>%
  ggplot(aes(org_type,fill=org_type)) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-.5,size=4.5) +
  coord_cartesian(ylim=c(0,80)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_blank(),
    legend.position = "none")+
  scale_fill_manual(values = rep("royalblue",50)) 

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/org_type.png",device = "png",width=7,height=4,bg="transparent")

#job function
df %>%
  select(contains("job_function"),id) %>%
  pivot_longer(-id) %>%
  drop_na(value) %>%
  count(value) %>%
  ggplot(aes(x=reorder(value,n),y=n,fill=value)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_cartesian(xlim=c(0,60)) +
  coord_flip() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = "none") +
  scale_fill_manual(values = rep("royalblue",50))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/jobfunction.png",device = "png",width=8,height=5,bg="transparent")

#role
df %>%
  count(role) %>%
  drop_na(role) %>%
  ggplot(aes(x=reorder(role,n),y=n,fill="place")) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_cartesian(xlim=c(0,60)) +
  coord_flip() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = "none")+
  scale_fill_manual(values = rep("royalblue",50))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/role.png",device = "png",width=7,height=4,bg="transparent")

#job function + role
df %>%
  select(contains("job_function"),id,role) %>%
  pivot_longer(contains("job_function")) %>%
  drop_na(value,name,role) %>%
  group_by(role) %>%
  count(value) %>%
  ggplot(aes(x=reorder(value,n),y=n,fill=role)) +
  geom_bar(stat='identity') +
  #  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_flip() +
  theme(
    #axis.ticks.x = element_blank(),
    #axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = c(0.8, 0.4)) +
  scale_fill_brewer(palette = "Spectral")


ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/roleandjob.png",device = "png",width=7,height=4,bg="transparent")

#data_center
df %>%
  select(contains("data_center"),id) %>%
  pivot_longer(-id) %>%
  drop_na(value) %>%
  count(value) %>%
  ggplot(aes(x=reorder(value,n),y=n,fill=value)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_cartesian(xlim=c(0,60)) +
  coord_flip() +
  labs(title="Data center types"
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = "none") +
  scale_fill_manual(values = rep("royalblue",50))


ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/datacenter.png",device = "png",width=7,height=4,bg="transparent")

#number of machines
df %>%
  count(number_of_machines) %>%
  drop_na(number_of_machines) %>%
  ggplot(aes(x=number_of_machines,y=n,fill=number_of_machines)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_cartesian(xlim=c(0,60)) +
  coord_flip() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = "none") +
    scale_fill_brewer(palette = "Spectral")

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/numberofmachines.png",device = "png",width=7,height=4,bg="transparent")


#activities

df %>%
  select(id,contains("activities_")) %>%
  pivot_longer(contains("activities_")) %>%
  mutate(name=gsub("activities_","",name)) %>%
  drop_na(value) %>%
  group_by(name) %>%
  mutate(total=n()) %>%
  group_by(name,value,total) %>%
  count() %>%
  mutate(prop=n/total) %>%
  ggplot(aes(x=reorder(name,n),y=prop,fill=value)) +
  geom_bar(stat='identity',position = "stack") +
  coord_flip() +
  scale_fill_futurama() +
  labs(x="",y="Proportion") +
  scale_x_discrete(labels = wrap_format(30))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/activities.png",device = "png",width=7,height=4,bg="transparent")


#container use

df %>%
  select(id,contains("container_")) %>%
  pivot_longer(contains("container_")) %>%
  mutate(name=gsub("container_","",name)) %>%
  drop_na(value) %>%
  group_by(name) %>%
  mutate(total=n()) %>%
  ungroup() %>%
  mutate(
    value=factor(value,levels = c("Used in the past","Currently using","Future plans","N/A")),
    name=factor(name,levels = c("Proof of concept","Development","Test","Production"))
  ) %>%
  group_by(name,value,total) %>%
  count() %>%
  mutate(prop=n/total) %>%
  ggplot(aes(x=name,y=prop,fill=value)) +
  geom_bar(stat='identity',position = "dodge") +
  coord_flip() +
  scale_fill_futurama() +
  labs(x="",y="Proportion") +
  scale_x_discrete(labels = wrap_format(30))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/containeruse.png",device = "png",width=7,height=4,bg="transparent")


#kubernetes use


df %>%
  select(id,contains("kubernetes_")) %>%
  pivot_longer(contains("kubernetes_")) %>%
  mutate(name=gsub("kubernetes_","",name)) %>%
  drop_na(value) %>%
  group_by(name) %>%
  mutate(total=n()) %>%
  group_by(name,value,total) %>%
  count() %>%
  ungroup() %>%
  mutate(prop=n/total,
         name=factor(name,levels = c("Proof of concept","Development","Test","Production"))) %>%
  filter(name!="Other (please specify)") %>% #removed other response
  ggplot(aes(x=name,y=prop,fill=value)) +
  geom_bar(stat='identity',position = "dodge") +
  coord_flip() +
  scale_fill_futurama() +
  labs(x="",y="Proportion") 

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/k8suse.png",device = "png",width=7,height=4,bg="transparent")


#learn

df %>%
  select(id,contains("learn_")) %>%
  pivot_longer(contains("learn_")) %>%
  mutate(name=gsub("learn_","",name)) %>%
  drop_na(value) %>%
  count(name) %>%
  ggplot(aes(x=name,y=n,fill="single_color")) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_flip() +
  scale_fill_futurama() +
  labs(x="",y="Proportion") +
  scale_x_discrete(labels = wrap_format(30))  +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank(),
    legend.position = "none") +
  scale_fill_manual(values = rep("royalblue",50))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/learn.png",device = "png",width=7,height=4,bg="transparent")


#nps

df_nps <- df %>%
  mutate(NPS_Categories = case_when(
    nps < 7 ~ "Detractor",
    nps == 7 | nps == 8 ~ "Passive",
    nps == 9 | nps == 10 ~ "Promoter"
  )) %>%
  drop_na(nps) %>%
  mutate(total=n()) %>%
  group_by(NPS_Categories,total) %>%
  count() %>%
  mutate(prop=n/total) %>%
  select(-total,-n) %>%
  pivot_wider(names_from = NPS_Categories,values_from = prop)

nps_value = scales::percent(df_nps$Promoter-df_nps$Detractor)


df %>%
  mutate(NPS_Categories = case_when(
    nps < 7 ~ "Detractor",
    nps == 7 | nps == 8 ~ "Passive",
    nps == 9 | nps == 10 ~ "Promoter"
  )) %>%
  ggplot(aes(nps,fill=NPS_Categories)) +
  geom_bar() +
  scale_fill_manual(values = c("#EE0000","#FFEA6C","#82C299")) +
  scale_x_continuous(breaks=seq(0,10,1), limits=c(-0.5,10.5)) +
  labs(y="Count",x="") +
  annotate("text", x = 2, y = 30, label = paste0("NPS score is ",nps_value))

ggsave("/Users/carlpearson/Documents/r_github/sig-usability-survey/plots/nps.png",device = "png",width=7,height=4,bg="transparent")
