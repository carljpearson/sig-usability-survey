library(tidyverse)
library(here)
library(ggthemes)
library(ggsci)
library(scales)
theme_set(ggthemes::theme_tufte(base_family = "sans"))

df <- read_csv(here("data_cleaned.csv"))

#location
df %>%
  ggplot(aes(location)) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-.5,size=4.5) +
  coord_cartesian(ylim=c(0,60)) +
  #labels
  labs(title="Number of respondents by location"
       ) +
  #theming
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_blank(),
    )+
  scale_x_discrete(labels = wrap_format(20))

#org type
df %>%
  ggplot(aes(org_type)) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-.5,size=4.5) +
  coord_cartesian(ylim=c(0,80)) +
  labs(title="Organization type of respondents"
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_blank())

df %>%
  select(contains("job_function"),id) %>%
  pivot_longer(-id) %>%
  drop_na(value) %>%
  count(value) %>%
  ggplot(aes(x=reorder(value,n),y=n)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n),hjust=-.3,size=3.5) +
  coord_cartesian(xlim=c(0,60)) +
  coord_flip() +
  labs(title="Job functions of respondents"
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text = element_text(size=10),
    axis.title = element_blank())

