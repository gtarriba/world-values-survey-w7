####################################################################
# Descriptive analysis of World Values Survey wave 7 (2017-2020)
####################################################################

# Clear workspace and load some packages
rm(list=ls())
library(tidyverse)
library(haven)
library(gdata)
library(viridis)

# Import the data (in R format), call it df
data_original <- readRDS("wvs_wave7.rds")

# Let's get an idea of the dimensions and structure of the dataset
dim(data_original) # almost 70K observations and 535 variables
class(data_original) # It's a dataframe so we can work with it right away

# Data preparation --------------------------------------------------------

# The data was imported as haven_labelled data, but we need the variables to be numeric or categorical to work with them properly

# First we identify all variables that are haven_labelled, and save them as vars_with_labels
vars_with_labels = map(data_original, function(x) attr(x, "class") == "haven_labelled") %>% 
  unlist() %>% 
  names()

print(vars_with_labels)

# Now we feed this vector to mutate_at in order convert all labelled variables to factors (some will then need to be converted into numeric!)
df = data_original %>%
  mutate_at( vars(vars_with_labels), as_factor)

# Some variables are numeric such as year of birth (Q261) and age (Q261). Let's create numeric versions of them
df$ybirth <- as.numeric(df$Q261)
df$age <- as.numeric(df$Q262)

# Let's change the name of the Sex variable
df$sex <- df$Q260

# Define key countries to examine
countries <- c("Mexico", "Colombia", "Germany", "Iran", "Nigeria", "United States", "Germany", "Indonesia", "China", "Turkey", "Pakistan", "Bolivia", "Argentina", "Russia", "New Zealand")

# Part I: Age, education, religion and religiosity --------------------------------------------------

# Questions to answer: 
#   Are more educated people less religious? 
#   Are older people more religious?
#   Does the relationship between education and religiosity depend on the religion?

# First, let's examine the variables:

# Religion
table(df$Q289) # Religion
df$religion <- drop.levels(df$Q289, reorder=FALSE) # Drop empty levels to create new variable
df %>% group_by(religion) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>% arrange(desc(prop))

# Education
table(df$Q275) # Educational attainment
df$educ <- drop.levels(df$Q275, reorder=FALSE)
df %>% group_by(educ) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>% arrange(desc(prop))

# Religiosity
table(df$Q6) # Religiosity
df$religiosity <- drop.levels(df$Q6, reorder=FALSE)
df %>% group_by(religiosity) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>% arrange(desc(prop))

# Age
ggplot(data=df, aes(x=age)) + geom_histogram(bins=80)

# Descriptive statistics
# Are more educated people less religious?

df %>% 
  filter(!is.na(religiosity) & !is.na(educ)) %>% 
  group_by(educ, religiosity) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(fill=religiosity, y=prop, x=educ)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Highest educational degree") +
  ylab("Proportion") +
  labs(fill = "Religion is...") +
  coord_flip()

# Does this relationship hold for all age groups? Let's repeat the analysis, segmenting our sample by age group

df$age_group <- cut(df$age, 
                   breaks=c(-Inf, 30, 50, Inf), 
                   labels=c("Less than 30","31 to 50","51 or more"))

df %>% 
  filter(!is.na(age_group) & !is.na(religiosity) & !is.na(educ)) %>% 
  group_by(age_group, educ, religiosity) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(fill=religiosity, y=prop, x=educ)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Highest educational degree") +
  ylab("Proportion") +
  labs(fill = "Religion is...") +
  coord_flip() +
  facet_wrap(~ age_group) # Yes, the relationship exists for all age groups

# Now we examine the relationship between religiosity and education for all major religions

by_religion <- df %>% 
  filter(!is.na(religion) & !is.na(religiosity) & !is.na(educ)) %>% 
  filter(religion %in% c("Muslim", "Protestant", "Roman Catholic", "Buddhist")) %>% 
  group_by(religion, educ, religiosity) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))
  
  ggplot(data = by_religion, aes(fill=religiosity, y=prop, x=educ)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Highest educational degree") +
  ylab("Proportion") +
  labs(fill = "Religion is...") +
  coord_flip() +
  facet_wrap(~ religion)
  
  # Final analysis: is the relationship between education and religiosity similar for men and women?
  
  df %>% 
    filter(!is.na(sex) & !is.na(religiosity) & !is.na(educ)) %>% 
    group_by(sex, educ, religiosity) %>% 
    summarize(n = n()) %>% 
    mutate(prop = n/sum(n)) %>% 
    ggplot(aes(fill=religiosity, y=prop, x=educ)) + 
    geom_bar(position="fill", stat="identity") + 
    xlab("Highest educational degree") +
    ylab("Proportion") +
    labs(fill = "Religion is...") +
    coord_flip() +
    facet_wrap(~ sex) # Yes, the relationship exists for all age groups

# Regression analysis
summary(lm(as.numeric(religiosity) ~ sex  + religion + as.numeric(educ), data = df))


# Part II: Religious Tolerance  ------------------------------------------------------

df$tolerance <- df$Q170

# To what extent do members of each religion consider that their faith is the only acceptable one?
df %>% 
  filter(!is.na(religion) & !is.na(tolerance)) %>% 
  group_by(religion, tolerance) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(fill=tolerance, y=prop, x=religion)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Religious denomination") +
  ylab("Proportion") +
  labs(fill = "My religion is the only acceptable one") +
  theme(legend.position="top") +
  coord_flip()

# Now let's see by country:

df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(tolerance)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(B_COUNTRY, tolerance) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(fill=tolerance, y=prop, x=B_COUNTRY)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion") +
  labs(fill = "My religion is the only acceptable one") +
  theme(legend.position="top") +
  coord_flip()


# Part III: Science and religion ----------------------------------------------------

df$religion_science <- df$Q169

df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(religion_science)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(B_COUNTRY, religion_science) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(fill=religion_science, y=prop, x=B_COUNTRY)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion") +
  labs(fill = " 'When science and religion collide, religion is always right' ") +
  theme(legend.position="top") +
  scale_fill_viridis(option="magma", discrete = TRUE) +
  coord_flip()

# Part IV: Religiosity -------------------------------------------------------------

df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q164)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(B_COUNTRY, Q164) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q164, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "Religiosity (10=max)") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE)


# Part V: How often pray -----------------------------------------------------

df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q172)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(B_COUNTRY, Q172) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q172, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often pray") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="viridis", discrete = TRUE, direction = -1)

# Part VI: Gender equality --------------------------------------------------------

# When is it justifiable: For a man to beat his wife
# By gender
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q189) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q189) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q189, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often justified: For a man to beat his wife") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = -1) + 
  facet_wrap(~ Q260)

# Part VII: Differences in ethical principles  ----------------------------
# How often justifiable: Terrorism as a political, ideological or religious mean
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q192) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q192) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q192, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often justified: Terrorism as a political, ideological or religious mean") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = -1) + 
  facet_wrap(~ Q260)

# How often justifiable: Homosexuality
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q182) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q182) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q182, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often justified: Homosexuality") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = 1) + 
  facet_wrap(~ Q260)

# How often justifiable: Parents beating children
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q190) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q190) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q190, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often justified: Beating up children") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = 1) + 
  facet_wrap(~ Q260)

# How often justifiable: Someone accepting a bribe in the course of their duties
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q181) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q181) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q181, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "How often justified: Accepting a bribe") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = 1)

# How justifiable: Monitor all e-mails and any other information exchanged on the Internet
df %>% 
  filter(!is.na(B_COUNTRY) & !is.na(Q197) & !is.na(Q260)) %>% 
  filter(B_COUNTRY %in% countries) %>% 
  group_by(Q260, B_COUNTRY, Q197) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = B_COUNTRY, fill=Q197, y=prop, order = prop)) + 
  geom_bar(position="fill", stat="identity") + 
  xlab("Country") +
  ylab("Proportion of population") +
  labs(fill = "Should gov't monitor the internet") +
  theme(legend.position="top") +
  coord_flip() +
  scale_fill_viridis(option="magma", discrete = TRUE, direction = 1)
