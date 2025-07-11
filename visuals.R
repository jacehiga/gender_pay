---
title: "Gender Pay Discrepencies"
Authors: KAIONA APIO and JACE HIGA
date: "2024-10-18"
output: pdf_document
---

```{r}
# Installing Packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2)
library(colorspace)
library(forcats)
```

```{r}
# Looking at Data
pay <- read.csv("Glassdoor Gender Pay Gap.csv")

str(pay)
```

```{r}
gender <- pay %>%
  count(Gender)
```

```{r}
ggplot(gender, aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) + # Clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(size = 12), 
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(0, 600))
```

```{r}
education <- pay %>%
  count(Education)
```

```{r}
education$Education <- factor(education$Education, levels = c("High School", "College", "Masters", "PhD"))

ggplot(education, aes(x = Education, y = n, fill = Education)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Education Distribution",
    x = "Education",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) + # Clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(size = 12), 
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(0, 300))
```


```{r}
# Word Cloud for Department 
#install.packages("tidytext")
library(tidytext)

# Tokenizing
token_data <- pay %>%
  unnest_tokens(word, JobTitle)

count_words <- token_data %>%
  count(word) %>%
  arrange(desc(n))
count_words
```

```{r}
#CLOUD (Please Do Not Grade)
#install.packages("wordcloud")
library(wordcloud)

wordcloud(words = count_words$word,
          freq = count_words$n,
          rot.per = 0.25,
          colors = "lightblue")

text(x = 0.5, 
     y = 1, 
     "Frequently Held Positions", 
     cex = 1.3)
text(x = 0.5, 
     y = 0.95, 
     "These words appeared most frequently in job titles", 
     cex = 0.9)
text(x = 0.5, 
     y = 0, 
     "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
     cex = 0.5)

```
## This is a wordcloud that we made for fun, it was not really that helpful because we didn't have a good variable to use it on. Nonetheless, we included it in our sketches and coded for it, so I (Jace) am gonna leave it in here. However, something interesting we could do later is to make wordclouds by gender. 

```{r}
#Separating Gender Pay by Department 
dept <- pay %>%
  group_by(JobTitle, Gender) %>%
  summarise(mean_BasePay = mean(BasePay, na.rm = TRUE))

dept
```

```{r}
# Graphing Side by Side Bar Charts

ggplot(dept, aes(JobTitle, mean_BasePay, fill = Gender)) +
  geom_col(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Disparities in Departmental Compensation", 
    subtitle = "The differences in pay within various departments",
    caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
    x = "Department", 
    y = "Base Pay"
  ) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
    axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
    axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
    axis.text.x = element_text(size = 10, family = "sans"),  
    axis.text.y = element_text(size = 10, family = "sans"),
    plot.caption = element_text(hjust = 0, family = "sans")) +
  geom_text(aes(label = paste0("$", round(mean_BasePay, 0))),
    position = position_dodge(width = 0.9), 
    vjust = 0.5, hjust = 1, size = 3, color = "white") +
  scale_fill_manual(values = c("Male" = "purple", "Female" = "darkblue")) +
  ylim(0, 150000)

```
## This a side-by-side bar graph for mean pay differences between men and women across various departments. There are ten different departments and within those there are five in which men are paid and five where women are paid more. This data doesn't really show a profound pay gap and if we are just counting individual departments then this is even. However, this averages could also be skewed by a few woman who make a lot of money, so it probably something to look at later down the line. 

```{r}
# Faceted Plot for Differences in Pay per Gender for Different Job Titles within Departments
ggplot(pay, aes(x = JobTitle, y = BasePay, fill = Gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") + 
  facet_wrap(~ Dept) +
  labs(
    title = "Is The Pay Gap as Bad as We Thought?",
    subtitle = "Within the different departments there are many jobs in which women\nget paid more than men",
    caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
    x = "Different Jobs by Department", 
    y = "Mean Base Pay") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
    axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
    axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0)) +
  scale_fill_manual(values = c("Male" = "purple", "Female" = "darkblue")) +
  ylim(0, 150000)

```
## We then decided to facet for the different jobs within each department in an effort to indentify an trends there. As we can see it is more of the same as the side-by-side bar graph we saw in the last example. There are many jobs within the departments in which the basepay for women is greater than for men. It seems that a logical next move could be to analyze other varibales that might affect the pay such as age, specific jobs, seniority levels, etc. to see any descrepencies within pay for that. 
```{r}
# Creating Graph for Pay Progression by Gender and Age
ggplot(pay, aes(Age, BasePay + Bonus, color = Gender)) +
  geom_smooth(se = FALSE) +
  geom_point(alpha = 0.3) +
   labs(title = "Age and Earnings: The Persistent Pay Gap", 
        subtitle = "The differences in pay as people age remain consistent",
        caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
        x = "Age", 
        y = "Base Pay") +
   theme_minimal() +
   theme(axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
    axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
    axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0)) +
  scale_color_manual(values = c("Male" = "purple", "Female" = "darkblue")) +
  ylim(0, 150000)
```
## The next graph displays basepay as people age. We included a geom_point() layer in there because it would allow us to see if there were any potential outliers as well as areas that were more concentrated. We then added a geom_smooth() layer which would allow us to compare the differences between men and women and through it we can see that there is a clear and pretty consistent pay group throughout the different ages. 

```{r}
pay$JobTitle <- factor(pay$JobTitle, levels = c("Marketing Associate", "IT",  "Warehouse Associate", "Driver","Data Scientist", "Graphic Designer", "Sales Associate","Financial Analyst" , "Manager", "Software Engineer"))

ggplot(pay,aes(x=JobTitle, fill=Gender))+
  geom_bar(position="fill")+
  scale_fill_manual(values=c(Male="purple", Female="darkblue"))+
  labs(title = "Which jobs are gender dominated?",subtitle = "Jobs Held by Male vs. Female Tech Employees", caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)", x = "", y = "") +
  coord_flip()+
  theme_minimal()+
  theme(axis.title.x = element_blank(), plot.title = element_text(size = 16, face = "bold", hjust = 0.5),plot.subtitle = element_text(size = 10, hjust = 0.5),axis.title.y = element_blank())
```
## This plot shows the main fields in tech that are either male or female dominated. This helps us tell our story further by looking at what fields female workers are most likely to be employed in, which could allow us to further compare the wage differences across jobs.

```{r}
# Heat map to compare Age and Pay
ggplot(pay, aes(x=Age, y=Gender, fill=(BasePay+Bonus)))+
  geom_tile(width=1, height=.75)+
  scale_fill_continuous_sequential("Emrld")+
  labs(title = "Age-Based Pay Distribution by Gender",
       subtitle = "Salaries of Male and Female Tech Employees Over Various Ages",
       caption = "Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
       x = "",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
    axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
    axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank())
```
## This plot helps us see who the highest paid individuals of the dataset are, and apparently that individual is a man in his late fifties. This was a fun way to use a geometry that I usually do not interact with. This graph also helps us further visualize the distribution of wealth among ages, similar to our other graph titled "Age and Earnings: The Persistent Pay Gap" and can act as a precursor or added context to the topic.

```{r}
ggplot(pay, aes(x = Age)) +
  geom_histogram(binwidth = 2, color = "purple", fill = "#D3D3D3") +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) + # Clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(size = 12), 
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(0, 65))
```


```{r}
pay2<-pay%>%
  mutate(AgeBin=cut(Age, breaks=c(0, 25, 35, 45, 55, 65)))

```

```{r}
pay2%>%
  count(AgeBin)
```

```{r}
payGroups<-pay2%>%
  group_by(Gender, AgeBin, Education, Dept)%>%
  summarise(avgBasePay=mean(BasePay, na.rm = FALSE))#%>%
  #pivot_wider(cols=c(AgeBin, Education, Dept), names_from = Gender, values_from = avgBasePay)

payGroups
```

```{r}
payF<-payGroups%>%
  filter(Gender=="Female")
```

```{r}
payM<-payGroups%>%
  filter(Gender=="Male")
```

```{r}
payComparison <- payF %>%
  rename(avgBasePayFemale = avgBasePay) %>%
  left_join(payM %>% rename(avgBasePayMale = avgBasePay),
    by = c("AgeBin", "Education", "Dept")) %>%
  left_join(payGroups)

payComparison
```

```{r}
payComp <- payComparison %>%
  mutate(diff = avgBasePayFemale < avgBasePayMale)

payComp
```

```{r}
ggplot(payComp, aes(avgBasePayFemale, avgBasePayMale, color = diff)) +
  geom_point(aes(shape = Education)) +
  geom_abline(slope = 1, intercept = 0, color = "black")+
  labs(
    title = "Are Men Paid More Than Women?",
    subtitle = "Yes. Pretty often.",
    caption = "Observations are grouped by identical age, department, and education.
    Source: Kaggle (Glassdoor - Analyze Gender Pay Gap)",
    x = "Average Base Pay for a Female Employee", 
    y = "Average Base Pay for a Male Employee") +
  scale_x_continuous(labels = scales::label_dollar())+
  scale_y_continuous(labels = scales::label_dollar())+
  scale_color_manual(values = c("FALSE"="red3", "TRUE"="green2","NA"="grey40"),labels = c("No","Yes","Same"))+
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans"), 
    plot.subtitle = element_text(size = 10, hjust = 0.5, family = "sans"),
    axis.title.x = element_text(size = 9, face = "bold", family = "sans"), 
    axis.title.y = element_text(size = 9, face = "bold", family = "sans"), 
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(hjust = 1),
    legend.title = element_blank())+
   annotate("text", x = 132000, y = 138000, label = "y = x", color = "black", size = 4)
```


```{}
ggplot(payComparison, aes(avgBasePayFemale, avgBasePayMale, color = AgeBin)) +
  geom_point(aes(shape = Education)) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count")
```
