# loading necessary libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)


survey = read_excel("Debate_Metaphor_Final11.29.xlsx")
# detach("package:reshape")

survey = survey %>%
    gather(condition, value, StrongV:ControlV) %>%
    filter(value == 1) %>%
    select(-value)
levels(survey$condition) = c("Strong Violence","Weak Violence","No Violence")


library(likert)
library(reshape)

# Figure 3 ----------------------------------------------------------------


x = survey[survey$Q1 == 1,] %>% select(Q2)
x$Q2 = as.factor(x$Q2)
x = as.data.frame(x)
names(x)[1] = "How confident are you about this (the outcome of the debate)?"
lik = likert(x, grouping = survey[survey$Q1 == 1,]$condition)
gp = plot(lik, legend.position = "right") 

ggsave(gp, file="fig3.png", width=8, height=3)
ggsave(gp, file="fig3.eps", width=8, height=3)


# Figure 5 ----------------------------------------------------------------

x =  survey[survey$Q1 == 1,] %>% select(3) 
x$Q3 = as.factor(x$Q3)
x = as.data.frame(x)
names(x)[1] = "How much of a lead did the Candidate have after the debate?"
lik = likert(x, grouping = survey[survey$Q1 == 1,]$condition)
plot(lik, legend.position = "right") +  theme(text=element_text(size=16, family="Arial"))
ggsave(gp, file="fig5.png", width=8, height=3)
ggsave(gp, file="fig5.eps", width=8, height=3)

