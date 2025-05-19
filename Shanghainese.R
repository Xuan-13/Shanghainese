#install.packages("DescTools")
library(DescTools)


library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lme4)
library(buildmer)
library(brms)
library(rstan)
library(loo)

SH1 <- read.csv("D:/Université Paris Cité/M1/M1 S2/Compared linguistics French-Mandarin/final project/Question.csv", header=TRUE, sep=",")
SH2 <- read.csv("D:/Université Paris Cité/M1/M1 S2/Compared linguistics French-Mandarin/final project/Bio.csv", header=TRUE, sep=",")
SH = merge(SH1,SH2,by = "Time")

SH$Condition <- factor(
  ifelse(grepl("Sbj-Dislocated", SH$Condition), "Sbj-Dislocated", 
         ifelse(grepl("Sbj-Default", SH$Condition), "Sbj-Default", "Obj-Dislocated")),
  levels = c("Sbj-Default", "Sbj-Dislocated", "Obj-Dislocated"))

t <- SH %>% 
  count(Condition,Answer) %>% 
  group_by(Condition) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2))


pp <- ggplot(data = t, 
             mapping = aes(x=Condition,y=prop,fill=factor(Answer))) +
  geom_col() +
  scale_fill_manual(name = "Answer", values = c("0" = "lightcyan", "1" = "lightsteelblue"), labels = c("NP2", "NP1")) +
  labs(title='Coreference preference of the subordinate pronoun',subtitle = 'Shanghainese Chinese', y = "Proportion") +
  geom_text(aes(label = paste0(round(prop * 100), "%"), color = factor(Answer)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_color_manual(values = c("0" = "black", "1" = "black")) +
  guides(color = "none")

pp

ggsave(filename = "D:/Université Paris Cité/M1/M1 S2/Compared linguistics French-Mandarin/final project/Shanghainese.png", plot = pp, 
       width = 8, height = 6, units = "in", dpi = 300)


SH <- SH %>%
  mutate(S_O = case_when(
    Condition == "Sbj-Default" & Answer == 1 ~ 1,
    Condition == "Sbj-Dislocated" & Answer == 1 ~ 1,
    Condition == "Obj-Dislocated" & Answer == 0 ~ 1,
    TRUE ~ 0
  ))

t1<- SH %>% 
  count(Condition,S_O) %>% 
  group_by(Condition) %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(prop=round(prop,2))

pp1 <- ggplot(data = t1, 
             mapping = aes(x=Condition,y=prop,fill=factor(S_O))) +
  geom_col() +
  scale_fill_manual(name = "S/O", values = c("0" = "lightcyan", "1" = "lightsteelblue"), labels = c("Object", "Subject")) +
  labs(title='Coreference preference of the subordinate pronoun',subtitle = 'Shanghainese Chinese', y = "Proportion") +
  geom_text(aes(label = paste0(round(prop * 100), "%"), color = factor(S_O)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_color_manual(values = c("0" = "black", "1" = "black")) +
  guides(color = "none")

pp1

ggsave(filename = "D:/Université Paris Cité/M1/M1 S2/Compared linguistics French-Mandarin/final project/Shanghainese-SO.png", plot = pp1, 
       width = 8, height = 6, units = "in", dpi = 300)


best_model_sh1 <- buildmer(
  formula = Answer ~ Condition + (Condition || Time) + (Condition || Item), 
  data = SH, family = binomial(link = "logit")
)
formula(best_model_sh1@model)

modelSH1 <- glmer(Answer ~ 1 + Condition + (1 | Item) ,
                  family = 'binomial',
                  data = SH)
summary(modelSH1)
isSingular(modelSH)

modelSH2 <- glm(Answer ~ 1 + Condition,
                  family = 'binomial',
                  data = SH)
summary(modelSH2)
anova(modelSH1, modelSH2)

best_model_sh2 <- buildmer(
  formula = S_O ~ Condition + (Condition || Time) + (Condition || Item), 
  data = SH, family = binomial(link = "logit")
)
formula(best_model_sh2@model)

modelSH3 <- glmer(S_O ~ 1 + Condition + (1 | Item) ,
                  family = 'binomial',
                  data = SH)
summary(modelSH3)
isSingular(modelSH3)

modelSH4 <- glm(S_O ~ 1 + Condition,
                family = 'binomial',
                data = SH)
summary(modelSH4)
anova(modelSH3, modelSH4)
