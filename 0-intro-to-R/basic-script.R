# 0 - Pre setup -----------------------------------------------------------
# clean environment
rm(list=ls()); gc()

# load libraries
library(skimr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)

# 1 - Load data -----------------------------------------------------------
grades_mat <- read.csv('0-intro-to-R/data/student-mat.csv', header = TRUE, sep = ';')
# grades_por <- read.csv('0-intro-to-R/data/student-por.csv', header = TRUE, sep = ';')

# 2 - Data Visualisation and Analysis -------------------------------------
# print the first 6 rows of data
head(grades_mat)

# print the internal structure of the data
str(grades_mat)

# get the summary of data
summary(grades_mat)

# check if there is any missing value
sapply(grades_mat, function(x) sum(is.na(x)))

# data visualisation
hist(grades_mat$G3)

# Use more sophisticated functions
skim(grades_mat)
skimr::skim(grades_mat)

## Check students that dropped
# A G3 value of 0 indicates that the particular student has dropped the subject. 
# Here we compare the data between students who dropped the subject and students who did not.

# extract data of students who dropped the subject
data_drop <- subset(grades_mat, G3 == 0)

# different way to do the same subset 
data_drop <- grades_mat[grades_mat$G3 == 0, ]           # direct condition
data_drop <- grades_mat[which(grades_mat$G3 == 0), ]    # with which - by index
data_drop <- grades_mat[grepl('^0$', grades_mat$G3), ]  # using RegEx - by logic

# extract data of students who did not drop the subject
data_stay <- subset(grades_mat,G3 != 0)

# Get summary of both datasets
skim(data_drop)
skim(data_stay)

data_drop$drop <- TRUE
data_stay$drop <- FALSE

# data_drop$drop <- NULL --- to remove a column

drop_analysis <- rbind(data_drop[,c('G1', 'G2', 'school', 'drop')], data_stay[,c('G1', 'G2',  'school', 'drop')])
drop_analysis$G_mean <- rowMeans(drop_analysis[,c('G1', 'G2')])


ggplot(drop_analysis, aes(drop, G_mean)) + 
  # geom_point() 
  geom_jitter(aes(colour = school), width = 0.25)

## Check grades distribution and relation between semesters
grades <- pivot_longer(grades_mat, 
                       cols = G1:G3,
                       names_to = "grade",
                       values_to = 'score')

# image about boxplots
library(magick)
boxplot_pic <- image_read('0-intro-to-R/data/boxplot.png')
boxplot_pic <- image_scale(boxplot_pic, "700")
print(boxplot_pic)
rm(boxplot_pic)


# regular
ggplot(grades, aes(x=grade, y=score, fill=grade)) + 
  geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point")

# faceted
ggplot(grades, aes(x=grade, y=score, fill=grade)) + 
  geom_boxplot() +
  expand_limits( y=c(0, 20)) +
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point") +
  facet_wrap(~grade, scales = "free")

# linear regression G1 with G2
ggplot(grades_mat,aes(x=G1,y=G2)) +
  geom_point() + 
  geom_smooth(method = 'lm')

lm_G1_G2 <- lm(G1 ~ G2, grades_mat)
summary(lm_G1_G2)

# linear regression G1 with G3
ggplot(grades_mat,aes(x=G1,y=G3)) +
  geom_point() + 
  geom_smooth(method = 'lm')

lm_G1_G3 <- lm(G1 ~ G3, grades_mat)
summary(lm_G1_G3)

# linear regression G2 with G3
ggplot(grades_mat,aes(x=G2,y=G3)) +
  geom_point() + 
  geom_smooth(method = 'lm') #se = FALSE

lm_G2_G3 <- lm(G2 ~ G3, grades_mat)
summary(lm_G2_G3)

## Impact of other variables on last grade
# studytime
ggplot(grades_mat, aes(x = studytime, y = G3)) +
  stat_summary(fun.y = "median", geom ="bar", fill = "#D50032") + 
  ggtitle("G3 (median) versus studytime") +
  scale_x_continuous(breaks=c(1, 2, 3, 4),
                     labels=c("1" = "<2 hours", 
                              "2" = "2 to 5 hours", 
                              "3" = "5 to 10 hours", 
                              "4" = ">10 hours"))

# romantic
ggplot(grades_mat, aes(x = romantic, y = G3)) + 
  stat_summary(fun.y = "median", geom = "bar", fill = "#efb92f") +
  ggtitle("G3 (median) versus romantic") +
  scale_x_discrete(breaks=c("no", "yes"),
                   labels=c("no" = "Not Dating", 
                            "yes" = "Dating"))

# Fjob
ggplot(grades_mat, aes(x = Fjob, y = G3)) + 
  stat_summary(fun.y = "median", geom="bar", fill="#3232ff") + 
  ggtitle("G3 (median) versus Father's job types")


## Diferent way to look at the data

# convert the target variable (G3) into binary (either pass or fail)
# grades less than 10 will be considered as fail and more than or equal to 10 will be considered pass
# assign the result into a new variable called final
grades_mat$final <- factor(ifelse(grades_mat$G3 >= 10, 1, 0), labels = c("fail", "pass"))

# remove G3 variable from the dataset
# grades_mat$G3 <- NULL

# impact of Fjob in pass or fail
ggplot(grades_mat, aes(x = Fjob, group = final, fill = final)) + 
  geom_bar()

Fjob_impact <- group_by(grades_mat, Fjob, final)
Fjob_impact <- summarise(Fjob_impact, total = n())
Fjob_impact

# impact of higher in pass or fail
ggplot(grades_mat, aes(x = higher, group = final, fill = final)) + 
  geom_bar()

higher_impact <- group_by(grades_mat, higher, final)
higher_impact <- summarise(higher_impact, total = n())
higher_impact

# ratios -- base-r
nohigher <- sum(higher_impact[which(higher_impact$higher == 'no'), 'total'])
nohigher_fail <- higher_impact[which(higher_impact$higher == 'no' & higher_impact$final == 'fail'), 'total']

higher <- sum(higher_impact[which(higher_impact$higher == 'yes'), 'total'])
higher_fail <- higher_impact[which(higher_impact$higher == 'yes' & higher_impact$final == 'fail'), 'total']

nohigher_fail/nohigher
higher_fail/higher

# ratios -- dplyr (magrittr)
grades_mat %>% 
  group_by(higher, final) %>% 
  summarise(count = n()) %>% 
  group_by(higher) %>% 
  mutate(ratio = count / sum(count)) 


# correlations
dmy <- dummyVars("~.", data = grades_mat)
newdata <- data.frame(predict(dmy, newdata = grades_mat))
correl1 <-cor(newdata[,c("G3","sex.F","sex.M","Walc","Dalc")])
correl1 %>%
  ggcorr(label = TRUE) +
  ggtitle("Correlation between Alcohol Consumption,Gender and Performance")

# 3 - Exercices -----------------------------------------------------------
# What is the effect in the success rate (final) of the Math Course of the following attributes?

# 3.1 Family size (famsize)


# 3.2 Students with internet access at home (internet) and extra paid classes (paid)
# have higher success?


# 3.3 Effect of traveltime in G3


