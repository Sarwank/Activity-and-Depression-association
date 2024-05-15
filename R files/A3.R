library(dplyr)
library(readxl)
library(ggplot2)

P <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/scores.csv")
View(P)

P$madrs1[is.na(P$madrs1)] <- 5
P$melanch[is.na(P$melanch)] <- 0

# t-Test for efficiency of the treatment 
P <- as.data.frame(P)
prepost <- data.frame(P$number, P$madrs1, P$madrs2)
prepost <- na.omit(prepost)
View(prepost)

library(reshape2)
melt1 <- melt(prepost, id.vars = 'P.number')

melt1 <- data.frame(melt1, c(rep('pre', 23), rep('post', 23)))
View(melt1)
names(melt1)[4] <- 'type'

melt1 %>% group_by(type) %>%
         summarise(
  count = n(),
  mean = mean(value),
  sd = sd(value)
)

boxplot(value~type, 
        data = melt1,
        main="Pre and post mean",
        ylab="Madrs score",
        xlab="Type(before and after treatment)",
        col='steelblue')


differences <- with(melt1, value[type == "post"] - value[type == "pre"])

#Shapiro-wilk test for normality 
shapiro.test(differences)
# data is normally distributed

t.test(value ~ type, data = melt1, paired = TRUE)
#Rejecting null hypothesis(p-value < 0.05), hence significant difference between between.
#Mean difference is negative.