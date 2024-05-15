library(readxl)
library(dplyr)
library(ggplot2)


C <- read_excel("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/New folder/condition_16.xlsx")

C13 <- read_excel("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/New folder/condition_16.xlsx")

C20 <- read_excel("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/New folder/condition_8.xlsx")

C29 <- read_excel("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/New folder/condition_23.xlsx")


na.omit(C)
na.omit(C13)
na.omit(C20)
na.omit(C29)

C$timestamp <- as.POSIXct(C$timestamp)

C$time <- format(C$timestamp, format = "%H:%M:%S")
C$time <- as.numeric(as.POSIXct(C$timestamp))

plot(x = C$time, y = C$activity, type = "b",
     main = "Activity of patient 1",
     xlab = "Time",
     ylab = "Activity")

C$timestamp <- as.POSIXct(C$timestamp)

C$time <- format(C$timestamp, format = "%H:%M:%S")
C$time <- as.numeric(as.POSIXct(format(C$timestamp, format = "%H:%M:%S")))

#Comparing daily patterns
View(C)
C$date <- as.character(C$date)

C$d1 <- ifelse(C$date == "2005-09-23", C$activity, NA)
## for daily meean calculation
CM <- na.omit(C)
View(CM)
summary(CM$activity)


##

C$d3 <- ifelse(C$d2 != NA , C$d2)

c1 <- data.frame(C$time)
View(c1)


library(scales)

C13$timestamp <- as.POSIXct(C13$timestamp)
View(C13)

ggplot(C13, aes(timestamp, activity)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


C20$timestamp <- as.POSIXct(C20$timestamp)

ggplot(C20, aes(timestamp, activity)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


C29$timestamp <- as.POSIXct(C29$timestamp)

ggplot(C29, aes(timestamp, activity)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = date_format("%H:%M:%S"))


# For 13 days
mini_rows <- min(length(C13$timestamp), length(C13$activity), length(C20$activity), length(C29$activity))
mini_rows

C13_timestamp <- C13$timestamp[1:19299]
C13_activity <- C13$activity[1:19299]
C20_activity <- C20$activity[1:19299]
C29_activity <- C29$activity[1:19299]

Combine <- data.frame(C13_activity, C20_activity, C29_activity)

View(Combine)
na.omit(Combine)

boxplot(Combine, col = c("blue", "green", "red"), main = "Variance among activities of 3 different patients for 1 day",
        xlab = "Patients with MADRS- 13, 20 & 29 respectively", ylab = "Activity performed by individuals for around 13 days")

anova <- aov(C13_activity ~ C20_activity + C29_activity, data = Combine)

summary(anova)

## For 1 day
CT <- C13$timestamp[1:1441]
C131 <- C13$activity[1:1441]
C201 <- C20$activity[1:1441]
C291 <- C29$activity[1:1441]

Combine1 <- data.frame(C131, C201, C291)

boxplot(Combine1, col = c("blue", "green", "red"), 
        main = "Variance among activities of 3 different patients for 1 day",
        xlab = "Patients with MADRS- 13, 20 & 29 respectively", 
        ylab = "Activity performed by individuals")

anova1 <- aov(C131 ~ C201 + C291, data = Combine1)

summary(anova1)

## Lineplot
library(reshape2)

Combine2 <- data.frame(CT, C131, C201, C291)
melt <- melt(Combine2, id.vars = "CT")

View(melt)

ggplot(melt, aes(x = CT, y = value, color = variable)) +
  geom_line() +
  labs(x = "CT", y = "Value", color = "Variable") +
  theme_minimal()


View(Combine1)
summary(Combine1)

View(Combine2)
 Morning <- data.frame(Combine2$CT[1:360], Combine1$C131[1:360], Combine1$C201[1:360], Combine1$C291[1:360])
View(Morning)
 summary(Morning)
 
 melt1 <- melt(Morning, id.vars = "Combine2.CT.1.360.")
 ggplot(melt1, aes(x = Combine2.CT.1.360., y = value, color = variable)) +
   geom_line() +
   labs(x = "Combine2.CT.1.360.", y = "Value", color = "Variable") +
   theme_minimal()
 
 Morning1 <- data.frame(Combine1$C131[1:360], Combine1$C201[1:360], Combine1$C291[1:360])
 
 boxplot(Morning1, col = c("blue", "green", "red"), 
         main = "Variance among activities of 3 different patients ",
         xlab = "Patients with MADRS- 13, 20 & 29 respectively", 
         ylab = "Activity performed by individuals")