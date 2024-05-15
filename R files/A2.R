library(dplyr)
library(readxl)
library(ggplot2)

P <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/scores.csv")

P$madrs1[is.na(P$madrs1)] <- 5
P <- P[, -13]

View(P)
str(P)

lm <- lm(madrs1 ~ gender, data = P)
summary(lm)

#Gender does not have clear patterns with depression with P-value 0.6597

# To get the improvement made through the treatment 
P <- na.omit(P)
P$madrs1 <- as.numeric(P$madrs1)
P$madrs2 <- as.numeric(P$madrs2)
P$improvement <- P$`madrs1` - P$`madrs2`


M_F <- P %>% group_by(gender) %>% 
  reframe(avg_madrs1 = mean(madrs1),
            avg_madrs2 = mean(madrs2),
            avg_improvement = mean(improvement))
M_F
#Male patients have better improvement rate of MADRS (i.e.17%) as compared to female patients (i.e. 8%)

Married <- P %>% group_by(marriage) %>% 
  reframe(avg_madrs1 = mean(madrs1),
          avg_madrs2 = mean(madrs2),
          avg_improvement = mean(improvement))

Married
#Married patients showed improvement in MADRS nearly four times of unmarried patients.

c1 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_1.csv")
c2 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_2.csv")
c3 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_3.csv")
c4 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_4.csv")
c5 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_5.csv")
c6 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_6.csv")
c7 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_7.csv")
c8 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_8.csv")
c9 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_9.csv")
c10 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_10.csv")
c11 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_11.csv")
c12 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_12.csv")
c13 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_13.csv")
c14 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_14.csv")
c15 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_15.csv")
c16 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_16.csv")
c17 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_17.csv")
c18 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_18.csv")
c19 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_19.csv")
c20 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_20.csv")
c21 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_21.csv")
c22 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_22.csv")
c23 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/condition/condition_23.csv")

c1$timestamp < as.POSIXct(c1$timestamp)

cm1 <- data.frame(c1$date, ave(c1$activity, c1$date))

cm_11 <- cm1$c1.date[!duplicated(cm1$c1.date)]

cm_12 <- cm1$ave.c1.activity..c1.date.[!duplicated(cm1$ave.c1.activity..c1.date.)]
cma <- data.frame(cm_11,cm_12)

View(cma)

#For Condition 2
c2$timestamp < as.POSIXct(c2$timestamp)

cm2 <- data.frame(c2$date, ave(c2$activity, c2$date))
View(cm2)
cm_21 <- cm2$c2.date[!duplicated(cm2$c2.date)]

cm_22 <- cm2$ave.c2.activity..c2.date.[!duplicated(cm2$ave.c2.activity..c2.date.)]

cma2 <- data.frame(cm_21, cm_22)
View(cma2)

#For Condition 3
c3$timestamp < as.POSIXct(c3$timestamp)

cm3 <- data.frame(c3$date, ave(c3$activity, c3$date))

cm31 <- cm3$c3.date[!duplicated(cm3$c3.date)]

cm32 <- cm3$ave.c3.activity..c3.date.[!duplicated(cm3$ave.c3.activity..c3.date.)]

cma3 <- data.frame(cm31, cm32)
View(cma3)

#For Condition 4
c4$timestamp < as.POSIXct(c4$timestamp)

cm4 <- data.frame(c4$date, ave(c4$activity, c4$date))

cm41 <- cm4$c4.date[!duplicated(cm4$c4.date)]

cm42 <- cm4$ave.c4.activity..c4.date.[!duplicated(cm4$ave.c4.activity..c4.date.)]

cma4 <- data.frame(cm41, cm42)
View(cma4)

#For Condition 3
c5$timestamp < as.POSIXct(c5$timestamp)

cm5 <- data.frame(c5$date, ave(c5$activity, c5$date))

cm51 <- cm5$c5.date[!duplicated(cm5$c5.date)]

cm52 <- cm5$ave.c5.activity..c5.date.[!duplicated(cm5$ave.c5.activity..c5.date.)]

cma5 <- data.frame(cm51, cm52)

#For Condition 3
c6$timestamp < as.POSIXct(c6$timestamp)

cm6 <- data.frame(c6$date, ave(c6$activity, c6$date))

cm61 <- cm6$c6.date[!duplicated(cm6$c6.date)]

cm62 <- cm6$ave.c6.activity..c6.date.[!duplicated(cm6$ave.c6.activity..c6.date.)]

cma6 <- data.frame(cm61, cm62)
View(cma6)

#For Condition 3
c7$timestamp < as.POSIXct(c7$timestamp)

cm7 <- data.frame(c7$date, ave(c7$activity, c7$date))

cm71 <- cm7$c7.date[!duplicated(cm7$c7.date)]

cm72 <- cm7$ave.c7.activity..c7.date.[!duplicated(cm7$ave.c7.activity..c7.date.)]

cma7 <- data.frame(cm71, cm72)
View(cma7)

#For Condition 3
c8$timestamp < as.POSIXct(c8$timestamp)

cm8 <- data.frame(c8$date, ave(c8$activity, c8$date))

cm81 <- cm8$c8.date[!duplicated(cm8$c8.date)]

cm82 <- cm8$ave.c8.activity..c8.date.[!duplicated(cm8$ave.c8.activity..c8.date.)]

cma8 <- data.frame(cm81, cm82)
View(cma8)

#For Condition 9
c9$timestamp < as.POSIXct(c9$timestamp)

cm9 <- data.frame(c9$date, ave(c9$activity, c9$date))

cm91 <- cm9$c9.date[!duplicated(cm9$c9.date)]

cm92 <- cm9$ave.c9.activity..c9.date.[!duplicated(cm9$ave.c9.activity..c9.date.)]

cma9 <- data.frame(cm91, cm92)
View(cma9)

#For Condition 3
c10$timestamp < as.POSIXct(c10$timestamp)

cm10 <- data.frame(c10$date, ave(c10$activity, c10$date))

cm101 <- cm10$c10.date[!duplicated(cm10$c10.date)]

cm102 <- cm10$ave.c10.activity..c10.date.[!duplicated(cm10$ave.c10.activity..c10.date.)]

cma10 <- data.frame(cm101, cm102)
View(cma10)

#For Condition 3
c11$timestamp < as.POSIXct(c11$timestamp)

cm11 <- data.frame(c11$date, ave(c11$activity, c11$date))

cm111 <- cm11$c11.date[!duplicated(cm11$c11.date)]

cm112 <- cm11$ave.c11.activity..c11.date.[!duplicated(cm11$ave.c11.activity..c11.date.)]

cma11 <- data.frame(cm111, cm112)
View(cma11)

#For Condition 3
c12$timestamp < as.POSIXct(c12$timestamp)

cm12 <- data.frame(c12$date, ave(c12$activity, c12$date))

cm121 <- cm12$c12.date[!duplicated(cm12$c12.date)]

cm122 <- cm12$ave.c12.activity..c12.date.[!duplicated(cm12$ave.c12.activity..c12.date.)]

cma12 <- data.frame(cm121, cm122)
View(cma12)

#For Condition 3
c13$timestamp < as.POSIXct(c13$timestamp)

cm13 <- data.frame(c13$date, ave(c13$activity, c13$date))

cm131 <- cm13$c13.date[!duplicated(cm13$c13.date)]

cm132 <- cm13$ave.c13.activity..c13.date.[!duplicated(cm13$ave.c13.activity..c13.date.)]

cma13 <- data.frame(cm131, cm132)
View(cma13)

#For Condition 3
c14$timestamp < as.POSIXct(c14$timestamp)

cm14 <- data.frame(c14$date, ave(c14$activity, c14$date))

cm141 <- cm14$c14.date[!duplicated(cm14$c14.date)]

cm142 <- cm14$ave.c14.activity..c14.date.[!duplicated(cm14$ave.c14.activity..c14.date.)]

cma14 <- data.frame(cm141, cm142)
View(cma14)

#For Condition 3
c15$timestamp < as.POSIXct(c15$timestamp)

cm15 <- data.frame(c15$date, ave(c15$activity, c15$date))

cm151 <- cm15$c15.date[!duplicated(cm15$c15.date)]

cm152 <- cm15$ave.c15.activity..c15.date.[!duplicated(cm15$ave.c15.activity..c15.date.)]

cma15 <- data.frame(cm151, cm152)
View(cma15)

#For Condition 16
c16$timestamp < as.POSIXct(c16$timestamp)

cm16 <- data.frame(c16$date, ave(c16$activity, c16$date))

cm161 <- cm16$c16.date[!duplicated(cm16$c16.date)]
cm161 <- as.data.frame(cm161)
cm161 <- cm161[-1, ]
View(cm161)
cm162 <- cm16$ave.c16.activity..c16.date.[!duplicated(cm16$ave.c16.activity..c16.date.)]

View(cm162)
cma16 <- data.frame(cm161, cm162)
View(cma16)

#For Condition 17
c17$timestamp < as.POSIXct(c17$timestamp)

cm17 <- data.frame(c17$date, ave(c17$activity, c17$date))

cm171 <- cm17$c17.date[!duplicated(cm17$c17.date)]

cm172 <- cm17$ave.c17.activity..c17.date.[!duplicated(cm17$ave.c17.activity..c17.date.)]

cma17 <- data.frame(cm171, cm172)
View(cma17)

#For Condition 18
c18$timestamp < as.POSIXct(c18$timestamp)

cm18 <- data.frame(c18$date, ave(c18$activity, c18$date))

cm181 <- cm18$c18.date[!duplicated(cm18$c18.date)]

cm182 <- cm18$ave.c18.activity..c18.date.[!duplicated(cm18$ave.c18.activity..c18.date.)]

cma18 <- data.frame(cm181, cm182)
View(cma18)

#For Condition 19
c19$timestamp < as.POSIXct(c19$timestamp)

cm19 <- data.frame(c19$date, ave(c19$activity, c19$date))

cm191 <- cm19$c19.date[!duplicated(cm19$c19.date)]

cm192 <- cm19$ave.c19.activity..c19.date.[!duplicated(cm19$ave.c19.activity..c19.date.)]

cma19 <- data.frame(cm191, cm192)
View(cma19)

#For Condition 20
c20$timestamp < as.POSIXct(c20$timestamp)

cm20 <- data.frame(c20$date, ave(c20$activity, c20$date))

cm201 <- cm20$c20.date[!duplicated(cm20$c20.date)]
cm201 <- as.data.frame(cm201)
cm201 <- cm201[1:17, ]
cm202 <- cm20$ave.c20.activity..c20.date.[!duplicated(cm20$ave.c20.activity..c20.date.)]

cma20 <- data.frame(cm201, cm202)
View(cma20)

#For Condition 3
c21$timestamp < as.POSIXct(c21$timestamp)

cm21 <- data.frame(c21$date, ave(c21$activity, c21$date))

cm211 <- cm21$c21.date[!duplicated(cm21$c21.date)]

cm212 <- cm21$ave.c21.activity..c21.date.[!duplicated(cm21$ave.c21.activity..c21.date.)]

cma21 <- data.frame(cm211, cm212)
View(cma21)

#For Condition 24
c22$timestamp < as.POSIXct(c22$timestamp)

cm22 <- data.frame(c22$date, ave(c22$activity, c22$date))

cm221 <- cm22$c22.date[!duplicated(cm22$c22.date)]

cm222 <- cm22$ave.c22.activity..c22.date.[!duplicated(cm22$ave.c22.activity..c22.date.)]

cma22 <- data.frame(cm221, cm222)
View(cma22)

#For Condition 23
c23$timestamp < as.POSIXct(c23$timestamp)

cm23 <- data.frame(c23$date, ave(c23$activity, c23$date))

cm231 <- cm23$c23.date[!duplicated(cm23$c23.date)]

cm232 <- cm23$ave.c23.activity..c23.date.[!duplicated(cm23$ave.c23.activity..c23.date.)]

cma23 <- data.frame(cm231, cm232)
View(cma23)


## STDdev & Variance

P1 <- read.csv("C:/Users/SHRAVAN/Desktop/PE/Depression data/scores.csv")
P1$melanch[is.na(P1$melanch)] <- 0
P1$improvement <- P1$`madrs1` - P1$`madrs2`
P1 <- P1[1:23, ]
View(P1)

cm22 <- as.data.frame(cm22)

CMA <- c(66.56162, 112.2316, 99.49827, 169.7462, 58.41683, 77.69075, 162.1249, 108.5272, 45.43002,
         105.537, 67.14356, 85.51253, 114.2907, 36.6234, 66.05553, 128.3103, 18.17083, 18.70627, 78.569, 31.68331,
         20.86004, 43.1718, 131.7159)
CMA <- as.data.frame(CMA)

P1$SD <- CMA
str(P1)

lm1 <- lm(improvement ~ SD$CMA, data = P1)
summary(lm1)

ggplot(P1, aes(x = SD$CMA, y = improvement)) +
  geom_line( color = 4,
             lwd = 1,
             linetype = 1) + geom_point() +
  labs(x = "SD", y = "Improvement", color = "blue") +
  theme_minimal()



ggplot(P1, aes(x = number, y = improvement, color = gender, group = gender)) +
  geom_line() + geom_point()+
  labs(x = "Subjects", y = "Improvement", color = "Gender") +
  theme_minimal()

# For female
min(length(cm141), length(cm162), length(cm172))

cm141 <- as.data.frame(cm141)
cm142 <- as.data.frame(cm142)
cm162 <- as.data.frame(cm162)
cm172 <- as.data.frame(cm172)
View(cm162)

CMF <- data.frame(cm141[1:16, ], cm142[1:16, ], cm162[1:16, ], cm172[1:16, ])
str(CMF)

library(reshape2)
melt <- melt(CMF, id.vars = "cm141.1.16...")

View(melt)

ggplot(melt, aes(x = cm141.1.16..., y = value, color = variable, group = variable)) +
  geom_line() + geom_point()+
  labs(main = "Daily activity average plot for females", x = "Days", y = "Daily activity average", color = "Variable") 

sd(cm142[1:16, ])
sd(cm172[1:16, ])
sd(cm162[1:16, ])

#Here we can see clear distinction between the daily activity patterns of people with different improvement rating(9, 2 & -4)
#Person with negative improvement growth shows very high SD, i.e. 57.23621, with starting daily average.
#For patients with positive improvement rates (i.e. 2 & 9) the SD remains 18.17083 & 36.6234 respectively.
#with gradually decreasing gradient.
#We can design a treatment module to ensure positive improvement rate - by keeping 18 to 36 SD,
#while gradually reducing the daily activity average.

# For Male
min(length(cm_21), length(cm122), length(cm152))

cm_21 <- as.data.frame(cm_21)
cm_22 <- as.data.frame(cm_22)
cm122 <- as.data.frame(cm122)
cm152 <- as.data.frame(cm152)


CMM <- data.frame(cm_21[1:16, ], cm_22[1:16, ], cm122[1:16, ], cm152[1:16, ])
str(CMM)

library(reshape2)
melt1 <- melt(CMM, id.vars = "cm_21.1.16...")

View(melt1)

ggplot(melt1, aes(x = cm_21.1.16..., y = value, color = variable, group = variable)) +
  geom_line() + geom_point() +
  labs(x = "Days", y = "Daily average", color = "Variable") 

summary(CMM)
#No distinct pattern can be seen, analysis with large data set might help determining clear patterns.

sd(cm_22[1:16, ])
sd(cm122[1:16, ])
sd(cm152[1:16, ])