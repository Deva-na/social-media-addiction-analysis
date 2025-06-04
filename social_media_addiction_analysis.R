#all packages
library(readxl)
library(tidyverse)
library(dplyr)
#__________________________________________________________________________
#data 
data <- read.csv("C:/Users/admin/Downloads/Students Social Media Addiction.csv")
head(data)# to check the variables 
str(data) # to identify the structure
colSums(is.na(data))#to under stand the missing value #no missing value
summary(data)

#_________________________________________________________________________
#rename the variable 
library(dplyr)

data <- data %>%
  rename(
    stdid = Student_ID,
    age = Age,
    gender = Gender,
    aclvl = Academic_Level,
    country = Country,
    avgduh = Avg_Daily_Usage_Hours,
    mp = Most_Used_Platform,
    r= Affects_Academic_Performance,
    slphr= Sleep_Hours_Per_Night,
    menhlt= Mental_Health_Score,
    rs= Relationship_Status,
    smc= Conflicts_Over_Social_Media,
    ascr = Addicted_Score
  )
head(data)
str(data)
#_____________________________________________________________________
##factorisation of data 
data1 <- data %>%
  mutate(
    gender = as.factor(gender),
    aclvl = as.factor(aclvl),
    country = as.factor(country),
    mp = as.factor(mp),
    r = as.factor(r),
    rs = as.factor(rs)
  )
sapply(data1, class)
#-------------------------------------------------------------------------
#descriptive statistics 
# sumary of numeric variable 
summary(data1[, c("avgduh", "slphr", "ascr", "menhlt")])
# frequecny table for categorical variable 
table(data1$gender)
table(data1$aclvl)
table(data1$mp)
table(data1$r)
table(data1$rs)
#-----------------------------------------------------------------
#visualisation 
# Barplot: Gender
ggplot(data1, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

# Histogram: Avg Daily Usage
ggplot(data1, aes(x = avgduh)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Average Daily Usage of Social Media", x = "Hours", y = "Number of Students") +
  theme_minimal()

# Boxplot: Addiction Score by Gender
ggplot(data1, aes(x = gender, y = ascr, fill = gender)) +
  geom_boxplot() +
  labs(title = "Addiction Score by Gender", x = "Gender", y = "Addiction Score") +
  theme_minimal()
#addiction score by  academic level 
ggplot(data1, aes(x = aclvl, y = ascr, fill = aclvl)) +
  geom_boxplot() +
  labs(title = "Addiction Score by Academic Level", x = "Academic Level", y = "Addiction Score") +
  theme_minimal() +
  theme(legend.position = "none")

#Categorize Addiction Score (Low, Medium, High) for grouped barplots
data1 <- data1 %>%
  mutate(ascr_cat = cut(ascr,
                        breaks = c(1,4,7,9),
                        labels = c("Low", "Medium", "High"),
                        right = TRUE))

# Barplot Addiction Category by Gender
ggplot(data1, aes(x = ascr_cat, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Addiction Level Category by Gender", x = "Addiction Level", y = "Count") +
  scale_fill_brewer(palette = "Set2", name = "Gender") +
  theme_minimal()
# Barplot: Academic Performance Affected (Yes/No) by Gender
ggplot(data1, aes(x = r, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Academic Performance Affected by Gender", x = "Affects Academic Performance", y = "Count") +
  scale_fill_brewer(palette = "Set2", name = "Gender") +
  theme_minimal()
#most used app vs gender 
ggplot(data1, aes(x = mp, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Most Used Social Media Platform by Gender", x = "Platform", y = "Count") +
  scale_fill_brewer(palette = "Set1", name = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#scatered plot addition vs mental health
ggplot(data1, aes(x = ascr, y = menhlt)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Addiction Score vs Mental Health Score", x = "Addiction Score", y = "Mental Health Score") +
  theme_minimal()
#addition score vs sleep hour
ggplot(data1, aes(x = ascr, y = slphr)) +
  geom_point(alpha = 0.6, color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Addiction Score vs Sleep Hours Per Night", x = "Addiction Score", y = "Sleep Hours") +
  theme_minimal()

# Scatter plots with regression lines

# Addiction Score vs Mental Health
ggplot(data1, aes(x = ascr, y = menhlt)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Addiction Score vs Mental Health Score",
       x = "Addiction Score", y = "Mental Health Score") +
  theme_minimal()

# Addiction Score vs Sleep Hours
ggplot(data1, aes(x = ascr, y = slphr)) +
  geom_point(alpha = 0.6, color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Addiction Score vs Sleep Hours Per Night",
       x = "Addiction Score", y = "Sleep Hours") +
  theme_minimal()

# Addiction Score vs Average Daily Usage
ggplot(data1, aes(x = ascr, y = avgduh)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(title = "Addiction Score vs Average Daily Usage",
       x = "Addiction Score", y = "Average Daily Usage (hours)") +
  theme_minimal()
#____________________________________________________________________________
#group comparison 
#1.addition score by gender
# normality test 
shapiro.test(data1$ascr[data1$gender == "Male"])
shapiro.test(data1$ascr[data1$gender == "Female"])
#non parametric test 
wilcox.test(ascr ~ gender, data = data1)
# 2. Addiction Score by Academic Level
# Check normality by group (repeat Shapiro for each academic level)
library(dplyr)
data1 %>%
  group_by(aclvl) %>%
  summarise(p_value = shapiro.test(ascr)$p.value)
# Or Kruskal-Wallis test (if not normal)
kruskal.test(ascr ~ aclvl, data = data1)
#pair wise comparisonn 
pairwise.wilcox.test(data1$ascr, data1$aclvl, p.adjust.method = "bonferroni")
#--------------------------------------------------------------------------
#association
#chi squre test
# 1. gender vs accaemic performance 
# Cross-tabulation
table_gender_r <- table(data1$gender, data1$r)

# Chi-square test
chisq.test(table_gender_r)
# 2. addition category vs accademic performance
# Make sure ascr_cat and r are factors
table_ascr_r <- table(data1$ascr_cat, data1$r)

# Chi-square test
chisq.test(table_ascr_r)
# 3. most used vs gender
# Cross-tabulation
table_mp_gender <- table(data1$mp, data1$gender)

# Chi-square test
chisq.test(table_mp_gender)
#____________________________________________________________________-
# correlation 


# Select variables for correlation
cor_data <- data1 %>% select(ascr, menhlt, slphr, avgduh)

# Compute Pearson correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
print(cor_matrix)

# Correlation tests
cor.test(data1$ascr, data1$menhlt)
cor.test(data1$ascr, data1$slphr)
cor.test(data1$ascr, data1$avgduh)

# Visualize correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.8, diag = FALSE)





