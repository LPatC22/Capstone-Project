# Project: Data Visualization on Contraception Method Dataset


#Generalizations 

# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)
library(readr)

# Read Contraception Method Dataset
cmc <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", sep=",")

# Attribute Information: 
# 
#    1. Wife's age                     (numerical)
#    2. Wife's education               (categorical)      1=low, 2, 3, 4=high
#    3. Husband's education            (categorical)      1=low, 2, 3, 4=high
#    4. Number of children ever born   (numerical)
#    5. Wife's religion                (binary)           0=Non-Islam, 1=Islam
#    6. Wife's now working?            (binary)           0=Yes, 1=No
#    7. Husband's occupation           (categorical)      1, 2, 3, 4
#    8. Standard-of-living index       (categorical)      1=low, 2, 3, 4=high
#    9. Media exposure                 (binary)           0=Good, 1=Not good
#    10. Contraceptive method used     (class attribute)  1=No-use 
#                                                         2=Long-term
#                                                         3=Short-term

#Add column names
colnames(cmc) <- c("WifeAge","WifeEdu","HusbandEdu","Children","WifeReligion",
                   "Working","HusbandOcc","SOLIndex","MediaExp","Method")

#Have a look at the data structure
str(cmc)

#Convert to factor variables the ones that are categorical
cmc[2:3] <- lapply(cmc[2:3], factor)
cmc[5:10] <- lapply(cmc[5:10], factor)

#Counting Numbers
summary(cmc$Method)
summary(cmc$WifeAge)
summary(cmc$Children)
summary(cmc$WifeReligion)
summary(cmc$Working)

#Woman's age frequency bar
barfill <- "#4271AE"
barlines <- "#1F3552"
q <- ggplot(cmc, aes(x = WifeAge)) +
  geom_histogram(aes(fill = ..count..), binwidth =5) +
  scale_x_continuous(name = "Age Of The Wife",
                     breaks = seq(0, 70, 5),
                     limits=c(0, 70)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency Histogram of Wife's Age")
q
 

#Children Ever Born: Frequency Bar
barfill <- "#4271AE"
barlines <- "#1F3552"
s <- ggplot(cmc, aes(x = Children)) +
  geom_histogram(aes(fill = ..count..), binwidth =3) +
  scale_x_continuous(name = "Number of Children",
                     breaks = seq(0, 25, 2),
                     limits=c(0, 25)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Number of Children Ever Born") +
  scale_fill_gradient("Count", low = "blue", high = "red")
s


#Checking for correlations between attributes

#Checking age distributions as per the contraceptive method used
q + facet_grid(Method~., scales = "free")

#Checking the number of children ever born as per the contraceptive method used
s + facet_grid(Method~., scales = "free")

#What impact does wife's religion has in number of children, method
ggplot(cmc, aes(x = WifeAge, y = Children, color = WifeReligion)) + 
  geom_jitter(alpha = 0.9) + facet_grid(. ~ Method) 
  

#Scatter plot relating WifeAge, Children, and Method 
ggplot(cmc, aes(x = WifeAge, y = Children, color = Method)) + 
  geom_jitter(alpha = 0.9) 



#-----------         -----Hypothesis: Number 1 -------------------------------------------------
#       Age of the wife affects the contraception method she chooses

#Create a contingency table

library(vcd)
tab_ma <- table(cmc$Method, cmc$WifeAge)
tab_ma
chisq.test(tab_ma)
# p-value is < 0.05 which means that we must reject the null hypothesis (attributes not related)
# Thus, the p-value is telling us that wife's age affects the contraceptive method she chooses

#Get marginal frequencies
margin.table(tab_ma,1)  # For Rows
margin.table(tab_ma,2)  # For Columns

#A bar of the relationship between age and method
val = c("#E41A1C", "#377EB8", "#1F3552") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = WifeAge, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_continuous("Age") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Wife's Age")

#---------------------End of Hypothesis Number 1 -------------------------------------


#------------------------Hypothesis: Number 2 ----------------------------------------
#       Husband's education affects the contraception method the wife chooses

tab_mhe <- table(cmc$Method, cmc$HusbandEdu)
tab_mhe
chisq.test(tab_mhe)
# p-value is < 0.05 which means that we must reject the null hypothesis (attributes NOT related)
# Thus, the p-value is telling us that husband's education affects the contraceptive method she 
# chooses

# Frequency bar of husband's education
val = c("plum", "darkgreen", "steelblue") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = HusbandEdu, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_discrete("Husband's Education") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Husband's Education")

#A bar of the relationship between husband's education and method
val = c("plum", "darkgreen", "steelblue") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = HusbandEdu, fill = Method)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer() + 
  scale_x_discrete("Husband's Edu") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Husband's Education")

# ---------------------------- End of Hypothesis Number 2 -----------------------

# -------------------------------Hypothesis Number 3 ----------------------------
#              Wife's Religion affects the contraception method she chooses

# Let's see what the chisq test tells us
tab_mwr <- table(cmc$Method, cmc$WifeReligion)
tab_mwr
chisq.test(tab_mwr)
# The p-value is < 0.05 which means that we must reject the null hypothesis (attributes NOT related)
# Thus, the p-value is telling us that wife's religion affects the contraceptive method she chooses

# Testing geom_bar and scales
val = c("#E41A1C", "#377EB8", "#1F3552") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = WifeReligion, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_discrete("Religion") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Wife's Religion")

# Testing geom_bar and scales - adding a brewed color
val = c("#E41A1C", "#377EB8", "#1F3552") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = WifeReligion, fill = Method)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer() + 
  scale_x_discrete("Religion") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Wife's Religion")

# ---------------------------  End of Hypothesis 3 ------------------------------------

# -------------------------------Hypothesis Number 4 ----------------------------
#              Wife's work status affects the contraception method she chooses

# Let's see what the chisq test tells us
tab_mww <- table(cmc$Method, cmc$Working)
tab_mww
chisq.test(tab_mww)
# p-value is > 0.05 which means that we CANNOT reject the null hypothesis (attributes NOT related)
# Thus, the p-value is telling us that wife's working's DOES NOT affect the contraceptive method 
# she chooses 

# Testing geom_bar and scales
val = c("cyan", "magenta", "grey") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = Working, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_discrete("Work Status") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Wife's Working")

# Testing geom_bar and scales - adding a brewed color
val = c("cyan", "magenta", "grey") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = Working, fill = Method)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer() +  
  scale_x_discrete("Work Status") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar of Wife's Working")

# ---------------------------  End of Hypothesis 4 ------------------------------------


# -------------------------------Hypothesis Number 5 ----------------------------
#              Media Exposure affects the contraception method she chooses

# Let's see what the chisq test tells us
tab_mm <- table(cmc$Method, cmc$MediaExp)
tab_mm
chisq.test(tab_mm)
# The p-value is < 0.05 which means that we must reject the null hypothesis (attributes NOT related)
# Thus, the p-value is telling us that exposure to media affects the contraceptive method she chooses

# Testing geom_bar and scales
val = c("mediumpurple", "mediumorchid3", "mediumpurple4") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = MediaExp, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_discrete("Media Exposure") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar - Media Exposure")

# Testing geom_bar and scales - adding a brewed color
val = c("mediumpurple", "mediumorchid3", "mediumpurple4")  
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = MediaExp, fill = Method)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer() + 
  scale_x_discrete("Media Exposure") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar - Media Exposure")

# ---------------------------  End of Hypothesis 5 ------------------------------------



# -------------------------------Hypothesis Number 6 ----------------------------
#              Standard-of-Living index affects the contraception method she # chooses

# Let's see what the chisq test tells us
tab_ms <- table(cmc$Method, cmc$SOLIndex)
tab_ms
chisq.test(tab_ms)
# The p-value is < 0.05 which means that we must reject the null hypothesis (attributes NOT related)
# Thus, the p-value is telling us that the standard-of-living affects the contraceptive method she chooses

# Testing geom_bar and scales
val = c("mediumpurple", "mediumorchid3", "mediumpurple4") 
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = SOLIndex, fill = Method)) +
  geom_bar(position = "dodge") + 
  scale_x_discrete("Standard-of-living Index") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar - Standard-of-living Index")

# Testing geom_bar and scales - adding a brewed color
val = c("mediumpurple", "mediumorchid3", "mediumpurple4")  
lab = c("No-Use", "Long-term", "Short-term")
ggplot(cmc, aes(x = SOLIndex, fill = Method)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer() + 
  scale_x_discrete("Standard-of-living Index") +
  scale_y_continuous(name = "Count") +
  scale_fill_manual("Method", 
                    values = val,
                    labels = lab) +
  ggtitle("Frequency Bar - Standard-of-living Index")

# ---------------------------  End of Hypothesis 6 ------------------------------------


```
