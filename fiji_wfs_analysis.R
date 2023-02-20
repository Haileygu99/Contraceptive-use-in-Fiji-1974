### Contraceptives uptake in Fiji 1974 data analysis

library(tidyverse)
library(dplyr)
library(tidyr)


# Load the data
wfs <- readRDS("fiji-wfs-1974-analytical.rds")


## exclude people who are infecund, currently pregnant,  not exposed to current contracept 
#### exclusion 
fiji<-subset(wfs, exposure_status != "Not fecund")
fiji1<-subset(fiji, exposure_status != "Pregnant")
fiji2<-subset(fiji1, curr_contracept != "Not exposed" )
fiji3<-subset(fiji2, exposure_status != "Self-spouse sterlzed")
fiji4<-subset(fiji3, exposure_status != "Not exposed")


# Create the a column where all the "Not using" and "Using inefficient" will return 0, and others return 1. 
wfs_contra_groups <- fiji4 %>%
  mutate(
    contra_binary = ifelse(curr_contracept %in% c("Not using","Using inefficient"),
                           0,1)
  )


# Removing all the NA's in the column "region"
# All the 0s in the contra_binary --> "Not using", 1s --> "Using"
wfs_contra_groups <- wfs_contra_groups %>% 
  filter(!is.na(region)) %>% # remove rows with NA in region column
  mutate(contra_binary = ifelse(contra_binary == 0, "Not using", "Using"))


###### Descriptives 
#summary of the characteristics
summary(wfs_contra_groups)
sd(wfs_contra_groups$age)


#### Histogram to see the difference in frequency of the contraceptive use between women
library(ggplot2)

ggplot(wfs_contra_groups, aes(x = factor(contra_binary), fill = factor(contra_binary))) +
  geom_bar(color = "gray50", alpha = 0.5, width = 0.5) +
  labs(title = "Frequency of Contraceptive Use", x = "Use of Contraceptives", y = "Number of Women") +
  scale_fill_manual(values = c("gray50", "gray50")) +
  theme_classic() +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"))



#### Aim 1:
### Do difference in contraceptive uptake vary by region in fecund women?
## H0: There is no difference in contraceptive uptake by region.
## H1: There is a difference in contraceptive uptake by region. 
# create a contingency table of the two variables "region" and "contra_binary"
# the resulting table shows the count of observations in each combination of categories between the two variables.
wfs_contra_table<-table(wfs_contra_groups$region, wfs_contra_groups$contra_binary)
wfs_contra_table


# Create a matrix of observed values
observed <- matrix(c(712, 312, 760, 413, 449, 133, 72, 35), nrow = 4, ncol = 2, byrow = TRUE)

# The expected values represent what we would expect to see under the null hypothesis, which is a hypothesis that assumes no difference between the groups being compared.
# Calculate the expected values
result <- chisq.test(observed)
expected <- result$expected

# Print the expected values
expected



# to check if there is association between the two variables.
chisq.test(wfs_contra_table)
?chisq.test





#### Aim 2
### Are differences in contraceptive uptake in different regions explained by differences in ethnicity in fecund women?
## H0: The odds ratios of not using contraceptives between different regions is equal to 1.
## H1: The odds ratio of not using contraceptives between regions is not equal to 1. 




### ---------------------------------------------------------
wfs_religion_table<-table(wfs_contra_groups$religion, wfs_contra_groups$contra_binary)
wfs_religion_table


chisq.test(wfs_religion_table)


   