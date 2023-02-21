### Contraceptives uptake in Fiji 1974 data analysis

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)


# Load the data
wfs <- readRDS("fiji-wfs-1974-analytical.rds")


## exclude people who are infecund, currently pregnant,  not exposed to current contracept 
#### exclusion 
fiji<-subset(wfs, exposure_status != "Not fecund")
fiji1<-subset(fiji, exposure_status != "Pregnant")
fiji2<-subset(fiji1, curr_contracept != "Not exposed" )
fiji3<-subset(fiji2, exposure_status != "Self-spouse sterlzed")
fiji4<-subset(fiji3, exposure_status != "Not exposed")


# Create the a column where all the "Not using" and "Using inefficient" will return 1, and others return 0. 
wfs_contra_groups <- fiji4 %>%
  mutate(
    contra_binary = ifelse(curr_contracept %in% c("Using efficient"),
                           0,1)
  )

####-----------------------------------------------------------------------------

wfs_contra_groups <- wfs_contra_groups %>% 
  filter(!is.na(region)) %>% # remove rows with NA in region column
####-----------------------------------------------------------------------------

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
# Create the region variable as a factor with "Western" as the reference category
wfs_contra_groups$region <- factor(wfs_contra_groups$region, levels = c("Western", "Eastern", "Northern", "Central"))


model <- glm(contra_binary ~ region, wfs_contra_groups, family = "binomial")

# print the model summary 
summary(model)

# summary 
p_values <- summary(model)$coefficients [,"Pr(>|z|)"]
print(p_values)

# Extract the coefficients and standard errors from the model fit summary 
coefs <- coef(model)
se <- sqrt(diag(vcov(model)))

print(coefs)
print(se)

# calculate the ORs and CIs 
ors <- exp(coefs)
ci <- exp(coefs -(1.96 * se))

print(ors)
print(ci)

or_df <- data.frame(predictor = names(coefs),
                    or = ors,
                    lower = ci,
                    upper = exp(coefs + (1.96 * se)))

### I don't want the intercept to be shown in the graph so I am going to delete the first row in the dataframe.
or_df <- or_df[-1,]

# create a forest plot 
ggplot(or_df, aes(y = predictor, x = or, xmin = lower, xmax = upper)) +
  geom_pointrange() +
  labs(x = "OR (95% CI)", y = "") + 
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  annotate("text", x = .8, y = 6.4, label = "") + 
  annotate("text", x = 1.2, y = 6.4, label = "") +
  annotate("text", x = 2, y = 6.8, label = "")



####-----------------------------------------------------------------------
### logistic regression of contraceptive uptake adjusted for ethnicity 
model1 <- glm(contra_binary ~ region + ethnicity, wfs_contra_groups, family = "binomial")



# print the model summary 
summary(model1)

# summary 
p_values1 <- summary(model1)$coefficients [,"Pr(>|z|)"]
print(p_values1)

# Extract the coefficients and standard errors from the model fit summary 
coefs1 <- coef(model1)
se1 <- sqrt(diag(vcov(model1)))

print(coefs1)
print(se1)

# calculate the ORs and CIs 
ors1 <- exp(coefs1)
ci1 <- exp(coefs1 -(1.96 * se1))

print(ors1)
print(ci1)

or_df1 <- data.frame(predictor = names(coefs1),
                    or = ors1,
                    lower = ci1,
                    upper = exp(coefs1 + (1.96 * se1)))



or_df1 <- or_df1[-9,]
or_df1 <- or_df1[-9,]

# create a forest plot 
ggplot(or_df1, aes(y = predictor, x = or, xmin = lower, xmax = upper)) +
  geom_pointrange() +
  labs(x = "OR (95% CI)", y = "") + 
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  annotate("text", x = .8, y = 6.4, label = "") + 
  annotate("text", x = 1.2, y = 6.4, label = "") +
  annotate("text", x = 2, y = 6.8, label = "") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))



        
##### Use VIF to test collinearity
library(car)
model2 <- glm(contra_binary ~ region + religion + ethnicity + age + education + education + occupation, data = wfs_contra_groups, family = "binomial")
vif(model2)

# Calculate the VIF values
vif_values <- vif(model2)

# Assuming the original data frame has columns 'a', 'b', 'c', 'd', 'e'
vif_values <- c(3, 7, 7, 1, 3, 9)
names(vif_values) <- c('region', 'religion', 'ethnicity', 'age', 'education', 'occupation')
vif_df <- data.frame(predictor = names(vif_values), vif = vif_values)

# Plot the VIFs as a bar plot
ggplot(vif_df, aes(x = predictor, y = vif)) +
  geom_bar(stat = 'identity', fill = 'steelblue', width = 0.5) +
  coord_flip() + 
  labs(x = '', y = 'VIF') + 
  theme_minimal() + 
  geom_hline(yintercept = 5, linetype = 'dashed', color = 'red', alpha = 0.7)







   