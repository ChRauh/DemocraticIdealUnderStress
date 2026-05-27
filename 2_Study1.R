####################################################
# Project:  Democratic ideal under stress
# Task:     Generate results of study 1 (conjoint)
# Authors:  @ChRauh/@jpheisig (19.01.2023)
####################################################

# This script re-produces the results for study 1 reported in the main text and the corresponding appendices


# Originally run on
# R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions, CRAN v1.0.2
library(Hmisc) # Harrell Miscellaneous, CRAN v4.7-0
library(summarytools) # Tools to Quickly and Neatly Summarize Data, CRAN v1.0.1
library(estimatr) # Fast Estimators for Design-Based Inference, CRAN v0.30.6
library(cjoint) # AMCE Estimator for Conjoint Experiments, CRAN v2.1.0
library(margins) # Marginal Effects for Model Objects, CRAN v0.3.26
library(prediction) # Tidy, Type-Safe 'prediction()' Methods, CRAN v0.3.14
library(colorspace) # A Toolbox for Manipulating and Assessing Colors and Palettes, CRAN v2.0-3
library(viridis) # Colorblind-Friendly Color Maps for R, CRAN v0.6.2
library(RColorBrewer) # ColorBrewer Palettes, CRAN v1.1-3
library(patchwork) # The Composer of Plots, CRAN v1.1.1
library(xlsx) # Export results to Excel
library(openxlsx) # Further exporting tools
library(gridExtra) 
library(broom)


# Prepare conjoint data ####

# Raw data prepared as provided IPSOS 
# Containing the data for the conjoint experiment (named "cjXXX", see below)
# Data and codebook permanently available at: https://doi.org/10.7802/2447 
df <-read_rds("./Data/PDTW2.rds")

# Reshape function that converts wide conjoint data provided by IPSOS 
# into # the format expected by cjoint package

cj.reshape <- function(df = data.frame(0), tasknum = numeric(0)) {
  
  d <- df
  
  # Extract everything from the df related to specified task
  task <- d %>% 
    select(id, 
           starts_with(paste0("cjPairID", tasknum)),
           starts_with(paste0("cjChoice", tasknum)),
           starts_with(paste0("cjTask", tasknum)))
  
  # First option in this task #
  option1 <- task %>% 
    select(id, matches("OptionA")) 
  
  # Keep only the (0/1) indicator variable recording whether
  # Option A showed the strong version of attribute level (ending "02")
  # (indicator recording if week version was shown (ending "01") is redundant) and can be dropped
  option1 <- option1 %>% 
    select(id, matches("02$")) 
  
  # Number of attributes set to strong
  option1 <- option1 %>% 
    mutate(cj_nstrict = rowSums(select(.,matches("02$"))))
  
  # Name the attributes, following the order of codes 1-11 we supplied to IPSOS
  names(option1)[2:12] <- c("LeaveHome", "IntTravel", "Leisure", "Shops", 
                            "Schools", "HomeOffice", "Gatherings", "ContactTracing", 
                            "Rallies", "Elections", "FactChecking")
  
  # Task and option identifiers
  option1$task <- tasknum
  option1$option <- 1
  
  
  # Second option in this task #
  option2 <- task %>% 
    select(id, matches("OptionB")) 
  
  # Keep only the strong version of each attribute level
  option2 <- option2 %>% 
    select(id, matches("02$")) 
  
  # Number of attributes set to strong
  option2 <- option2 %>% 
    mutate(cj_nstrict = rowSums(select(.,matches("02$"))))
  
  # Name the attributes, following the order of codes 1-11 we supplied to IPSOS
  names(option2)[2:12] <- c("LeaveHome", "IntTravel", "Leisure", "Shops", 
                            "Schools", "HomeOffice", "Gatherings", "ContactTracing", 
                            "Rallies", "Elections", "FactChecking")
  
  # Task and option identifiers
  option2$task <- tasknum
  option2$option <- 2
  
  # Combine data on options 1 and 2
  result <- rbind(option1, option2) %>% arrange(id)
  
  # Add respondent choice (as a dummy)
  result <- result %>% 
    left_join(task[ , c("id", paste0("cjChoice", tasknum))],
              by = "id") %>% 
    rename(chosen_opt = 16)
  
  # Indicates whether the row was respondent's preferred option
  result$choice <- ifelse(result$option == result$chosen_opt, T, F) 
  
  # Return data frame
  return(result)
}

# Reshape data on experiment 1
exp1 <- rbind(cj.reshape(df, 1),
              cj.reshape(df, 2),
              cj.reshape(df, 3),
              cj.reshape(df, 4),
              cj.reshape(df, 5))


# Store number of attributes with 'strong' restriction
# for each policy a respondent has seen
exp1$cj_nstrict <- rowSums(exp1[, 2:12])
# As factor, lumping the edge categories together
exp1 <-exp1 %>% 
  mutate(cj_nstrict_f = case_when(
    cj_nstrict > 0 & cj_nstrict < 11 ~ cj_nstrict - 1,
    cj_nstrict == 11 ~ cj_nstrict - 2,
    TRUE ~ cj_nstrict
  ))
exp1$cj_nstrict_f <- factor(exp1$cj_nstrict_f,
                            levels = 0:9,
                            labels = c("0-1 restrictions",
                                       "2 restrictions",
                                       "3 restrictions",
                                       "4 restrictions",
                                       "5 restrictions",
                                       "6 restrictions",
                                       "7 restrictions",
                                       "8 restrictions",
                                       "9 restrictions",
                                       "10-11 restrictions"))

# Numeric outcome variable (respondent policy choice)
# cjoint:amce() doesn't accept logical
exp1$choice_num <- as.numeric(exp1$choice) 




# Basic checks of the  experimental data ####

# Balanced?
# There should be no systematic pref for first option etc.
table(exp1$chosen_opt)

# Did randomization work properly?
# SD should be close to .5 
for (i in 2:12) {
  print(names(exp1)[i])
  print(paste("SD: ", sd(as.numeric(exp1[,i]))))
}
# Number of 'strong' restrictions should be distributed roughly normal
table(rowSums(exp1[, 2:12]), useNA = "ifany")

# Missing outcomes?
sum(is.na(exp1$choice_num)) # n of policies
sum(is.na(exp1$choice_num))/2 # n of tasks




# Merge background data ####

# Load cleaned background data (produced by 0_PrepareBackgroundData.R)
bg <- read_rds("./Data/CleanedData.rds")

# Merge to experiment 1 data
exp1 <- exp1 %>% left_join(bg, by = "id")

# How many individuals?
length(unique(exp1$id)) # All of them 

# N per country (should be 1500 each)
table(exp1$countryName, useNA = "ifany") 

#  Clean environment
rm(bg, df)




# Conjoint analyses - pooled ####

# Double Check case numbers, reported in Figure note
nrow(filter(exp1, !is.na(choice_num))) # 89882
table(filter(exp1, !is.na(choice_num))$Country)
length(unique(filter(exp1, !is.na(choice_num))$id)) # 8997


# Visual inspection of choice vs. number of restrictions
p1 <- ggplot(exp1, aes(x = factor(cj_nstrict), y = choice_num)) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) +
  stat_summary(geom = "line", fun = mean) +
  geom_hline(yintercept = .5, linetype = "dashed")+
  labs(x = " \nNumber of policy attributes at the respective 'strong' level",
       y= "Probability that respondent prefers policy\nagainst a random alternative")+
  theme_bw() +
  theme(
    axis.text.x = element_text(),
    axis.text = element_text(color = "black"),
    axis.title = element_text()
  )

p2 <- ggplot(exp1, aes(x = factor(cj_nstrict)))+
  geom_histogram(stat = "count", fill = "black")+
  labs(y = "Observations\n ")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"))

p <- p2+p1+
  plot_layout(ncol = 1,
              heights = c(1,4))

ggsave("./Output/Fig1_ChoiceVsNstrong.png", p, width=16, height = 12, units = "cm")

# Table with exact values for appendix
n_profiles <- ggplot_build(p2)$data[[1]]$count
p_choice <- ggplot_build(p1)$data[[1]][, c("x", "y", "ymin", "ymax")]
fig1_data <- cbind(p_choice, n_profiles)
fig1_data$x <- fig1_data$x - 1 # Num of restrictions goes from 0-11 rather than 1-12
xlsx::write.xlsx(fig1_data, file = "./Output/TableA3_DataFig1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)



# Function to estimate linear probability model
cj_lpm <- function(df, formula, byvar = character(0), cluster = TRUE, weights = TRUE, tidy = FALSE) {
  
  #df <- exp1 
  #formula <- as.formula("choice_num ~ cj_nstrict_f")
  #cluster <- TRUE
  #weights <- TRUE
  #byvar = "Country"
  #tidy = TRUE
  
  formula <- as.formula(formula)
  
  # Overwrite weights with 1s if weight == FALSE
  if (!weights) df$weight <- 1
  
  # Fit model
  if (length(byvar) == 0) {
    if (!cluster) {
      fit <- lm(formula = formula, data = df, weight = "weight")
    }
    if (cluster) {
      fit <- lm_robust(formula = formula, data = df, clusters = id, weight = weight)
    }
  } else {
    df <- df %>% group_by(!!sym(byvar))
    if (tidy) {
      if (!cluster) {
        fit <- df %>%
          group_modify(~ broom::tidy(lm(formula = formula, data = df, weight = "weight")))
      }
      if (cluster) {
        fit <- df %>%
          group_modify(~ broom::tidy(lm_robust(formula = formula, data = ., clusters = id, weight = weight)))
      }  
    } else {
      if (!cluster) {
        fit <- df %>% 
          group_map(~ lm(formula = formula, data = df, weight = "weight"))
      }
      if (cluster) {
        fit <- df %>% 
          group_map(~ lm_robust(formula = formula, data = ., clusters = id, weight = weight))
      }
    }
  }
  
  return(fit) 
  
}


# Estimate effects by type of restriction
results <- cj_lpm(df = exp1, formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)


# Extract coefficient information
coeff <- as.data.frame(cbind(results$coefficients,results$std.error,results$df))
names(coeff) <- c("b", "se", "df")

# Factor names and confidence bounds
coeff <- coeff %>% 
  rownames_to_column(var = "resttype") %>% 
  mutate(
    lb95 = b - qt(.975, df) * se,
    ub95 = b + qt(.975, df) * se,
    lb99 = b - qt(.995, df) * se,
    ub99 = b + qt(.995, df) * se
  )

# Clean up attribute labels
coeff$resttype <- coeff$resttype %>% 
  str_remove("(prohibited|yes|postponed|fines for false information|closed)") %>% 
  str_replace_all("([a-z])([A-Z])", "\\1 \\2")
coeff <- coeff %>% filter(resttype != "(Intercept)")

# Order by strength of effect
coeff$resttype <- as.character(coeff$resttype)
order <- coeff %>% arrange(b)
coeff$resttype <- factor(coeff$resttype, levels = order$resttype)

# Highlight restrictions of democratic rights
coeff$dem <- ifelse(str_detect(coeff$resttype, "Elections|Fact |Rallies"), TRUE, FALSE)

# Plot coefficients
p <- 
  ggplot(coeff, aes(x = b, y = resttype)) +
  geom_point(color = NA) +
  geom_point(data = filter(coeff, !dem), position = position_dodge(.7), color = 'black', size = 3) +
  geom_point(data = filter(coeff, dem), position = position_dodge(.7), color = 'red', size = 3) +
  geom_linerange(data = filter(coeff, !dem), aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), color = 'black', size = 1.5)+
  geom_linerange(data = filter(coeff, !dem), aes(xmin = lb99, xmax = ub99), position = position_dodge(.7), color = 'black', size = 1)+
  geom_linerange(data = filter(coeff, dem), aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), color = 'red', size = 1.5)+
  geom_linerange(data = filter(coeff, dem), aes(xmin = lb99, xmax = ub99), position = position_dodge(.7), color = 'red', size = 1)+        
  geom_vline(xintercept = 0, linetype = "dashed")+
  annotate("rect", ymin =0.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =8.5, ymax = 9.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =10.5, ymax = 11.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  labs(y = "Restriction\n(policy attribute comes in its 'strong' version)\n ",
       x = " \nEffect on the probability that respondents prefer a policy\n(average marginal component effects)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                            'bold', 'bold', 'bold', 'plain', 'plain'), 
                                   color = c('black', 'black', 'black', 'black', 'black', 'black',
                                             'red', 'red', 'red', 'black', 'black'), 
                                   size = 12),
        axis.title = element_text(size = 14))

ggsave("./Output/Fig2_AMCEs_full.png", p, width=22, height = 14, units = "cm")

# We do not need to use ggplot_build here, coeff already contains the plotted data
fig2_data <- select(coeff, resttype, b, lb95, ub95, lb99, ub99)
fig2_data <- arrange(fig2_data, desc(b))
xlsx::write.xlsx(fig2_data, file = "./Output/TableA4_DataFig2.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


# Conjoint analyses - pooled - robustness check for inattentive or quick/slow respondents ####

# Estimation: Full sample 
nrow(unique(exp1 %>% filter(!is.na(choice_num)) %>% select(id))) # N Respondents
nrow(exp1 %>% filter(!is.na(choice_num))) # N Profiles 
results <- cj_lpm(df = exp1, 
                  formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)
coeff.full <- as.data.frame(cbind(results$coefficients,results$std.error,results$df))
names(coeff.full) <- c("b", "se", "df")

# Estimation: Excluding the fastest/slowest 5%, respectively
nrow(unique(exp1 %>% filter(!is.na(choice_num)) %>% filter(!dropExp1Duration) %>% select(id))) # N Respondents
nrow(exp1 %>% filter(!is.na(choice_num))%>% filter(!dropExp1Duration)) # N Profiles 
results <- cj_lpm(df = exp1 %>% filter(!dropExp1Duration), 
                  formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)
coeff.duration <- as.data.frame(cbind(results$coefficients,results$std.error,results$df))
names(coeff.duration) <- c("b", "se", "df")

# Estimation: Excluding respondents having failed one of the two attentiveness checks
nrow(unique(exp1 %>% filter(!is.na(choice_num)) %>% filter(attentiveness >= 1) %>% select(id))) # N Respondents
nrow(exp1 %>% filter(!is.na(choice_num)) %>% filter(attentiveness >= 1)) # N Profiles 
results <- cj_lpm(df = exp1 %>% filter(attentiveness >= 1), 
                  formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)
coeff.att1 <- as.data.frame(cbind(results$coefficients,results$std.error,results$df))
names(coeff.att1) <- c("b", "se", "df")

# Estimation: Excluding respondents having failed both attentiveness checks
nrow(unique(exp1 %>% filter(!is.na(choice_num)) %>% filter(attentiveness == 2) %>% select(id))) # N Respondents
nrow(exp1 %>% filter(!is.na(choice_num)) %>% filter(attentiveness == 2)) # N Profiles 
results <- cj_lpm(df = exp1 %>% filter(attentiveness == 2), 
                  formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)
coeff.att2 <- as.data.frame(cbind(results$coefficients,results$std.error,results$df))
names(coeff.att2) <- c("b", "se", "df")

# Combine and prettify coeff data
coeff <- rbind(coeff.full %>% mutate(group = "Full sample"),
               coeff.duration %>% mutate(group = "Excluding the fastest/slowest 5% of respondents"),
               coeff.att1 %>% mutate(group = "Excluding respondents having failed one of two attentiveness checks"),
               coeff.att2 %>% mutate(group = "Excluding respondents having failed both attentiveness checks"))
coeff <- coeff %>% 
  rownames_to_column(var = "resttype") %>% 
  mutate(
    lb95 = b - qt(.975, df) * se,
    ub95 = b + qt(.975, df) * se,
    lb99 = b - qt(.995, df) * se,
    ub99 = b + qt(.995, df) * se
  )
coeff$resttype <- coeff$resttype %>% 
  str_remove("(prohibited|yes|postponed|fines for false information|closed)") %>% 
  str_replace_all("([a-z])([A-Z])", "\\1 \\2")
coeff <- coeff %>% filter(!str_detect(resttype, fixed("(Intercept)")))
coeff$resttype <- coeff$resttype %>% 
  str_remove_all("[0-9]") %>% 
  factor(levels = order$resttype) # Order from the main model above
coeff$group <- coeff$group %>% 
  factor(levels = c("Full sample",
                    "Excluding the fastest/slowest 5% of respondents",
                    "Excluding respondents having failed one of two attentiveness checks",
                    "Excluding respondents having failed both attentiveness checks"))

# Comparison plot of restriction coefficients
p <- 
  ggplot(coeff, aes(x = b, y = resttype, color = fct_rev(group))) +
  geom_point(color = NA) +
  geom_point(position = position_dodge(.7), size = 3) +
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1.5)+
  geom_linerange(aes(xmin = lb99, xmax = ub99), position = position_dodge(.7), size = 1)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  annotate("rect", ymin =0.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =8.5, ymax = 9.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =10.5, ymax = 11.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  labs(y = "Restriction\n(policy attribute comes in its 'strong' version)\n ",
       x = " \nEffect on the probability that respondents prefer a policy\n(average marginal component effects)",
       color = "") +
  guides(color = guide_legend(reverse = TRUE, nrow = 2))+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.text=element_text(size=12),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                            'bold', 'bold', 'bold', 'plain', 'plain'), 
                                   color = c('black', 'black', 'black', 'black', 'black', 'black',
                                                    'red', 'red', 'red', 'black', 'black'), 
                                                    size = 12),
        axis.title = element_text(size = 14))

ggsave("./Output/FigA9_AMCEs_full_AttentivenessSpeeding.png", p, width=32, height = 24, units = "cm")

# Export
figA9_data <- select(coeff, resttype, group, b, lb95, ub95, lb99, ub99)
figA9_data <- arrange(figA9_data, desc(resttype), group)
xlsx::write.xlsx(figA9_data, file = "./Output/TableA11_DataFigA9.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)



# Conjoint analyses  - by country ####


# Check N by country
complete <- exp1 %>% filter(!is.na(choice_num)) %>% group_by(Country)
complete %>% summarise(profiles = n())
complete %>% summarise(tasks = n()/2)
complete %>% summarise(respondents = length(unique(id)))
rm(complete)

# Estimate models
coeff <- cj_lpm(df = exp1, formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", 
                byvar = "Country", tidy = TRUE)
names(coeff)[1:4] <- c("country", "resttype", "b", "se")
coeff <- coeff %>% ungroup()

# Confidence bounds
coeff <- coeff %>%
  mutate(
    lb83 = b - qt(.917, df) * se,
    ub83 = b + qt(.917, df) * se,
    lb95 = b - qt(.975, df) * se,
    ub95 = b + qt(.975, df) * se,
    lb99 = b - qt(.995, df) * se,
    ub99 = b + qt(.995, df) * se
  )

# Drop intercepts and clean up attribute labels
coeff <- coeff %>% filter(resttype != "(Intercept)")
coeff$resttype <- coeff$resttype %>%
  # str_remove("(prohibited|yes|postponed|fines for false information|closed)") %>%
  str_replace_all("([a-z])([A-Z])", "\\1 \\2")

# Create 11 tables (one per restriction) reporting two-tailed p-values for all country pairs

# Duplicate and join the dataframe to itself for pairwise comparison
coeff_comp <- coeff %>%
  full_join(coeff, by = "resttype", suffix = c("", "2")) %>%
  mutate(
    z = (b - b2) / sqrt(se^2 + se2^2),
    p = 2 * pnorm(-abs(z)),
  ) %>%
  select(resttype, country, country2, p) %>%
  arrange(resttype, country, country2)

# Reshape 
coeff_comp <- coeff_comp %>%
  pivot_wider(names_from = country2, values_from = p)

# Export tables

# Create a new workbook
wb <- openxlsx::createWorkbook()

# number style for Excel export
numStyle <- createStyle(numFmt = "0.000")

# Unique predictors for looping
resttypes <- unique(coeff_comp$resttype)


for (restriction in resttypes) {
  
  # Filter for current resttype
  export <- coeff_comp %>%
    filter(resttype == restriction) %>%
    select(-resttype)
  
  # Set upper triangle and diagonal to NA
  pvals <- as.matrix(export[-1])  # Remove the first column for country names temporarily
  pvals <- pvals * 15 # Bonferroni correction, 15 pairwise compariasons
  pvals[pvals > 1] <- 1 # Cap Bonferroni p-vals at 1
  pvals[upper.tri(pvals, diag = TRUE)] <- NA
  
  export[,2:7] <- round(pvals, digits = 3)
    
  # Add and create each sheet in the workbook
  openxlsx::addWorksheet(wb, restriction)

  # Write colnames
  openxlsx::writeData(wb, sheet = restriction, x = colnames(export), startRow = 1, startCol = 2)
  
  # Write data
  openxlsx::writeData(wb, sheet = restriction, x = export, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, sheet = restriction, style = numStyle, rows = 2:7, cols = 2:7, gridExpand = TRUE)

  
}

# Save the workbook to file
saveWorkbook(wb, "./Output/CountryComp_Coefficients.xlsx", overwrite = TRUE)

# Graphical version

# We need the long version of the data for this
coeff_comp <- coeff %>%
  full_join(coeff, by = "resttype", suffix = c("", "2")) %>%
  mutate(
    z = (b - b2) / sqrt(se^2 + se2^2),
    p = 2 * pnorm(-abs(z)),
  ) %>%
  select(resttype, country, country2, p) %>%
  arrange(resttype, country, country2)

# Bonferroni correction
coeff_comp$p <- coeff_comp$p * 15
coeff_comp$p <- pmin(coeff_comp$p, 1)
coeff_comp$significant <- coeff_comp$p < .05

# ISO codes
coeff_comp <- coeff_comp %>% 
  mutate(
    iso = case_when(
      country == "Germany" ~ "DE",
      country == "Hungary" ~ "HU",
      country == "Japan" ~ "JP",
      country == "Poland" ~ "PL",
      country == "South Korea" ~ "KR",
      country == "Spain" ~ "ES"),
    iso2 = case_when(
      country2 == "Germany" ~ "DE",
      country2 == "Hungary" ~ "HU",
      country2 == "Japan" ~ "JP",
      country2 == "Poland" ~ "PL",
      country2 == "South Korea" ~ "KR",
      country2 == "Spain" ~ "ES")
  )

# Factorize iso vars in reversing order for country2 to get table-like layout
coeff_comp$iso <- factor(coeff_comp$iso, levels = c("DE", "HU", "JP", "PL", "KR", "ES"))
coeff_comp$iso2 <- factor(coeff_comp$iso2, levels = c("ES", "KR", "PL", "JP", "HU", "DE"))

# Remove upper diagonal
# We set it to 1 rather than NA, because the latter results in grey cells 
coeff_comp <- coeff_comp %>% 
  mutate(
    significant = case_when(
      as.numeric(country2) <= as.numeric(country) ~ NA,
      TRUE ~ significant),
    p_char = case_when(
      as.numeric(country2) <= as.numeric(country) ~ "",
      TRUE ~ sprintf("%.3f", p))
)

# Initialize list to store plots
plot_list <- list()

# Generate one plot per resttype
for (rtype in unique(coeff_comp$resttype)) {
  
  plot_data <- filter(coeff_comp, resttype == rtype)
  
  plot <- ggplot(plot_data, aes(x = iso, y = iso2, fill = significant, alpha = .5)) +
    geom_tile(color = "grey", size = .3) +
    scale_fill_manual(values = c("TRUE" = "#ffb09c", "FALSE" = "white"), na.value = "white") +
    geom_text(aes(label = p_char), color = "black", size = 2.5, vjust = 0.5, hjust = 0.5) +
    labs(title = rtype) +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none",
          axis.ticks = element_blank(), 
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank())
  
  plot_list[[rtype]] <- plot
}

combined_plot <- do.call(grid.arrange, c(plot_list, list(nrow = 3)))
ggsave("./Output/CountryComp_Coefficients_Study1.pdf", combined_plot, width = 9.69, height = 6.27, units = "in")
ggsave("./Output/CountryComp_Coefficients_Study1.png", combined_plot, width = 9.69, height = 6.27, units = "in")


# Plot country-specific effects

# Order by strength of overall effect (see above)
coeff$resttype_ordered <- factor(coeff$resttype, levels = order$resttype)

# Mark and isolate democratic freedoms
coeff$dem <- ifelse(str_detect(coeff$resttype, "Elections|Fact |Rallies"), TRUE, FALSE)
res_dem <- filter(coeff, dem)
res_nondem <- filter(coeff, !dem)

# Plot by country

colors <- qualitative_hcl(6, palette = "Dark 3")

p_base <- ggplot(data = coeff, aes(x = b, y = resttype_ordered, color = country)) +
  labs(y = "Restriction\n(policy attribute comes in its 'strong' version)\n ",
       x = "Effect on the probability that respondents prefer a policy\n(average marginal component effects)",
       color = "Country sample: ")+
  theme_bw()

p_full <- p_base + 
  geom_point(position = position_dodge(.7), size = 2)+
  geom_linerange(aes(xmin = lb83, xmax = ub83), position = position_dodge(.7), size = .8)+
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = .4)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c(colors[1], colors[5], colors[2], colors[3], colors[6], colors[4])) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        title = element_text(size = 15),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(face = c('plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain'), 
                                   color = c('black', 'black', 'black', 'black', 'black', 'black',
                                             'black', 'black', 'black', 'black', 'black'), 
                                   size = 12),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14))  


p_full_shaded <- p_full + 
  annotate("rect", ymin =0.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =8.5, ymax = 9.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA) +
  annotate("rect", ymin =10.5, ymax = 11.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)

p_full_shaded

ggsave("./Output/FigA5_AMCEs_ByCountry.png", p_full_shaded, width=22, height = 16, units = "cm")

# We do not need to use ggplot_build here, coeff already contains the plotted data
figA5_data <- select(coeff, resttype_ordered, country, b, lb95, ub95, lb99, ub99)
figA5_data <- arrange(figA5_data, desc(resttype_ordered), country)
figA5_data <- as.matrix(figA5_data)
xlsx::write.xlsx(figA5_data, file = "./Output/TableA7_DataFigA5.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


# Plot democratic freedoms by country 

# Only restrictions on core democratic rights
dem <- coeff %>% 
  filter(str_detect(resttype, "Elections|Fact |Rallies"))
dem$country <- as.character(dem$country)

# Nice labels for plotting
dem$country[dem$country == "South Korea"] <- "South\nKorea"
dem$resttype[dem$resttype == "Elections"] <- "Elections\npostponed"
dem$resttype[dem$resttype == "Fact Checking"] <- "Fines for\nfalse information" # Makes this consistent
dem$resttype[dem$resttype == "Rallies"] <- "Rallies/demonstrations\nprohibited"

# Order by V-Dem LibDem 2020 score
# https://en.wikipedia.org/wiki/V-Dem_Institute
# Germany: 0.833 
# Spain: 0.8 
# South Korea: 0.792
# Japan: 0.731 
# Poland: 0.487 
# Hungary: 0.368

dem$vdem <- NA
dem$vdem[dem$country == "Germany"] <-  0.833
dem$vdem[dem$country == "Spain"] <-  0.8
dem$vdem[dem$country == "South\nKorea"] <- 0.792
dem$vdem[dem$country == "Japan"] <-  0.731
dem$vdem[dem$country == "Poland"] <-  0.487
dem$vdem[dem$country == "Hungary"] <-  0.368

# Order by democracy score
dem$Country_by_vdem <- factor(dem$country)
dem$Country_by_vdem <- reorder(dem$Country_by_vdem, dem$vdem)

# Order democratic restrictions
dem$resttype_ordered = factor(dem$resttype, 
                              levels = c("Elections\npostponed", "Rallies/demonstrations\nprohibited", "Fines for\nfalse information"))

dem1_blue <- filter(dem, resttype == "Elections\npostponed")
dem1_grey <- filter(dem, resttype != "Elections\npostponed")

dem2_blue <- filter(dem, resttype != "Fines for\nfalse information")
dem2_none <- filter(dem, resttype == "Fines for\nfalse information")


# Plot by country and type of restriction

p_base <- ggplot(dem, aes(x=b, y=Country_by_vdem, color = vdem)) + 
  facet_wrap(. ~ resttype_ordered) +
  labs(y = " \nCountry",
       x = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n",
       color = "V-Dem Liberal Democracy Score 2020: ") +
  geom_vline(xintercept = 0, linetype = "dashed")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5)) +
  xlim(-.1, .07)

p_dem <- p_base +
  geom_point(position = position_dodge(.7), size = 4)+
  geom_linerange(aes(xmin = lb83, xmax = ub83), position = position_dodge(.7), size = 2)+
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1)+
  scale_color_continuous_sequential(palette = "Blues", rev = TRUE, begin = .5, end = 1)

p_dem

ggsave("./Output/FigA6_PoliticalFreedoms_ByCountry.png", p_dem, width=22, height = 16, units = "cm")


# We do not need to use ggplot_build here, coeff already contains the plotted data
figA6_data <- select(dem, resttype, country, vdem, b, lb95, ub95, lb99, ub99)
figA6_data <- arrange(figA6_data, resttype, vdem)
figA6_data <- as.matrix(figA6_data)
xlsx::write.xlsx(figA6_data, file = "./Output/TableA8_DataFigA6.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

## Repeat for covid deaths

dem$covdeaths <- NA
dem$covdeaths[dem$country == "Germany"] <-  1191
dem$covdeaths[dem$country == "Spain"] <-  1870
dem$covdeaths[dem$country == "South\nKorea"] <- 55.5
dem$covdeaths[dem$country == "Japan"] <-  147
dem$covdeaths[dem$country == "Poland"] <-  1933
dem$covdeaths[dem$country == "Hungary"] <-  3098

# Order by covid deaths
dem$Country_by_covdeaths <- factor(dem$country)
dem$Country_by_covdeaths <- reorder(dem$Country_by_covdeaths, dem$covdeaths)


p_base <- ggplot(dem, aes(x=b, y=Country_by_covdeaths, color = covdeaths)) + 
  facet_wrap(. ~ resttype_ordered) +
  labs(y = " \nCountry",
       x = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n",
       color = "Cumulative Covid-19 deaths per million people:") +
  geom_vline(xintercept = 0, linetype = "dashed")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 90, vjust = .5)) +
  xlim(-.1, .07)

p_covdeaths <- p_base +
  geom_point(position = position_dodge(.7), size = 4)+
  geom_linerange(aes(xmin = lb83, xmax = ub83), position = position_dodge(.7), size = 2)+
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1)+
  scale_color_continuous_sequential(palette = "Blues", rev = TRUE, begin = .5, end = 1)

p_covdeaths

ggsave("./Output/FigA6_PoliticalFreedoms_ByCovDeaths.png", p_covdeaths, width=22, height = 16, units = "cm")




# Subgroup analyses ####


# Trust in government

exp1$trust_gov_cut <- ifelse(exp1$trust_gov > 5, "high",
                             ifelse(exp1$trust_gov > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(exp1$trust_gov_cut, useNA = "ifany")
plot(exp1$trust_gov, exp1$trust_gov_cut)

nrow(exp1 %>% filter(is.na(trust_gov_cut) & is.na(choice_num))) # Number of missings on either side
nrow(exp1 %>% filter(!is.na(trust_gov_cut) & !is.na(choice_num))) # Number of obs in subgroup analysis
nrow(exp1 %>% filter(!is.na(trust_gov_cut) & !is.na(choice_num))) / 2 # Number of pairwise comparisons
nrow(unique(exp1 %>% filter(!is.na(trust_gov_cut) & !is.na(choice_num)) %>% select(id))) # Number of unique respondents in valid data

grouped <- exp1 %>% group_by(trust_gov_cut)

coeff.trust_gov <- grouped %>%
 group_modify(~ broom::tidy(lm_robust(formula = choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking,
                                      data = ., clusters = id, weight = weight), conf.int = TRUE)) %>%
 rename(group = 1) %>%
 filter(term %in% c("Elections", "Rallies", "FactChecking")) %>% # Democratic freedoms
 filter(!is.na(group))


p.trust_gov <-
  ggplot(coeff.trust_gov, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(.~term) +
  labs(title = "Respondent's trust in the national government",
       x = "",
       y = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))


  
# Satisfaction with political system

exp1$pol_sat_cut <- ifelse(exp1$q27 > 5, "high",
                             ifelse(exp1$q27 > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(exp1$pol_sat_cut, useNA = "ifany")
plot(exp1$q27, exp1$pol_sat_cut)

nrow(exp1 %>% filter(is.na(pol_sat_cut) & is.na(choice_num))) # Number of missings on either side
nrow(exp1 %>% filter(!is.na(pol_sat_cut) & !is.na(choice_num))) # Number of obs in subgroup analysis
nrow(exp1 %>% filter(!is.na(pol_sat_cut) & !is.na(choice_num))) / 2 # Number of pairwise comparisons
nrow(unique(exp1 %>% filter(!is.na(pol_sat_cut) & !is.na(choice_num)) %>% select(id))) # Number of unique respondents in valid data

grouped <- exp1 %>% group_by(pol_sat_cut)

coeff.sat <- grouped %>%
 group_modify(~ broom::tidy(lm_robust(formula = choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking,
                                      data = ., clusters = id, weight = weight), conf.int = TRUE)) %>%
 rename(group = 1) %>%
 filter(term %in% c("Elections", "Rallies", "FactChecking")) %>% # Democratic freedoms
 filter(!is.na(group))


p.sat <- 
  ggplot(coeff.sat, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(.~term) +
  labs(title = "Respondent's satisfaction with the national political system",
       x = "",
       y = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))  

# Combined plot

# p.trust_gov + p.sat +
#   plot_layout(ncol = 1)

coeff <- rbind(coeff.trust_gov %>% mutate(iv = "Trust\nin national government"),
               coeff.sat %>% mutate(iv = "Satisfaction\nwith national political system")) %>% 
  mutate(iv = factor(iv, levels = c("Trust\nin national government",
                                    "Satisfaction\nwith national political system")))


coeff$term[coeff$term == "Elections"] <- "Elections\npostponed"
coeff$term[coeff$term == "FactChecking"] <- "Fines for\nfalse information"
coeff$term[coeff$term == "Rallies"] <- "Rallies/demonstrations\nprohibited"

coeff$term = factor(coeff$term, 
                    levels = c("Elections\npostponed", "Rallies/demonstrations\nprohibited", "Fines for\nfalse information"))



ggplot(coeff, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_grid(term ~ iv) +
  labs(x = "",
       y = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigA7_PoliticalFreedoms_ByPoliticalViews_noLines.png", width=22, height = 20, units = "cm")

# We do not need to use ggplot_build here, coeff already contains the plotted data
figA7_data <- select(coeff, iv, group, term, estimate, conf.low, conf.high)
figA7_data <- arrange(figA7_data, term, iv, group)
figA7_data <- as.matrix(figA7_data)
xlsx::write.xlsx(figA7_data, file = "./Output/TableA9_DataFigA7.xlsx", sheetName = "Sheet1",
          col.names = TRUE, row.names = FALSE, append = FALSE)



# Trust in parties 

exp1$trust_parties_cut <- ifelse(exp1$trust_parties > 5, "high",
                                 ifelse(exp1$trust_parties > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(exp1$trust_parties_cut)
plot(exp1$trust_parties, exp1$trust_parties_cut)

grouped <- exp1 %>% group_by(trust_parties_cut)

coeff.trust_parties <- grouped %>%
  group_modify(~ broom::tidy(lm_robust(formula = choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking,
                                       data = ., clusters = id, weight = weight), conf.int = TRUE)) %>%
  rename(group = 1) %>%
  filter(term %in% c("Elections", "Rallies", "FactChecking")) %>% # Democratic freedoms
  filter(!is.na(group))

coeff.trust_parties$term[coeff.trust_parties$term == "Elections"] <- "Elections\npostponed"
coeff.trust_parties$term[coeff.trust_parties$term == "FactChecking"] <- "Fines for\nfalse information"
coeff.trust_parties$term[coeff.trust_parties$term == "Rallies"] <- "Rallies/demonstrations\nprohibited"

coeff.trust_parties$term = factor(coeff.trust_parties$term, 
                        levels = c("Elections\npostponed", "Rallies/demonstrations\nprohibited", "Fines for\nfalse information"))


p.trust_parties <-
  ggplot(coeff.trust_parties, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(.~term) +
  labs(title = "Respondent's trust in the political parties",
       x = "",
       y = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))

# Save, but this one is not included in Appendix, so we are not exporting data for table
ggsave("./Output/FigXX_PoliticalFreedoms_ByTrustInParties.png", p.trust_parties, width=24, height = 12, units = "cm")


# Self-rated health

exp1$srh_cut <- ifelse(exp1$srh > 5, "Very good",
                           ifelse(exp1$srh > 2, "Average", "Very poor")) %>% 
  factor(levels = c("Very poor", "Average", "Very good"))
table(exp1$srh_cut)
plot(exp1$srh, exp1$srh_cut)

nrow(exp1 %>% filter(is.na(srh) & is.na(choice_num))) # Number of missings on either side
nrow(exp1 %>% filter(!is.na(srh) & !is.na(choice_num))) # Number of obs in subgroup analysis
nrow(exp1 %>% filter(!is.na(srh) & !is.na(choice_num))) / 2 # Number of pairwise comparisons
nrow(unique(exp1 %>% filter(!is.na(srh) & !is.na(choice_num)) %>% select(id))) # Number of unique respondents in valid data

grouped <- exp1 %>% group_by(srh_cut)

coeff.srh <- grouped %>%
  group_modify(~ broom::tidy(lm_robust(formula = choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking,
                                       data = ., clusters = id, weight = weight), conf.int = TRUE)) %>%
  rename(group = 1) %>%
  filter(term %in% c("Elections", "Rallies", "FactChecking")) %>% # Democratic freedoms
  filter(!is.na(group))

coeff.srh$term[coeff.srh$term == "Elections"] <- "Elections\npostponed"
coeff.srh$term[coeff.srh$term == "FactChecking"] <- "Fines for\nfalse information"
coeff.srh$term[coeff.srh$term == "Rallies"] <- "Rallies/demonstrations\nprohibited"

coeff.srh$term = factor(coeff.srh$term, 
                    levels = c("Elections\npostponed", "Rallies/demonstrations\nprohibited", "Fines for\nfalse information"))


p.srh <- 
  ggplot(coeff.srh, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(.~term) +
  labs(title = "Respondent's self-rated health",
       subtitle = "How would you describe your current health?",
       x = "",
       y = "Effect of restriction on the\nprobability that a policy is prefered\n(average marginal component effect)\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))  

ggsave("./Output/FigA8_PoliticalFreedoms_BySelfRatedHealth.png", p.srh, width=24, height = 12, units = "cm")

# We do not need to use ggplot_build here, coeff already contains the plotted data
figA8_data <- select(coeff.srh, group, term, estimate, conf.low, conf.high)
figA8_data <- arrange(figA8_data, term, group)
figA8_data <- as.matrix(figA8_data)
xlsx::write.xlsx(figA8_data, file = "./Output/TableA10_DataFigA8.xlsx", sheetName = "Sheet1",
    col.names = TRUE, row.names = FALSE, append = FALSE)


# Corona infections

table(exp1$q31)
exp1$cinfect <- ifelse(exp1$q31 == 1, "Yes",
                       "No") %>% 
  factor(levels = c("Yes", "No"))

grouped <- exp1 %>% group_by(cinfect)

coeff.cinfect <- grouped %>%
  group_modify(~ broom::tidy(lm_robust(formula = choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking,
                                       data = ., clusters = id, weight = weight), conf.int = TRUE)) %>%
  rename(group = 1) %>%
  filter(term %in% c("Elections", "Rallies", "FactChecking")) %>% # Democratic freedoms
  filter(!is.na(group))

coeff.cinfect$term[coeff.cinfect$term == "Elections"] <- "Elections\npostponed"
coeff.cinfect$term[coeff.cinfect$term == "FactChecking"] <- "Fines for\nfalse information"
coeff.cinfect$term[coeff.cinfect$term == "Rallies"] <- "Rallies/demonstrations\nprohibited"

coeff.cinfect$term = factor(coeff.cinfect$term, 
                        levels = c("Elections\npostponed", "Rallies/demonstrations\nprohibited", "Fines for\nfalse information"))


p.cinfect <- 
  ggplot(coeff.cinfect, aes(x = group, y = estimate, ymax = conf.high, ymin = conf.low, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(.~term) +
  labs(title = "Corona infection",
       subtitle = "Have you or someone close to you fallen seriously ill due to a Coronavirus infection?",
       x = "",
       y = "Effect of restriction on the\nprobability that a policy is preferred\n(average marginal component effect)\ ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12))  

ggsave("./Output/FigXX_PoliticalFreedoms_ByCoronaInfection.png", p.cinfect, width=24, height = 12, units = "cm")
# Not shown in paper or appendix, so no need to export



# Subgroup analysis by marginal means #####

# Function to generate marginal means
mm_maker <- function(restrict, groupvar) {
  
  grouped <- exp1 %>% group_by(!!sym(groupvar), !!sym(restrict))
  
  mms <- grouped %>% 
    group_modify(~ broom::tidy(lm_robust(formula = as.formula("choice_num ~ 1"), 
                                         data = ., clusters = id, weight = weight)))
  mms <- ungroup(mms)
  
  # Write type of restriction into variable
  mms$restriction <- restrict
  # Rename restriction (grouping variable) to "strict", 
  # so we have one column indicating weak or strict condition in combined data
  mms <- rename(mms, strict = !!sym(restrict))
  
  # Alternative conf bounds
  mms <- mms %>% 
    mutate(
    lb83 = estimate - qt(.917, df) * std.error,
    ub83 = estimate + qt(.917, df) * std.error,
    lb95 = estimate - qt(.975, df) * std.error,
    ub95 = estimate + qt(.975, df) * std.error,
    lb99 = estimate - qt(.995, df) * std.error,
    ub99 = estimate + qt(.995, df) * std.error
  )
  
  mms
}

restrictions <- c("LeaveHome", "IntTravel", "Leisure", "Shops", "Schools", "HomeOffice", "Gatherings", "ContactTracing", "Rallies", "Elections", "FactChecking")

# Estimate and plot MMs by 
# 1) trust in gov
# 2) satisfaction w/ pol. system
# 3) cntry


results <- lapply(restrictions, mm_maker, groupvar = "trust_gov_cut")
mms_trust_gov <- data.frame()
for (i in 1:length(restrictions)) {
  mms_trust_gov <- bind_rows(mms_trust_gov,results[i])
}

results <- lapply(restrictions, mm_maker, groupvar = "pol_sat_cut")
mms_pol_sat <- data.frame()
for (i in 1:length(restrictions)) {
  mms_pol_sat <- bind_rows(mms_pol_sat,results[i])
}

results <- lapply(restrictions, mm_maker, groupvar = "Country")
mms_country <- data.frame()
for (i in 1:length(restrictions)) {
  mms_country <- bind_rows(mms_country,results[i])
}
rm(results)

# Create an ordered and nicely labeled 
# version of restrictions
# (following order in Table 1)

addvars <- function(d) {
  d$rest_ordered <-
    factor(d$restriction,
           levels = c("LeaveHome",
                      "IntTravel",
                      "Leisure",
                      "Shops",
                      "Schools",
                      "HomeOffice",
                      "Gatherings",
                      "ContactTracing",
                      "Rallies",
                      "Elections",
                      "FactChecking"),
           labels = c("Leaving home",
                      "International travel",
                      "Leisure facilities",
                      "Shops and services",
                      "Schools",
                      "Home office",
                      "Private gatherings",
                      "Contact tracing",
                      "Rallies/demonstrations",
                      "Elections",
                      "Fact-checking"))

  # Factorize strict, more intuitive to reverse coding here
  d$strict <- factor(d$strict, levels = c(1,0), labels = c("Strict", "Weak"))
  
  
  
  d
}

mms_trust_gov <- addvars(mms_trust_gov)
mms_pol_sat <- addvars(mms_pol_sat)
mms_country <- addvars(mms_country)

# Drop missings
mms_trust_gov <- filter(mms_trust_gov, !is.na(trust_gov_cut))
mms_pol_sat <- filter(mms_pol_sat, !is.na(pol_sat_cut))
mms_country <- filter(mms_country, !is.na(Country))


# Plot
colors <- qualitative_hcl(3, palette = "Dark 3")


mm_plotter <- function(plotdat, groupvar) {
  
  # Determine number of groups and choose colors shapes accordingly
  grps <- select(plotdat, !!sym(groupvar))
  ngrps <- nrow(unique(grps))
  colors <- qualitative_hcl(3, palette = "Dark 3")[ngrps:1]
  shapes <- c(15,16,17,18, 25, 23)[ngrps:1]
  
  p <- ggplot(data = plotdat, aes(x = estimate, y = strict, 
                                 shape = !!sym(groupvar), color = !!sym(groupvar))) +
    geom_point(position=ggstance::position_dodgev(height=0.6), size = 2.5) +
    geom_linerange(aes(xmin = lb83, xmax = ub83), position=ggstance::position_dodgev(height=0.6),
                   size = 1.5)+
    geom_linerange(aes(xmin = lb95, xmax = ub95), position=ggstance::position_dodgev(height=0.6),
                   size = .75)+
    scale_color_manual(values = colors, 
                       guide = guide_legend(reverse = TRUE)) +
    scale_shape_manual(values = shapes, 
                       guide = guide_legend(reverse = TRUE)) +
    facet_wrap(. ~ rest_ordered) +
    labs(y = "Attribute level\n",
         x = "\nMarginal mean\n with 83% (thick) and 95% (thin) confidence interval")+
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(color = "black", size = 12, face = "bold"),
          axis.text.y = element_text(color = "black", size = 12),
          axis.text.x = element_text(color = "black", size = 10),
          legend.text = element_text(size = 12),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom")
  p
  
}

p <- mm_plotter(plotdat = mms_trust_gov, groupvar = "trust_gov_cut")  +
  labs(color = "Trust in government:",
       shape = "Trust in government:") 
ggsave("./Output/MMs_TrustGovernment.png", p, width=24, height = 16, units = "cm")
# Not shown, no need to export


p <- mm_plotter(plotdat = mms_pol_sat, groupvar = "pol_sat_cut") +
  labs(color = "Satisfaction with political system:",
  shape = "Satisfaction with political system:")
ggsave("./Output/MMs_PoliticalSatisfaction.png", p, width=24, height = 16, units = "cm")

# This one needs a broader palette, should we need the plot at all ...
p <- mm_plotter(plotdat = mms_country, groupvar = "Country") +
  labs(color = "Country",
       shape = "Country")
ggsave("./Output/MMs_Country.png", p, width=24, height = 16, units = "cm")
# Not shown, no need to export




# Robustness check - unlikely/implausible scenarios ####

# Sequentially exclude all profile pairs where restrictions on leaving home
# were combined with 0,1,2... restrictions in at least one of the profiles,
# re-running main analysis for each of the resulting samples

# Intuition: scenarios with no/few additional restrictions are implausible
# ("have these politicians gone completely crazy???")

# Modified cj_lpm function that includes additional 
cj_lpm_drop <- function(drop, df, formula, byvar = character(0), cluster = TRUE, weights = TRUE, tidy = FALSE) {
  
  # Drop profile pairs with restrictions on leaving home and "drop" 
  # add'l restrictions from analysis
  
  # Generate identifier for id/task combinations
  # (there must be a more elegant way, but who cares??)
  df <- df %>% group_by(id, task) 
  id_cjpair <- group_indices(df)
  df <- ungroup(df)
  df$id_cjpair <- id_cjpair
  
  # Tag pairs that include a profile with restriction on leaving home
  # and (1 + drop) restrictions in total (where the one comes from the leaviong home restriction)
  df <- df %>% 
    mutate(tag = LeaveHome == 1 & cj_nstrict <= 1 + drop) %>% 
    group_by(id_cjpair) %>% 
    mutate(tag_cjpair = max(tag)) %>% 
    ungroup() %>% 
    filter(!tag_cjpair) %>% 
    select(-tag, -tag_cjpair)
 
  # Count rows
  N <- nrow(df)
  
  formula <- as.formula(formula)
  
  # Overwrite weights with 1s if weight == FALSE
  if (!weights) df$weight <- 1
  
  # Fit model
  if (length(byvar) == 0) {
    if (!cluster) {
      fit <- lm(formula = formula, data = df, weight = "weight")
    }
    if (cluster) {
      fit <- lm_robust(formula = formula, data = df, clusters = id, weight = weight)
    }
    if (tidy) fit <- broom::tidy(fit)
  } else {
    df <- df %>% group_by(!!sym(byvar))
    if (tidy) {
      if (!cluster) {
        fit <- df %>%
          group_modify(~ broom::tidy(lm(formula = formula, data = df, weight = "weight")))
      }
      if (cluster) {
        fit <- df %>%
          group_modify(~ broom::tidy(lm_robust(formula = formula, data = ., clusters = id, weight = weight)))
      }  
    } else {
      if (!cluster) {
        fit <- df %>% 
          group_map(~ lm(formula = formula, data = df, weight = "weight"))
      }
      if (cluster) {
        fit <- df %>% 
          group_map(~ lm_robust(formula = formula, data = ., clusters = id, weight = weight))
      }
    }
  }
  
  if (tidy) fit$N <- N
  
  return(fit) 
  
}

# Apply function - excluding profile pairs w/ at least one profile
# with LeaveHome = strict and only 0,1,2... add'l restrictions
results <- lapply(0:10, cj_lpm_drop, df = exp1, formula = "choice_num ~ LeaveHome + IntTravel + Leisure + Shops + Schools + HomeOffice + Gatherings + ContactTracing + Rallies + Elections + FactChecking", tidy = TRUE)

# Combine results

plotdat <- data.frame()

for (i in 1:11) {
  
  res <- as.data.frame(results[[i]])
  res$add_restrict <- i-1
  # Add row that contains case number
  res[13, "term"] <- "Obs"
  res[13, "estimate"] <- res[1, "N"]
  res[13, "add_restrict"] <- i-1
  plotdat <- bind_rows(plotdat, res)
  rm(res)  

}

plotdat <- filter(plotdat, term != "(Intercept)" & !is.na(estimate))

# Reorder and label
plotdat$restriction <- factor(plotdat$term,
           levels = c("LeaveHome",
                      "IntTravel",
                      "Leisure",
                      "Shops",
                      "Schools",
                      "HomeOffice",
                      "Gatherings",
                      "ContactTracing",
                      "Rallies",
                      "Elections",
                      "FactChecking",
                      "Obs"),
           labels = c("Leaving home",
                      "International travel",
                      "Leisure facilities",
                      "Shops and services",
                      "Schools",
                      "Home office",
                      "Private gatherings",
                      "Contact tracing",
                      "Rallies/demonstrations",
                      "Elections",
                      "Fact-checking",
                      "N (profiles)"))

# Free memory
gc()

# Plot
# [plotdat$restriction != "N (profiles)", ]
p <- 
  ggplot(data = plotdat, 
            aes(x = add_restrict, y = estimate, ymax = conf.high, ymin = conf.low, color = restriction)) +
  geom_hline(yintercept = 0, size = .75) +
  geom_linerange(size = .75) +
  geom_line(size = .5) +
  geom_point(size = 2) +
  scale_color_manual(values = viridis::plasma(n = 12, begin = 0, end = .8)) +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "\nProfile pairs excluded if at least one of the profiles includes\nrestrictions on leaving home and no more than X additional restrictions ",
       y= "AMCE with 95%-CI\n ") +
  theme_bw()  +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10),
    axis.text.x = element_text(color = "black", size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  facet_wrap(. ~ restriction, scales = "free_y")
  # facet_wrap(. ~ restriction)
p  

ggsave("./Output/RobCheck_LeaveHome.png", p, width=26, height = 16, units = "cm")

# We do not need to use ggplot_build here, coeff already contains the plotted data
figA10_data <- select(plotdat, restriction, add_restrict, term, estimate, conf.low, conf.high, N)
figA10_data <- arrange(figA10_data, restriction, add_restrict)
figA10_data <- as.matrix(figA10_data)
xlsx::write.xlsx(figA10_data, file = "./Output/TableA12_DataFigA10.xlsx", sheetName = "Sheet1",
           col.names = TRUE, row.names = FALSE, append = FALSE)


