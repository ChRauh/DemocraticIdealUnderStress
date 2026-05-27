####################################################
# Project:  Democratic ideal under stress
# Task:     Prepare background questionnaire data
# Authors:  @ChRauh/@jpheisig (15.11.2022)
####################################################

# This script scrutinizes, cleans and labels the raw data provided by IPSOS
# focusing on the questionnaire items (experimental data prepared in the respective scripts for study 1 & 2)


# Originally run on
# R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

# Packages ####
library(tidyverse)    # CRAN v1.3.1
library(tidylog)      # CRAN v1.0.2
library(psych)        # CRAN v2.2.5
library(haven)        # CRAN v2.5.2



# Raw data prepared by IPSOS ####
# Data and codebook permanently available at: https://doi.org/10.7802/2447 

df <-read_rds("./Data/PDTW2.rds")

# Check whether weights already re-scaled to sum to sample size, 
# so that they can be used for pooled analysis
df %>% group_by(country) %>% 
  summarise(mean_weight = mean(weight), mean_weight1 = mean(weight1))


# Prepare non-experimental data ####
# Background questionnaire (bg)

# Select variables
bg <- df %>% 
  select("id", "countryName", "gender", 
         "age", "eduISCED", "weight",
         starts_with("q28"), starts_with("q25"),
         starts_with("q12"), starts_with("q13"),
         starts_with("q33"), q30, q31, q35, 
         starts_with("q29"), starts_with("q26"),
         starts_with("q27"), 
         starts_with("exp2treatment"), starts_with("q23"))

# Recode 99 to NA (exclude id variable)
bg <- bg %>%  mutate(across(3:ncol(bg), ~ na_if(., 99)))

# Female dummy
bg$female <- bg$gender == 2

# Education -> 3 categories and as factor
bg$Education <- as.numeric(bg$eduISCED)
bg$Education <- recode(bg$Education, `1` = 1, `2` = 1, `3` = 2, `4` = 3)
bg$Education <- factor(bg$Education, 
                       labels = c("Low (ISCED 0-2)", "Med (ISCED 3-4)", "High (ISCED 5-8)"))

# Age
table(bg$age)
bg <- bg %>% mutate(agegr = cut(age, 
                                breaks = c(0, 29, 39, 49, 59, 1000), 
                                label = c("18-29", "30-39", "40-49", "50-59", "60-75")))
bg %>% group_by(agegr) %>% summarise(min = min(age), max = max(age))

# Children (and proxy living alone)
sapply(select(bg, starts_with("q33")), table, useNA = "always")
# As per codebook, we have to subtract one from all of these 
# vars to get actual number
bg <- bg %>%
  mutate(
    across(starts_with("q33"), ~ .x - 1) 
  ) %>% 
  rename(
    hh_nchld_0_5 = q33a,
    hh_nchld_6_11 = q33b,
    hh_nchld_12_17 = q33c, 
    hh_nadlts_18plus = q33d,
  ) %>% 
  mutate(
    hh_yngstchld_0_5 = hh_nchld_0_5 > 0,
    hh_yngstchld_0_11 = (hh_nchld_0_5 + hh_nchld_6_11) > 0,
    hh_snglpar = hh_nadlts_18plus == 1 & (hh_nchld_0_5 + hh_nchld_6_11 + hh_nchld_12_17) > 1,
    hh_snglpar_yngchild = hh_nadlts_18plus == 1 & hh_nchld_0_5 > 0,
    hh_livealone = hh_nadlts_18plus == 1 & (hh_nchld_0_5 + hh_nchld_6_11 + hh_nchld_12_17) == 0
  )

# Very few single parents...
sapply(select(bg, starts_with("hh")), table)

# Subjective standard of living
table(bg$q35, useNA = "always")
bg <- bg %>% rename(stdlivng = q35)

# Trust variables
bg <- bg %>% 
  rename(
    # Generalized trust
    gen_trust1 = q25a,
    gen_trust2 = q25b,
    # Trust
    trust_gov = q28a,
    trust_media = q28b,
    trust_parties = q28c,
    trust_parl = q28d,
    trust_UN = q28e,
    trust_experts = q28f,
    trust_WHO = q28g,
    trust_business = q28h,
    trust_EU = q28i
  )

# Self-rated health
table(bg$q30, useNA = "always")
bg <- bg %>% rename(srh = q30)

# Technocratic attitudes
techno_complete <- select(bg, starts_with("q29")) %>% drop_na
factanal(techno_complete, factors = 3, 
         rotation = "varimax", scores = "regression")
rm(techno_complete)

# First two and last four items form coherent subscales
# a and b capture elitism
# c and d capture expertise
# e and f may be intended to capture pro-democratic attitudes

bg <- bg %>% 
  mutate(
    techno_elitism = rowMeans(select(.,q29a, q29b)),
    techno_expertise = rowMeans(select(.,q29c,q29d)),
    techno_antipol = 8 - rowMeans(select(.,q29e,q29f)),
  ) 

cor(select(bg, starts_with("techno")), use = "complete.obs")


# Risk aversion and empathy
cor(select(bg, q25c, q25d), use = "complete.obs")
cor(select(bg, q25e, q25f), use = "complete.obs")
bg <- bg %>% 
  mutate(
    riskaversion = rowMeans(select(.,q25c,q25d)),
    empathy = rowMeans(select(.,q25e,q25f)),
  ) 



# Culture indicators

# Individualism-collectivism (q12a-q12g)
bg <- bg %>% 
  rename(
    # Generalized trust
    ind_religious = q12a,
    ind_fame = q12b,
    ind_punctual = q12c,
    ind_conflict = q12d,
    ind_boss = q12e,
    ind_laws = q12f,
    ind_favors = q12g
  )


# FLEX-Mon (q13a-q13g)
bg <- bg %>% 
  rename(
    flex_ordinary = q13a,
    flex_compete = q13b,
    flex_luck = q13c,
    flex_pretend = q13d,
    flex_diffhome = q13e,
    flex_situation = q13f,
    flex_help = q13g
  )


# Country as factor
bg$Country <- factor(bg$countryName, levels = c("Germany", "Hungary", "Japan", "Poland", "South Korea", "Spain"))



# Identify and mark duration outliers ####

# Pre-registered: "We will re-run our main tests on a sample excluding the fastest and the slowest 5% of the respondents"
# Given 1500 respondents per country, this suggests dropping any respondent faster/slower than that of the 75th respondent from head/tail in data ordered by duration

# Heiko noted that this 5% threshold should be calculated within country as response times vary systematically across languages 
# so that a 'global' threshold might create country-level biases

# Furthermore, I calculate this separately for the two experiments

# Select duration variables
durations <- df %>% 
  select(c("id", "countryName", starts_with("duration")))


# Get country-level thresholds

speedySurvey  <- durations %>% 
  select(c(countryName, durationSurvey)) %>% 
  arrange(countryName, durationSurvey) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 75) %>% 
  rename(speedySurvey = 2)
slowSurvey <- durations %>% 
  select(c(countryName, durationSurvey)) %>% 
  arrange(countryName, durationSurvey) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 1500-75) %>% 
  rename(slowSurvey = 2)

speedyExp1  <- durations %>% 
  select(c(countryName, durationExp1)) %>% 
  arrange(countryName, durationExp1) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 75) %>% 
  rename(speedyExp1 = 2)
slowExp1 <- durations %>% 
  select(c(countryName, durationExp1)) %>% 
  arrange(countryName, durationExp1) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 1500-75) %>% 
  rename(slowExp1 = 2)

speedyExp2  <- durations %>% 
  select(c(countryName, durationExp2)) %>% 
  arrange(countryName, durationExp2) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 75) %>% 
  rename(speedyExp2 = 2)
slowExp2 <- durations %>% 
  select(c(countryName, durationExp2)) %>% 
  arrange(countryName, durationExp2) %>% 
  group_by(countryName) %>% 
  filter(row_number() == 1500-75) %>% 
  rename(slowExp2 = 2)

# Combine
thresholds <- speedySurvey %>% 
  left_join(slowSurvey) %>% 
  left_join(speedyExp1) %>% 
  left_join(slowExp1) %>% 
  left_join(speedyExp2) %>% 
  left_join(slowExp2)
rm(speedySurvey, slowSurvey, speedyExp1, slowExp1, speedyExp2, slowExp2)

# Add this to the respondent-level duration data
durations <- durations %>% 
  left_join(thresholds, by = "countryName")

# Mark those respondents out of range
durations <- durations %>% 
  mutate(dropSurveyDuration = ifelse(durationSurvey <= speedySurvey | durationSurvey >= slowSurvey, T, F),
         dropExp1Duration = ifelse(durationExp1 <= speedyExp1 | durationExp1 >= slowExp1, T, F),
         dropExp2Duration = ifelse(durationExp2 <= speedyExp2 | durationExp2 >= slowExp2, T, F))

# For each of those, we should drop about 10% of the sample (900)
# Slight deviations possible if more respondents had the same duration as the 75th person
sum(durations$dropSurveyDuration)
sum(durations$dropExp1Duration)
sum(durations$dropExp2Duration)

# Add this information to the background data set
bg <- bg %>% 
  left_join(durations %>% select(id, starts_with("drop")))


# Add attentiveness indicator to back ground data ####
bg <- bg %>% 
  left_join(df %>% select(id, attentiveness))


# Export ####
write_rds(bg, "./Data/CleanedData.rds")


# Get an overview of missing data ####
# Pre-registered threshold: imputation if share of missings > 5% per country

missings <- bg %>%
  group_by(countryName) %>%
  summarise_all(function(x) round((sum(is.na(x))/length(x))*100, 2))

missings2 <- missings %>% 
  mutate(across(!countryName, function(x) ifelse(x > 5, "Problem", "OK")))


# Get overview of missing data used in subgroup analyses later

# Self-rated health
table(is.na(bg$srh), useNA = "ifany")
table(is.na(bg$srh), bg$countryName)

# Trust in government
table(is.na(bg$trust_gov), useNA = "ifany")
table(is.na(bg$trust_gov), bg$countryName)

# Satisfaction with national political system
table(is.na(bg$q27), useNA = "ifany")
table(is.na(bg$q27), bg$countryName)
