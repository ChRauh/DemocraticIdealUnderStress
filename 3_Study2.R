####################################################
# Project:  Democratic ideal under stress
# Task:     Generate results of study 2 (BWS)
# Authors:  @ChRauh/@jpheisig (19.01.2023)
####################################################

# This script re-produces the results for study 2 reported in the main text and the corresponding appendices


# Originally run on
# R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"


# Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions, CRAN v1.0.2
library(Hmisc) # Harrell Miscellaneous, CRAN v4.7-0
library(summarytools) # Tools to Quickly and Neatly Summarize Data, CRAN v1.0.1
library(estimatr) # Fast Estimators for Design-Based Inference, CRAN v0.30.6
library(margins) # Marginal Effects for Model Objects, CRAN v0.3.26
library(prediction) # Tidy, Type-Safe 'prediction()' Methods, CRAN v0.3.14
library(colorspace) # A Toolbox for Manipulating and Assessing Colors and Palettes, CRAN v2.0-3
library(viridis) # Colorblind-Friendly Color Maps for R, CRAN v0.6.2
library(RColorBrewer) # ColorBrewer Palettes, CRAN v1.1-3
library(patchwork) # The Composer of Plots, CRAN v1.1.1
library(support.BWS) # Tools for Case 1 Best-Worst Scaling, CRAN v0.4-4
library(xlsx) # Export
library(gridExtra) 
library(broom)

# Prepare BWS data ####

# Raw data prepared as provided IPSOS 
# Containing the data for the conjoint experiment (named "cjXXX", see below)
# Data and codebook permanently available at: https://doi.org/10.7802/2447 
df <-read_rds("./Data/PDTW2.rds")

# Select bws items
bdf <- df %>% 
  select(c(id, # respondent id
           matches("bwsChoice"))) # respondent choices in BWS tasks 

# Rename according to support.BWS package conventions
names(bdf) <- c("id",
                "b1", "w1",
                "b2", "w2",
                "b3", "w3",
                "b4", "w4",
                "b5", "w5",
                "b6", "w6",
                "b7", "w7")

# Drop incomplete cases (2%, 213 cases)
bdf <- bdf %>% drop_na

# Case numbers
nrow(bdf)
nrow(bdf)*7
nrow(bdf)*4

# Item names for support.BWS package
bws.items <- data.frame(item = seq(1,7,1),
                        label = c("Expert.Input",
                                  "Citizen.Input",
                                  "Party.Consensus",
                                  "Quick.Action",
                                  "Business.Input",
                                  "Internat.Coord",
                                  "Parl.Vote"))
item.names <- bws.items[,2] # As required by support.BWS

# In the shape that support.BWS requires along the th BIBD format
# See http://lab.agr.hokudai.ac.jp/nmvr/03-bws1.html
# Matrix with screens in rows and items per screen in cols
bws.des <- read.csv2("./Data/BWSdesign.csv")
des <- bws.des[,2:5]
names(des) <- 1:4
des <- as.matrix(des)


# Data in package-specific format
bws <- bws.dataset(bdf, # Respondent choices
                   response.type = 2, # Item number format
                   choice.sets = des, # The block design
                   design.type = 2, # BIBD design
                   item.names = item.names) # Short names of our items

# Count approach to measuring preferences
# Includes raw and standardized scores
cs <- bws.count(bws, cl = 2) 

# Reshape for plotting
bws.scores <- cs %>% 
  select(c(id, contains("sbw"))) %>% # Standardized scores only
  pivot_longer(!id, names_to = "procedure", values_to = "sbw")
bws.scores$procedure <- bws.scores$procedure %>% 
  str_remove(fixed("sbw.")) %>% 
  str_replace_all(fixed("."), " ")


# Add additional respondent-level data ####

# Load cleaned background data (produced by 0_PrepareBackgroundData.R)
bg <- read_rds("./Data/CleanedData.rds")

# Merge with BWS scores
bws.scores <- bws.scores %>% 
  left_join(bg[, c("id", "countryName", "exp2Treatment", "weight", "trust_gov", "q27", "trust_parties", "srh", "q31", "attentiveness", "dropExp2Duration")],
            by = "id")



# BWS scores - pooled sample ####

# Weighted mean with cluster-robust SEs, by framing treatment
bws_means <- bws.scores %>% 
  group_by(procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

bws_means <- bws_means %>% 
   mutate(
     lb95 = estimate - qt(.975, df) * std.error,
     ub95 = estimate + qt(.975, df) * std.error,
     lb99 = estimate - qt(.995, df) * std.error,
     ub99 = estimate + qt(.995, df) * std.error
  )


# Weighted mean with cluster-robust SEs, polling all treatments
bws_means_f <- bws.scores %>% 
  group_by(procedure) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

bws_means_f <- bws_means_f %>% 
  mutate(
    lb95 = estimate - qt(.975, df) * std.error,
    ub95 = estimate + qt(.975, df) * std.error,
    lb99 = estimate - qt(.995, df) * std.error,
    ub99 = estimate + qt(.995, df) * std.error
  )

bws_means_f$exp2Treatment <- 4
bws_means_f <- bws_means_f %>% select(names(bws_means)) # Harmonize column order


# Combine treatment-grouped and ungrouped estimates
bws_means <- rbind(bws_means, bws_means_f)

# Plotting order by average effect across treatments
order <- bws_means_f %>% 
  arrange(estimate)
bws_means$procedure <- factor(bws_means$procedure, levels = order$procedure)

# Treatment to factor
bws_means$exp2Treatment
bws_means$treatment <- factor(bws_means$exp2Treatment,
                              levels = c(1, 3, 2, 4),
                              labels = c("Major societal\nchallenges", "Climate\nchange",
                                         "Coronavirus", "Across all\ntreatments")) 

# Plot full sample scores

# Avoid abbreviations in procedure names
levels(bws_means$procedure)
levels(bws_means$procedure) = c("Parliamentary\nvote", "Party\nconsensus",
                            "International\ncoordination", "Quick\naction",
                            "Citizen\ninput", "Business\ninput", "Expert\ninput")


colors <- qualitative_hcl(6, palette = "Dark 3")

p <- 
  ggplot(bws_means, aes(x=estimate, y=procedure, color = treatment, fill = treatment, shape = treatment)) +
  geom_point(position = position_dodge(.7), size = 3.2) +
  geom_linerange(aes(xmin = lb99, xmax = ub99), position = position_dodge(.7), size = .8) +
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1.6) +
  coord_cartesian(xlim = c(-.5, .5)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(name = "Priming\ntreatment", values = c(colors[3],  colors[2], colors[1], "black"), drop = FALSE) +
  scale_fill_manual(name = "Priming\ntreatment", values = c(colors[3],  colors[2], colors[1],"black"), drop = FALSE) +
  scale_shape_manual(name = "Priming\ntreatment", values = c(25,24,22,21)) +
  # facet_wrap(.~treatment, ncol = 4)+
  labs(x = "Average standardized best-worst scores",
       y = "",
       color = "") +
  guides(
    color  = guide_legend(reverse = TRUE, ncol=1, title.position = "top", byrow = TRUE),
    fill  = guide_legend(reverse = TRUE, ncol=1,  title.position = "top", byrow = TRUE),
    shape  = guide_legend(reverse = TRUE, ncol=1, title.position = "top", byrow = TRUE)
  ) +
  theme_bw() +
  theme(legend.position = "right",
        # legend.box = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5,
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.spacing.y = unit(.25, "cm"),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(color = "black", face = "bold", size = 13),
        axis.text.y = element_text(color = "black", face = "bold", size = 12),
        axis.text.x = element_text(color = "black", size = 12)) +
 
  annotate("rect", ymin =.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)
p

ggsave("./Output/Fig3_BWSScores_PooledSample.png", p, width=26, height = 16, units = "cm")


# Table with exact values for appendix
fig3_data <- select(bws_means, procedure, exp2Treatment, treatment, estimate, lb95, ub95, lb99, ub99)
fig3_data <- arrange(fig3_data, desc(procedure), desc(treatment))
fig3_data <- fig3_data %>% ungroup() %>% select(-exp2Treatment)
#fig3_data$procedure <- as.character(fig3_data$procedure)
#fig3_data$treatment <- as.character(fig3_data$treatment)
# Export (as matrix, error otherwise)
write.xlsx(as.matrix(fig3_data), file = "./Output/TableA5_DataFig3.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)



# BWS scores by country ####

# Get country N tasks (bws.scores only contains valid obs at that point)
table(bws.scores$countryName)

# Get country N repsondets
bws.scores %>% group_by(countryName) %>% summarise(respondets = length(unique(id)))


# Weighted mean with cluster-robust SEs

bws_means <- bws.scores %>% 
  group_by(countryName, procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

bws_means <- bws_means %>% 
  mutate(
    lb95 = estimate - qt(.975, df) * std.error,
    ub95 = estimate + qt(.975, df) * std.error,
    lb99 = estimate - qt(.995, df) * std.error,
    ub99 = estimate + qt(.995, df) * std.error
  )



# Plotting order should follow that for pooled results
bws_means$procedure <- factor(bws_means$procedure, levels = order$procedure)


# Avoid abbreviations in procedure names
levels(bws_means$procedure)
levels(bws_means$procedure) = c("Parliamentary\nvote", "Party\nconsensus",
                                "International\ncoordination", "Quick\naction",
                                "Citizen\ninput", "Business\ninput", "Expert\ninput")

# Treatment to factor

# Following no longer works for me (JPH, 22.11.23)
# "Can't convert `x` <haven_labelled> to <character>."
# bws_means$treatment <- factor(bws_means$exp2Treatment,
#                              levels = c(3, 1, 2),
#                              labels = c("Climate change", "Major societal challenges", "Coronavirus")) 

bws_means$treatment <- haven::as_factor(bws_means$exp2Treatment,
                                        levels = "labels")
# Reorder
bws_means$treatment <- factor(bws_means$exp2Treatment,
                              levels = c(1, 3, 2),
                              labels = c("Major societal\nchallenges", "Climate\nchange","Coronavirus"))


# Plot by country 
colors <- qualitative_hcl(6, palette = "Dark 3")

p <- 
  ggplot(bws_means, aes(x=estimate, y=procedure, color = treatment, fill = treatment, shape = treatment)) +
  geom_point(position = position_dodge(.7), size = 2) +
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1) +
  facet_wrap(countryName ~., ncol = 3) +
  coord_cartesian(xlim = c(-.625, .625)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual(name = "Priming\ntreatment", values = c(colors[3],  colors[2], colors[1], "black"), drop = FALSE) +
  scale_fill_manual(name = "Priming\ntreatment", values = c(colors[3],  colors[2], colors[1],"black"), drop = FALSE) +
  scale_shape_manual(name = "Priming\ntreatment", values = c(25,24,22,21)) +
  scale_x_continuous(breaks = seq(-.6,.6,.3)) +
  # facet_wrap(.~treatment, ncol = 4)+
  labs(x = "Average standardized best-worst scores",
       y = "",
       color = "") +
  guides(
    color  = guide_legend(reverse = TRUE, ncol=1, title.position = "top", byrow = TRUE),
    fill  = guide_legend(reverse = TRUE, ncol=1,  title.position = "top", byrow = TRUE),
    shape  = guide_legend(reverse = TRUE, ncol=1, title.position = "top", byrow = TRUE)
  ) +
  theme_bw() +
  theme(legend.position = "right",
        # legend.box = "horizontal",
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5,
        legend.text = element_text(size = 12),
        legend.key.width = unit(1, "cm"),
        legend.spacing.y = unit(.25, "cm"),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 10),
        axis.title = element_text(color = "black", face = "bold", size = 13),
        axis.text.y = element_text(color = "black", face = "bold", size = 9),
        axis.text.x = element_text(color = "black", size = 8)) +
  annotate("rect", ymin =.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)
p

ggsave("./Output/FigA11_BWSScoresByCountry.png", p, width=26, height = 14, units = "cm")

# Table with exact values for appendix
figA11_data <- select(bws_means, countryName, procedure, exp2Treatment, treatment, estimate, lb95, ub95)
figA11_data <- arrange(figA11_data, countryName, desc(procedure), desc(treatment))
fiA11_data <- figA11_data %>% ungroup() %>% select(-exp2Treatment)
# Export (as matrix, error otherwise)
write.xlsx(as.matrix(figA11_data), file = "./Output/TableA13_DataFigA11.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


# Bonferroni-correctd p-values for country diffs in BWS scores
# We only do a graphical version (for Study 1 we also did tables)

# Weighted mean with cluster-robust SEs
# Aggregate across treatments (so unlike for previous plot, we do not group by it)

bws_means_pooled <- bws.scores %>% 
  group_by(countryName, procedure) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))


# Plotting order should follow that for pooled results
bws_means_pooled$procedure <- factor(bws_means_pooled$procedure, levels = order$procedure)


# Avoid abbreviations in procedure names
levels(bws_means_pooled$procedure)
levels(bws_means_pooled$procedure) = c("Parliamentary\nvote", "Party\nconsensus",
                                "International\ncoordination", "Quick\naction",
                                "Citizen\ninput", "Business\ninput", "Expert\ninput")

# We need the long version of the data for this
means_comp <- bws_means_pooled %>%
  full_join(bws_means_pooled, by = "procedure", suffix = c("", "2")) %>%
  mutate(
    z = (estimate - estimate2) / sqrt(std.error^2 + std.error2^2),
    p = 2 * pnorm(-abs(z)),
  ) %>%
  select(procedure, countryName, countryName2, p) %>%
  arrange(procedure, countryName, countryName2)

# Bonferroni correction
means_comp$p <- means_comp$p * 15
means_comp$p <- pmin(means_comp$p, 1)
means_comp$significant <- means_comp$p < .05

# ISO codes
means_comp <- means_comp %>% 
  mutate(
    iso = case_when(
      countryName == "Germany" ~ "DE",
      countryName == "Hungary" ~ "HU",
      countryName == "Japan" ~ "JP",
      countryName == "Poland" ~ "PL",
      countryName == "South Korea" ~ "KR",
      countryName == "Spain" ~ "ES"),
    iso2 = case_when(
      countryName2 == "Germany" ~ "DE",
      countryName2 == "Hungary" ~ "HU",
      countryName2 == "Japan" ~ "JP",
      countryName2 == "Poland" ~ "PL",
      countryName2 == "South Korea" ~ "KR",
      countryName2 == "Spain" ~ "ES")
  )

# Factorize iso vars in, reversing order for country2 to get table-like layout
means_comp$iso <- factor(means_comp$iso, levels = c("DE", "HU", "JP", "PL", "KR", "ES"))
means_comp$iso2 <- factor(means_comp$iso2, levels = c("ES", "KR", "PL", "JP", "HU", "DE"))

# Remove upper diagonal
# We set it to 1 rather than NA, because the latter results in grey cells 
means_comp <- means_comp %>% 
  mutate(
    significant = case_when(
      7-as.numeric(iso2) <= as.numeric(iso) ~ NA,
      TRUE ~ significant),
    p_char = case_when(
      7-as.numeric(iso2) <= as.numeric(iso) ~ "",
      TRUE ~ sprintf("%.3f", p))
  )

# Initialize list to store plots
plot_list <- list()

# Generate one plot per resttype
for (proc in unique(means_comp$procedure)) {
  
  plot_data <- filter(means_comp, procedure == proc)
  
  plot <- ggplot(plot_data, aes(x = iso, y = iso2, fill = significant, alpha = .5)) +
    geom_tile(color = "grey", size = .3) +
    scale_fill_manual(values = c("TRUE" = "#ffb09c", "FALSE" = "white"), na.value = "white") +
    geom_text(aes(label = p_char), color = "black", size = 2.5, vjust = 0.5, hjust = 0.5) +
    labs(title = proc) +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none",
          axis.ticks = element_blank(), 
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank())
  
  plot_list[[proc]] <- plot
}

combined_plot <- do.call(grid.arrange, c(plot_list, list(nrow = 3)))
ggsave("./Output/CountryComp_BWS_Scores.pdf", combined_plot, width = 9.69, height = 6.27, units = "in")
ggsave("./Output/CountryComp_BWS_Scores.png", combined_plot, width = 9.69, height = 6.27, units = "in")

# BWS scores against V-Dem (Quality of democracy)
# --> In this case we pool across priming treatments

plotdat <- bws_means_pooled %>% 
  filter(procedure %in% c("Parliamentary\nvote", "Party\nconsensus", "Citizen\ninput"))

plotdat$vdem <- NA
plotdat$vdem[plotdat$countryName == "Germany"] <-  0.833
plotdat$vdem[plotdat$countryName == "Spain"] <-  0.8
plotdat$vdem[plotdat$countryName == "South Korea"] <- 0.792
plotdat$vdem[plotdat$countryName == "Japan"] <-  0.731
plotdat$vdem[plotdat$countryName == "Poland"] <-  0.487
plotdat$vdem[plotdat$countryName == "Hungary"] <-  0.368

# Order by democracy score
plotdat$Country_by_vdem <- factor(plotdat$countryName)
plotdat$Country_by_vdem <- reorder(plotdat$Country_by_vdem, plotdat$vdem)

# Order democratic restrictions
plotdat$procedure_ordered = factor(plotdat$procedure, 
                              levels = c("Parliamentary\nvote", "Party\nconsensus", "Citizen\ninput"))



# Plot by country and type of restriction

p_base <- ggplot(plotdat, aes(x=estimate, y=Country_by_vdem, color = vdem)) + 
  facet_wrap(. ~ procedure_ordered) +
  labs(y = " \nCountry",
       x = "Average standardized BWS score",
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
        axis.text.x = element_text(angle = 90, vjust = .5))

p_dem <- p_base +
  geom_point(position = position_dodge(.7), size = 2)+
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(.7), size = 1)+
  scale_color_continuous_sequential(palette = "Blues", rev = TRUE, begin = .5, end = 1)

p_dem

ggsave("./Output/FigAXX_DemocraticProcedures_ByVDEM.png", p_dem, width=22, height = 16, units = "cm")

# Repeat for Covid deaths
# restricting to pandemic priming


plotdat <- bws_means %>% 
  filter(procedure %in% c("Parliamentary\nvote", "Party\nconsensus", "Citizen\ninput")) %>% 
  filter(treatment == "Coronavirus")


plotdat$covdeaths <- NA
plotdat$covdeaths[plotdat$countryName == "Germany"] <-  1191
plotdat$covdeaths[plotdat$countryName == "Spain"] <-  1870
plotdat$covdeaths[plotdat$countryName == "South Korea"] <- 55.5
plotdat$covdeaths[plotdat$countryName == "Japan"] <-  147
plotdat$covdeaths[plotdat$countryName == "Poland"] <-  1933
plotdat$covdeaths[plotdat$countryName == "Hungary"] <-  3098

# Order by covid deaths
plotdat$Country_by_covdeaths <- factor(plotdat$countryName)
plotdat$Country_by_covdeaths <- reorder(plotdat$Country_by_covdeaths, plotdat$covdeaths)


# Order democratic restrictions
plotdat$procedure_ordered = factor(plotdat$procedure, 
                                   levels = c("Parliamentary\nvote", "Party\nconsensus", "Citizen\ninput"))



# Plot by country and type of restriction

p_base <- ggplot(plotdat, aes(x=estimate, y=Country_by_covdeaths, color = covdeaths)) + 
  facet_wrap(. ~ procedure_ordered) +
  labs(y = " \nCountry",
       x = "Average standardized BWS score",
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
        axis.text.x = element_text(angle = 90, vjust = .5))

p_covdeaths <- p_base +
  geom_point(position = position_dodge(.7), size = 2)+
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(.7), size = 1)+
  scale_color_continuous_sequential(palette = "Blues", rev = TRUE, begin = .5, end = 1)

p_covdeaths

ggsave("./Output/FigAXX_DemocraticProcedures_ByCovDeaths.png", p_covdeaths, width=22, height = 16, units = "cm")


# BWS scores - full sample - robustness check for inattentive or quick/slow respondents ####

#  Full sample
nrow(bws.scores) # N Tasks
length(unique(bws.scores$id)) # N respondents
bws_means.full <- bws.scores %>% 
  group_by(procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

# Excluding the fastest/slowest 5%, respectively
nrow(bws.scores %>% filter(!dropExp2Duration)) # N Tasks
bws.scores %>% filter(!dropExp2Duration) %>% select(id) %>% unique() %>% nrow() # N respondents
bws_means.duration <- bws.scores %>% 
  filter(!dropExp2Duration) %>% 
  group_by(procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

# Excluding respondents having failed one of the two attentiveness checks
nrow(bws.scores %>% filter(attentiveness >= 1)) # N Tasks
bws.scores %>% filter(attentiveness >= 1) %>% select(id) %>% unique() %>% nrow() # N respondents
bws_means.att1 <- bws.scores %>% 
  filter(attentiveness >= 1) %>% 
  group_by(procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

# Excluding respondents having failed both attentiveness checks
nrow(bws.scores %>% filter(attentiveness == 2)) # N Tasks
bws.scores %>% filter(attentiveness == 2) %>% select(id) %>% unique() %>% nrow() # N respondents
bws_means.att2 <- bws.scores %>% 
  filter(attentiveness == 2) %>% 
  group_by(procedure, exp2Treatment) %>% 
  group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))

# Combine and prettify
bws_means <- rbind(bws_means.full %>% mutate(group = "Full sample"),
                   bws_means.duration %>% mutate(group = "Excluding the fastest/slowest 5% of respondents"),
                   bws_means.att1 %>% mutate(group = "Excluding respondents having failed one of two attentiveness checks"),
                   bws_means.att2 %>% mutate(group = "Excluding respondents having failed both attentiveness checks"))

bws_means <- bws_means %>% 
  mutate(
    lb95 = estimate - qt(.975, df) * std.error,
    ub95 = estimate + qt(.975, df) * std.error,
    lb99 = estimate - qt(.995, df) * std.error,
    ub99 = estimate + qt(.995, df) * std.error
  )

bws_means$procedure <- factor(bws_means$procedure, levels = order$procedure) # Order from above (mean procedure)

# Reorder
bws_means$treatment <- factor(bws_means$exp2Treatment,
                              levels = c(1, 3, 2),
                              labels = c("Major societal\nchallenges", "Climate\nchange", "Coronavirus"))

# Plot
p <- 
  ggplot(bws_means, aes(x=estimate, y=procedure, color = group)) +
  geom_point(position = position_dodge(.7), size = 2) +
  geom_linerange(aes(xmin = lb99, xmax = ub99), position = position_dodge(.7), size = .5) +
  geom_linerange(aes(xmin = lb95, xmax = ub95), position = position_dodge(.7), size = 1) +
  coord_cartesian(xlim = c(-.6, .6)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  # scale_color_manual(values = c(colors[3], colors[2], colors[1]), drop = FALSE) +
  guides(color = guide_legend(reverse = TRUE, nrow = 2))+
  annotate("rect", ymin =.5, ymax = 1.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =2.5, ymax = 3.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =4.5, ymax = 5.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  annotate("rect", ymin =6.5, ymax = 7.5, xmin = -Inf, xmax = Inf, fill ="gray50", alpha = 0.15, color = NA)+
  facet_wrap(.~ fct_rev(treatment))+
  labs(x = " \nAverage standardized best-worst scores",
       y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "vertical",
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        axis.title = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 10))


p

ggsave("./Output/FigA14_BWSScores_FullSample_AttentivenessSpeeding.png", p, width=30, height = 15, units = "cm")

# Table with exact values for appendix
figA14_data <- select(bws_means, treatment, procedure, group, estimate, lb95, ub95, lb99, ub99)
figA14_data <- arrange(figA14_data, desc(treatment), desc(procedure), desc(group))

write.xlsx(as.matrix(figA14_data), file = "./Output/TableA16_DataFigA14.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)






# Subgroup analyses ####

# Government trust and system satisfaction 

# Factorize political views
bws.scores$trust_gov_cut <- ifelse(bws.scores$trust_gov > 5, "high",
                             ifelse(bws.scores$trust_gov > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(bws.scores$trust_gov_cut)
plot(bws.scores$trust_gov, bws.scores$trust_gov_cut)

bws.scores %>% filter(!is.na(trust_gov_cut)) %>%  nrow() # N tasks
bws.scores %>% filter(!is.na(trust_gov_cut)) %>% select(id) %>% unique() %>%  nrow() # N respondents

bws.scores$pol_sat_cut <- ifelse(bws.scores$q27 > 5, "high",
                           ifelse(bws.scores$q27 > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(bws.scores$pol_sat_cut, useNA = "ifany")
plot(bws.scores$q27, bws.scores$pol_sat_cut)

bws.scores %>% filter(!is.na(pol_sat_cut)) %>%  nrow() # N tasks
bws.scores %>% filter(!is.na(pol_sat_cut)) %>% select(id) %>% unique() %>%  nrow() # N respondents


# Weighted mean with cluster-robust SEs - trust government

# TO DO: the group_modify approach doesn't work on my end, somewhere it tries to colSum an unidimensional vector !?
# Working with unweighted, unclustered data for now !!!
bws_means_trust_gov <- 
  bws.scores %>% 
  group_by(procedure, trust_gov_cut) %>% 
  # group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # group_map(tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # summarise(list = tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  summarise(estimate = mean(sbw),
            lb95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[2]),
            ub95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[3]),
            lb99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[2]),
            ub99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[3])) %>% 
  filter(!is.na(trust_gov_cut)) %>% 
  filter(procedure %in% c("Expert Input", "Citizen Input")) %>% 
  rename(ivlevel = 2) %>% 
  mutate(type = "Trust\nin national government")


# TO DO: the group_modify approach doesn't work on my end, somewhere it tries to colSum an unidimensional vector !?
# Working with unweighted, unclustered data for now !!!
bws_means_pol_sat <- 
  bws.scores %>% 
  group_by(procedure, pol_sat_cut) %>% 
  # group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # group_map(tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # summarise(list = tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  summarise(estimate = mean(sbw),
            lb95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[2]),
            ub95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[3]),
            lb99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[2]),
            ub99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[3])) %>% 
  filter(!is.na(pol_sat_cut)) %>% 
  filter(procedure %in% c("Expert Input", "Citizen Input")) %>% 
  rename(ivlevel = 2) %>% 
  mutate(type = "Satisfaction\nwith national political system")

bws_means <- rbind(bws_means_trust_gov,
                   bws_means_pol_sat)

ggplot(bws_means, aes(x = ivlevel, y = estimate, ymax = ub95, ymin = lb95, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_grid(procedure ~ fct_rev(type)) +
  labs(x = "",
       y = "Average standardized best-worst scores\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigA15_BWS_ByPoliticalViews_noLines.png", width=22, height = 14, units = "cm")

# Table with exact values for appendix
figA15_data <- select(bws_means, type, ivlevel, estimate, lb95, ub95)
figA15_data <- arrange(figA15_data, procedure, desc(type), ivlevel)
write.xlsx(as.matrix(figA15_data), file = "./Output/TableA17_DataFigA15.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


# Trust in parties 

bws.scores$trust_parties_cut <- ifelse(bws.scores$trust_parties > 5, "high",
                                   ifelse(bws.scores$trust_parties > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(bws.scores$trust_parties_cut)
plot(bws.scores$trust_parties, bws.scores$trust_parties_cut)

bws_means_trust_parties <- 
  bws.scores %>% 
  group_by(procedure, trust_parties_cut) %>% 
  # group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # group_map(tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # summarise(list = tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  summarise(estimate = mean(sbw),
            lb95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[2]),
            ub95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[3]),
            lb99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[2]),
            ub99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[3])) %>% 
  filter(!is.na(trust_parties_cut)) %>% 
  filter(procedure %in% c("Expert Input", "Citizen Input", "Parl Vote", "Party Consensus")) %>% 
  rename(ivlevel = 2)

ggplot(bws_means_trust_parties, aes(x = ivlevel, y = estimate, ymax = ub95, ymin = lb95, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(. ~procedure, nrow = 2) +
  labs(title = "Selected BWS scores by respondents' trust in parties",
       x = "",
       y = "Average standardized best-worst scores\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigXX_BWS_ByTrustInParties_noLines.png", width=22, height = 22, units = "cm")



# Self-rated health

bws.scores$srh_cut <- ifelse(bws.scores$srh > 5, "high",
                                       ifelse(bws.scores$srh > 2, "mid", "low")) %>% 
  factor(levels = c("low", "mid", "high"))
table(bws.scores$srh_cut)
plot(bws.scores$srh, bws.scores$srh_cut)

bws.scores %>% filter(!is.na(srh_cut)) %>%  nrow() # N tasks
bws.scores %>% filter(!is.na(srh_cut)) %>% select(id) %>% unique() %>%  nrow() # N respondents

bws_means_srh <- 
  bws.scores %>% 
  group_by(procedure, srh_cut) %>% 
  # group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # group_map(tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # summarise(list = tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  summarise(estimate = mean(sbw),
            lb95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[2]),
            ub95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[3]),
            lb99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[2]),
            ub99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[3])) %>% 
  filter(!is.na(srh_cut)) %>% 
  filter(procedure %in% c("Expert Input", "Citizen Input")) %>% 
  rename(ivlevel = 2)

ggplot(bws_means_srh, aes(x = ivlevel, y = estimate, ymax = ub95, ymin = lb95, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(. ~procedure, nrow = 2) +
  labs(title = "Selected BWS scores by respondents' self-rated health",
       x = "",
       y = "Average standardized best-worst scores\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigA16_BWS_BySelfRatedHealth_noLines.png", width=22, height = 14, units = "cm")



# Table with exact values for appendix
figA16_data <- select(bws_means_srh, procedure, ivlevel, estimate, lb95, ub95)
write.xlsx(as.matrix(figA16_data), file = "./Output/TableA18_DataFigA16.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)




# Corona infections

table(bws.scores$q31)
bws.scores$cinfect <- ifelse(bws.scores$q31 == 1, "Yes",
                       "No") %>% 
  factor(levels = c("Yes", "No"))

bws_means_cinfect <- 
  bws.scores %>% 
  group_by(procedure, cinfect) %>% 
  # group_modify(~ broom::tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # group_map(tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  # summarise(list = tidy(lm_robust(formula = sbw ~ 1, data = ., clusters = id, weight = weight)))
  summarise(estimate = mean(sbw),
            lb95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[2]),
            ub95 = as.numeric(mean_cl_normal(sbw, conf.int = .95)[3]),
            lb99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[2]),
            ub99 = as.numeric(mean_cl_normal(sbw, conf.int = .99)[3])) %>% 
  filter(!is.na(cinfect)) %>% 
  filter(procedure %in% c("Expert Input", "Citizen Input")) %>% 
  rename(ivlevel = 2)

ggplot(bws_means_cinfect, aes(x = ivlevel, y = estimate, ymax = ub95, ymin = lb95, group = 1))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange()+
  # geom_line()+ # Heiko doesn't want lines here ;)
  facet_wrap(. ~procedure, nrow = 2) +
  labs(title = "Selected BWS scores by respondents' corona infection",
       x = "",
       y = "Average standardized best-worst scores\n ") +
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigXX_BWS_ByCoronaExperience_noLines.png", width=22, height = 14, units = "cm")



# Anchoring question ####


# Number of respondents
resp <- bg %>% 
  select(c(id, countryName, exp2Treatment, starts_with("q23")))
na_count <-sapply(resp, function(y) sum(length(which(is.na(y)))))
na_count # Zero missings, so N is county sample size
table(resp$exp2Treatment)


# Select anchoring items
anch <- bg %>% 
  select(c(countryName, exp2Treatment, starts_with("q23"))) %>% 
  pivot_longer(cols = 3:11)

# Label treatment
anch$treatment <- factor(anch$exp2Treatment,
                              levels = c(3, 1, 2),
                              labels = c("Climate change", "Major societal challenges", "Coronavirus")) 

# Label items correctly (see codebook)
anch$name <- anch$name %>% 
  str_replace("q23a", "Expert Input") %>% 
  str_replace("q23b", "Citizen Input") %>% 
  str_replace("q23c", "Party Consensus") %>% 
  str_replace("q23dk", "Don't know")  %>% 
  str_replace("q23d", "Quick Action") %>%
  str_replace("q23e", "Business Input") %>% 
  str_replace("q23f", "Internat Coord") %>% 
  str_replace("q23g", "Parl Vote") %>%
  str_replace("q23h", "None important")
  

table(anch$name, useNA = "ifany")


# Full sample choice frequencies
# Ignoring the framing experiment for now (equally distributed w/in country)

anch.full <- anch %>% 
  group_by(name) %>% 
  summarise(share = mean(value)) %>% 
  mutate(share = share*100) %>% 
  arrange(desc(share))

# Full sample, with treatment

anch.full.t <- anch %>% 
  group_by(name, treatment) %>% 
  summarise(share = mean(value)) %>% 
  mutate(share = share*100) %>% 
  arrange(desc(share))

# Order procedures as in main analysis, but adding don't know/none important
order_anch <- c("None important",  "Don't know", order$procedure)
anch.full.t$name <- factor(anch.full.t$name, levels = order_anch)


ggplot(anch.full.t, aes(y = name, x = share, fill = treatment))+
  geom_col(width = .7, position = position_dodge2(padding = .3))+
  scale_fill_manual(values = c(colors[3], colors[2], colors[1]), drop = FALSE) +
  labs(title = "Decision-making procedures considered 'very important'",
    x = "Percentage of respondents selecting the option\n(multiple choices possible)",
    y = "",
    fill = "Societal challenge treatment: ")+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigA12_BWS_Anchor_FullSample.png", width=22, height = 14, units = "cm")

# Table with exact values for appendix
figA12_data <- pivot_wider(anch.full.t, names_from = treatment, values_from = share)
figA12_data <- arrange(figA12_data, desc(name))
write.xlsx(as.matrix(figA12_data), file = "./Output/TableA14_DataFigA12.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


# By country

anch.country <- anch %>% 
  group_by(countryName, name, treatment) %>% 
  summarise(share = mean(value)) %>% 
  mutate(share = share*100) 

anch.country$name <- factor(anch.country$name, levels = anch.full$name)

# Reorder treatment
anch.country$treatment <- factor(anch.country$treatment,
                              levels = c("Major societal challenges", "Climate change","Coronavirus"))

# Order procedures
anch.country$name <- factor(anch.country$name, levels = order_anch)

ggplot(anch.country, aes(y = name, x = share, fill = treatment))+
  geom_col(width = .7, position = position_dodge2(padding = .3))+
  scale_fill_manual(values = c(colors[3], colors[2], colors[1]), drop = FALSE) +
  labs(title = "Decision-making procedures considered 'very important'",
       x = "Percentage of respondents selecting the option\n(multiple choices possible)",
       y = "",
       fill = "Societal challenge treatment: ")+
  facet_wrap(.~ countryName)+
  theme_bw()+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 12)) 

ggsave("./Output/FigA13_BWS_Anchor_ByCountry.png", width=22, height = 14, units = "cm")

# Table with exact values for appendix
figA13_data <- pivot_wider(anch.country, names_from = countryName, values_from = share)
figA13_data <- arrange(figA13_data, desc(name), desc(treatment))
write.xlsx(as.matrix(figA13_data), file = "./Output/TableA15_DataFigA13.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)



