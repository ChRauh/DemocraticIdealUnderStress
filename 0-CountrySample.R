####################################################
# Project:  Democratic ideal under stress
# Task:     Descriptives of sampled countries
#           (in wider universe)
# Authors:  @ChRauh (05.10.2023)
####################################################


# Packages
library(tidyverse)
library(ggrepel)
library(patchwork)
library(cowplot)
library(haven)


# Universe of 'minimally defined' democracies ####

# Along V-Dem V-Dem Liberal Democracy Index
# Data downloaded on October 2, 2023, from:
# https://v-dem.net/data/the-v-dem-dataset/country-year-v-dem-core-v13/

vdem <- read_rds("./Data/vdem-core/V-Dem-CY-Core-v13.rds") %>% 
  filter(year == 2021) %>% # Year of fieldwork
  select(c(country_name, country_text_id, v2x_libdem, v2x_polyarchy, v2x_regime, v2elfrfair_osp, v2elmulpar_osp)) %>% 
  rename(iso_code = country_text_id,
         libdem = v2x_libdem,
         elecdem = v2x_polyarchy,
         row = v2x_regime,
         electfreefair = v2elfrfair_osp,
         electmultiparty = v2elmulpar_osp) %>% 
  arrange(desc(libdem)) %>% 
  mutate(in_sample = iso_code %in% c("DEU", "ESP", "HUN", "JPN", "KOR", "POL")) # Mark our country sample

# Distributions of the standrd indices
hist(vdem$libdem)
hist(vdem$elecdem)

# By V-Dem / "Regimes of the World" standards, Hungary is an 'electoral autocracy'
# cf. https://www.cogitatiopress.com/politicsandgovernance/article/view/1214/1214 (esp. p. 63)
vdem[vdem$iso_code == "HUN", ]

# But it has - at least - free and fair, multiparty elections (v2elfrfair_osp > 2 & v2elmulpar_osp > 2)
# thus meeting the 'minimalist' conception of democracy by Przeworski
# cf. https://is.muni.cz/el/fss/podzim2019/POLn4002/um/Przeworski_Minimalist_Conception_of_Democracy.pdf

# If we take this minimalist cut-off, the universe of country cases is the following
universe <- vdem %>% 
  filter(electfreefair > 2 & electmultiparty > 2) # 118 countries
sum(universe$in_sample) # All six in there




# Quality of democracy indicators ####
# Country sample vs. universe of country cases


pl.elecdem <- 
  ggplot(data = universe, aes(y = elecdem, label = iso_code)) +
  geom_hline(yintercept = universe$elecdem[universe$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = universe %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = universe %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black", face = "bold")+
  labs(title = "\nElectoral democracy index",
       y = "")+
  coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic", hjust = .5))

pl.libdem <- 
  ggplot(data = universe, aes(y = libdem, label = iso_code)) +
  geom_hline(yintercept = universe$libdem[universe$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = universe %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = universe %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black", face = "bold")+
  labs(title = "\nLiberal democracy index",
       y = "")+
  coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic", hjust = .5))


pl.dem <- 
  pl.elecdem+pl.libdem +
  plot_annotation(title = "Quality of democracy (V-Dem 2021)",
                  theme = theme(plot.title = element_text(hjust = .5, size = 16, face = "bold")))
pl.dem

## ggsave("./Output/CountrySample/CountrySample_Democracy.png", pl.dem, width = 24, height = 30, units = "cm")  



# Dominant national culture ####

# We have pre-registered collectivism and flexibility vs monumentalism in this regard
# Pre-reg section 1.2.5.1 pp. 10-12
# and Pre-reg section 3.4 (country selection, p. 26)

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9461707/

# Data collected from World Value Survey by Heiko-Giebler
df <- read_dta("./Data/wvs/covid-19_wvs_2020_09_10.dta") %>% 
  rename(iso_code = alpha3code) %>% 
  filter(iso_code %in% universe$iso_code) %>% 
  mutate(in_sample = iso_code %in% c("DEU", "ESP", "HUN", "JPN", "KOR", "POL")) # Mark our country sample


# 70 countries with missing data on these dimensions !!!
sum(is.na(df$idvcoll18))
sum(is.na(df$flexmon18))

# Listwise deletion ...
df <- df %>% filter(!is.na(idvcoll18) & !is.na(flexmon18))



# Indvidualism/collectivism

pl.collect <- 
  ggplot(data = df, aes(y = idvcoll18, label = iso_code)) +
  geom_hline(yintercept = df$idvcoll18[df$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = df %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = df %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black")+
  labs(title = "\nIndividualism / collectivism",
       y = "")+
  # coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic", hjust = .5))

pl.flex <- 
  ggplot(data = df, aes(y = flexmon18, label = iso_code)) +
  geom_hline(yintercept = df$flexmon18[df$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = df %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = df %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black")+
  labs(title = "\nFlexibility / monumentalism",
       y = "")+
  # coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic", hjust = .5))

pl.culture <- 
  pl.collect+pl.flex +
  plot_annotation(title = "Dominant societal culture (WVS 2018)",
                  theme = theme(plot.title = element_text(hjust = .5, size = 16, face = "bold")))
pl.culture


# Combined plot ####
# Arranged with cowplot, patchwork doesn't like multiple patchworks ;)


pl.comb <-
  plot_grid(pl.dem, pl.culture, 
          align = "hv")

ggsave("./Output/CountrySample/CountrySample_Variation.png", pl.comb, width = 36, height = 20, units = "cm")




# Covid incidence / threat ####

# Downloaded on October 2 from Our World in Data (based on WHO information): 
# https://ourworldindata.org/covid-deaths#cumulative-confirmed-deaths-per-million-people

cd <- read_delim("./Data/covid/owid-covid-data_20231002.csv", delim = ",") %>% 
  filter(date == "2021-11-01") %>% # Briefly before our survey was fielded
  filter(!str_detect(iso_code, "OWID_")) %>% # Remove aggregator categories
  select(c("date", # Key indicators for Covid threat - the 'total' columns are cumulative
           "iso_code", 
           "total_cases_per_million", 
           "total_deaths_per_million", 
           "people_vaccinated_per_hundred")) %>% 
  filter(iso_code %in% universe$iso_code) %>% 
  mutate(in_sample = iso_code %in% c("DEU", "ESP", "HUN", "JPN", "KOR", "POL"))

# Get values for our six countries (for use in Study 1/2 analysis)
cd %>% 
  filter(iso_code %in% c("DEU", "ESP", "HUN", "JPN", "KOR", "POL")) %>% 
  select(iso_code, total_deaths_per_million)


# Plots 
pl.cases <- 
  ggplot(data = cd, aes(y = total_cases_per_million, label = iso_code)) +
  geom_hline(yintercept = cd$total_cases_per_million[cd$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = cd %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = cd %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black")+
  labs(title = "Cumulative Covid-19 cases per million people",
       y = "")+
  # coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic"))

pl.deaths <- 
  ggplot(data = cd, aes(y = total_deaths_per_million, label = iso_code)) +
  geom_hline(yintercept = cd$total_deaths_per_million[cd$in_sample], linetype = "dotted")+
  geom_boxplot(aes(x=1.3), width = .1)+
  geom_text(data = cd %>% filter(!in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = .8, color = "gray50")+
  geom_label(data = cd %>% filter(in_sample), aes(x = 1), position = position_jitter(width = .2), alpha = 1, color = "black")+
  labs(title = "Cumulative Covid-19 deaths per million people",
       y = "")+
  # coord_cartesian(ylim = c(0, 1))+ 
  # coord_flip()+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title =  element_text(face = "italic"))

pl.covid <- 
  pl.cases+pl.deaths +
  plot_annotation(title = "Distribution of country sample\nin universe of minimally defined democracies",
                  subtitle = "Covid-19 incidences (November 2021)\n",
                  caption = "Data source: https://ourworldindata.org/covid-deaths#cumulative-confirmed-deaths-per-million-people (October 2, 2023)",
                  theme = theme(plot.title = element_text(hjust = .5, size = 10, face = "plain"),
                                plot.subtitle = element_text(hjust = .5, size = 16, face = "bold")))

ggsave("./Output/CountrySample/CountrySample_Covid.png", pl.covid, width = 28, height = 14, units = "cm")  




# Stringency of Covid Interventions ####
# Data source: https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker (April 13, 2022)
string <- read.csv2("./Data/covid/covid-stringency-index.csv", sep = ",") %>% 
  rename(iso_code = Code) %>% 
  filter(iso_code %in% universe$iso_code)

# Mark our cases (and all other countries as a separate group)
string$selected <- string$Entity %in% c("Germany", "Hungary", "Japan", "Poland", "South Korea", "Spain")
string$group <- ifelse(string$selected, string$Entity, "Other democracies\n(average)")

# Clean up relevant variables
string$stringency_index <- as.numeric(string$stringency_index)
string$month <- str_remove(string$Day, "-[0-9]*?$")

# Aggregate to monthly means
string <- string %>% 
  group_by(month, group) %>% 
  summarise(stringency = mean(stringency_index))

# Plotting order
string$group2 <- string$group %>% 
  factor(levels = c("Germany", "Hungary", "Japan", "Poland", "South Korea", "Spain", "Other democracies\n(average)"))

# Temporal plot (full, as in first submission)
# ggplot(string , aes(x = month, y = stringency, color = group2, group = group2))+
#   geom_line()+
#   geom_vline(xintercept = "2021-11")+
#   scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#a6761d', '#66a61e','#e6ab02','#e7298a'))+
#   labs(title = "Governmental restrictions to contain COVID-19 in the country sample",
#        subtitle = "The stringency index records the strictness of ‘lockdown style’ policies that primarily restrict citizen’s behaviour.\nData from the Oxford COVID-19 Government Response Tracker. The vertical line marks our survey fieldwork.",
#        caption = " \nData source: https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker (April 13, 2022)",
#        y = "Governmental stringency index\n ",
#        x = " \nMonth",
#        color = "Country/group: ")+
#   theme_bw()+
#   theme(legend.position = "top",
#         legend.justification='left',
#         axis.text.x = element_text(angle = 90, vjust = .5))

# Temporal plot (for combination with averages)

pl.time <- 
  ggplot(string , aes(x = month, y = stringency, color = group2, group = group2, alpha = (month>="2021-11")))+
  geom_vline(xintercept = "2021-11", linewidth = 1.2)+
  geom_line(linewidth = 1.2)+
  scale_y_continuous(limits = c(0,90), breaks = seq(0,100,10))+
  # scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#a6761d', '#66a61e','#e6ab02','grey50'))+
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3', '#ff7f00','#e6ab02','grey50'))+
  scale_alpha_manual(values = c(1,0.3))+
  labs(title = "Monthly country averages",
       subtitle = "Vertical line marks survey fieldwork in November 2021",
       y = " ",
       x = " ",
       color = "Country/group: ")+
  theme_bw()+
  theme(legend.position = "none",
        legend.justification='left',
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))


# Cross-sectional plot (prior to field work)
pl.c <- 
  ggplot(string %>% filter(month < "2021-11"), aes(x = group2, y = stringency, color = group2, group = group2))+
  stat_summary(geom = "pointrange", fun.data = mean_cl_normal, size = 1.2, linewidth = 1.2)+
  labs(x= "", 
       title = "Country averages",
       subtitle = "January 2020 to November 2021",
       y = "Governmental stringency index\n ")+
  scale_y_continuous(limits = c(0,90), breaks = seq(0,100,10))+
  # scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#a6761d', '#66a61e','#e6ab02','grey50'))+
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3', '#ff7f00','#e6ab02','grey50'))+
  theme_bw()+
  theme(legend.position = "none",
        legend.justification='left',
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, face = "bold",
                                   color = c('#e41a1c','#377eb8','#4daf4a','#984ea3', '#ff7f00','#e6ab02','grey50'),
                                   size = 12),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"))


# Combined plot
pl.stringency <- 
  pl.c + pl.time +
  plot_layout(widths = c(1, 3))+
  plot_annotation(title =  "Governmental restrictions to contain COVID-19 in the country sample",
                  subtitle = "",
                  caption = "Data from the Oxford COVID-19 Government Response Tracker.\nThe stringency index records the strictness of ‘lockdown style’ policies that primarily restrict citizen’s behaviour.\nhttps://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker (April 13, 2022)",
                  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                plot.subtitle = element_text(size = 14, hjust=0.5))) 

ggsave("./Output/CountrySample/CountrySample_CovidStringency.png", pl.stringency, width = 32, height = 20, units = "cm")  
  





