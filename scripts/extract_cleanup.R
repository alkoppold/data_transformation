library(tidyverse)
library(gsheet)
data_extract = gsheet2tbl('https://docs.google.com/spreadsheets/d/1In5IKFNKbVj4WJCawDu2xfr_0CJ3off3Nr5chBaoYYU/edit?gid=1869532958')


# Renaming ----------------------------------------------------------------
data_extract %>% names()
data_extract = data_extract %>% 
  #manual renames to avoid error in subsequent rename_with
  rename(normality_how = `If yes, how? (e.g., specific test or visually), if no: NA`,
         homoscedasticity_how = `If yes, how?  (e.g., specific test or visually), if no: NA...27`,
         independence_how = `If yes, how?  (e.g., specific test or visually), if no: NA...31`) %>% 
  #rename_with(\(x) {x %>% str_extract("^\\S+(\\s+\\S+){0,3}")}) %>% #extract first 1-4 words
  rename_with(\(x) x %>% str_extract("^\\S+\\s+\\S+"), .cols = starts_with("open")) %>%
  rename_with(\(x) x %>% str_extract("^\\S+"), .cols = starts_with("n_")) %>% 
  rename(prereg = `preregistration yes/no`,
         cross_vs_long = `longitudinal design (LD), cross-sectional design (CD), unclear`,
         mental_health_exclusion = `mental health disorder exclusion (yes, no, unclear, not reported, partially)`,
         individual_level = `Was an individual level analysis conducted in the study? group/ individual level analysis, both`,
         individual_level_VOI = `If an individual level analysis was conducted in the study, what was the variable of interest? (e.g. sex/ gender, STAI, IUS, BDI, multiple)`,
         normality = `Was the normal distribution checked? answers: dependent variable, independent variable, residuals, mixed, not reported`,
         normality_when = `If yes, was the normal distribution tested before or after transformation procedure? answers: before, after, both, not reported. if no: NA`,
         homoscedasticity = `Was the homoscedasticity checked? answers: yes, not reported`,
         sphericity_old = `Was the sphericity checked? answers: yes, not reported`,
         sphericity = `How was sphericity handeled (test, correction)`,
         independence = `Was the independence of residuals checked? answers: yes, not reported`,
         linearity = `Was the linearity checked? answers: yes, not reported`,
         linearity_how = `If yes, how?  search for linearity, quadratic (predictor/ trends)`,
         multicollinearity = `Was the multicollinearity checked? answers: yes, not reported`,
         multicollinearity_how = `If yes, how?   (e.g., specific test or visually), if no: NA`,
         outlier = `outlier removal reported? yes, no (this refers to quantitative measures such as +/-3SD and not qualitative ones (artifacts, zero responses)`,
         outlier_when = `outlier refer to: subjects, trials, both, NA, not reported`,
         outlier_how = `outlier criterion (e.g., +/- 2SD; this refers to quantitative measures such as +/-2SD and not qualitative ones (artifacts, zero responses)`,
         dt_specs = `specification of data transformation method (e.g. range correction formula), not reported (if there are multiple different specifications for multiple outcome measures, extract them all)`,
         dt_rationale = `is there a rationale for specific data transformation procedure (e.g. guideline)? yes/ no`,
         dt_rationale_details = `which rationale? (e.g. guideline/ ref/ not reported)`,
         dt_rationale_ref = `reference rational`,
         dt_when = `was the transformation applied (AND EXPLICITELY STATED IN THE METHODS SECTION) on trial level or on an average-level (across trials)/ other/ or not reported (if this is done differently for various outcome measures, indicate it)`,
         design = `Stastistical model: within, between or mixed design? (e.g., paired ttest = within, independent ttest = between)`,
         design_within_levels_max = `If within or mixed: how many within factor levels (of largest within factor)? (e.g., two different CS stimuli = two within factors)`,
         statistical_test   = `Main statistical test: ttest, AN(C)OVA, correlation, regression, mixed model, other (ask Perplexity.ai with copy pasting info from the method section)`,
         statistical_test_details = `Main statistical test: further specification (e.g., non-parametric test), if ANCOVA: centered covariate?, if mixed model: paste formula here, of other: name of test (e.g. chi-squared, MANOVA)`
         ) %>% 
  rename_with(\(x) x %>% gsub(" ", "_", .)) %>% #replace space with "_"
  select(-sphericity_old) %>% 
  select(-n_with_exclusions) #check number of matches with n_before_exclusion for internal validation?
#TODO: deselect more variables that have not been extracted consistently or not been validated?

# Exclusions --------------------------------------------------------------
## Exclude some studies due to several reasons (listed where?)
doi_exclude_studies <- c(
  "10.1111/psyp.12456",
  "10.1016/j.neuroimage.2015.06.086",
  "10.1093/scan/nsaa074",
  "10.1017/S0033291712000359",
  "10.1038/mp.2011.66",
  "10.5665/sleep/32.1.19",
  "10.1080/10615806.2012.672976",
  "10.1093/scan/nsw181",
  "10.1093/scan/nsv122",
  "10.1016/j.brat.2018.09.003",
  "10.1111/psyp.13650",
  "10.1016/j.biopsych.2010.08.015",
  "10.1027/2151-2604/a000523",
  "10.1016/j.nlm.2014.03.008",
  "10.1101/lm.053902.123",
  "10.1080/02699931.2018.1500445",
  "10.1038/s41598-019-49751-4",
  "10.1093/scan/nsx148",
  "10.1016/j.neuroimage.2018.03.030",
  "10.1016/j.clinph.2019.04.010",
  "10.1037/xlm0000558",
  "10.1037/xge0000496",
  "10.1093/sleep/zsad209"
)

#data_extract <- data_extract[!is.element(data_extract$doi, doi_exclude_studies), ]
data_extract = data_extract %>% filter(doi %in% doi_exclude_studies == F)

#TODO deselect some columns that have not been coded consistently

# Longer Format -----------------------------------------------------------
#TODO put some columns into longer format if they contain several pieces of information (identifier = DOI)
#or maybe just do this later in order to not duplicate some other entries that are always unique?


# TODO check individual columns and clean them up -------------------------


# Statistical Test --------------------------------------------------------
data_extract %>% count(statistical_test) %>% arrange(desc(n)) %>% data.frame()
#TODO check "general linear model" (could be an emulation of ANOVA/regression or even a mixed model)
#TODO check "bayesian model" (what kind?)
#TODO check "generalized estimating equation model" & "path analysis". Are they are kind of SEM?
#TODO check "multivariate model for repeated measures"

#data_extract %>% select(statistical_test, `statistical test details`) %>% filter(statistical_test == "rmANOVA")
data_extract <- data_extract %>% mutate(
  statistical_test = case_when(statistical_test == "rmANOVA" ~ "ANOVA", #rmANOVA is specified in details
                                 
                                 statistical_test %>% grepl("growth", .) ~ "computational modeling", #"multilevel growth" = "computational" not "hierarchical"
                                 #statistical_test == "growth curve models with multilevel modelling" ~ "multilevel growth curve model",
                                 statistical_test == "computational model" ~ "computational modeling",
                                 
                                 statistical_test %>% grepl("hierarchical", .) ~ "multilevel model",
                                 statistical_test %>% grepl("multilevel", .) ~ "multilevel model",
                                 
                                 statistical_test %>% grepl("mixed", .) ~ "mixed model",
                                 
                                 T ~ statistical_test)
)

data_extract %>% count(statistical_test) %>% arrange(desc(n)) %>% data.frame()
#TODO could include MANOVA & ANCOVA into "ANOVA & extensions". But the same would make sense for correlation & regression. Ultimatively, many models are just a subclass of a linear model so maybe better leave it specific.

data_extract %>% select(statistical_test, statistical_test_details) %>% filter(statistical_test == "multiple")
#TODO could replace "multiple" with content in details and then put comma separation into long form (but harder to count percentages since they add > 1)

# Write to RDS ------------------------------------------------------------
data_extract %>% write_rds("data/data_extract.rds")
