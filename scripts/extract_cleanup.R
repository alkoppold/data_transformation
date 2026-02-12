library(tidyverse)
library(gsheet)

data_extract.original = gsheet2tbl('https://docs.google.com/spreadsheets/d/1In5IKFNKbVj4WJCawDu2xfr_0CJ3off3Nr5chBaoYYU/edit?gid=1869532958')

# Renaming ----------------------------------------------------------------
#data_extract.original %>% names()
data_extract.original = data_extract.original %>% 
  #manual renames to avoid error in subsequent rename_with
  rename(normality_how = `If yes, how? (e.g., specific test or visually), if no: NA`,
         homoscedasticity_how = `If yes, how?  (e.g., specific test or visually), if no: NA...27`,
         independence_how = `If yes, how?  (e.g., specific test or visually), if no: NA...31`) %>% 
  #rename_with(\(x) {x %>% str_extract("^\\S+(\\s+\\S+){0,3}")}) %>% #extract first 1-4 words
  rename_with(\(x) x %>% str_extract("^\\S+\\s+\\S+"), .cols = starts_with("open")) %>% #extract first 2 words for columns starting with "open"
  rename_with(\(x) x %>% str_extract("^\\S+"), .cols = starts_with("n_")) %>% #extract first word for columns starting with "n_"
  rename(prereg = `preregistration yes/no`,
         cross_vs_long = `longitudinal design (LD), cross-sectional design (CD), unclear`,
         mental_health_exclusion = `mental health disorder exclusion (yes, no, unclear, not reported, partially)`,
         individual_level = `Was an individual level analysis conducted in the study? group/ individual level analysis, both`,
         individual_level_VOI = `If an individual level analysis was conducted in the study, what was the variable of interest? (e.g. sex/ gender, STAI, IUS, BDI, multiple)`,
         normality = `Was the normal distribution checked? answers: dependent variable, independent variable, residuals, mixed, not reported`,
         normality_when = `If yes, was the normal distribution tested before or after transformation procedure? answers: before, after, both, not reported. if no: NA`,
         homoscedasticity = `Was the homoscedasticity checked? answers: yes, not reported`,
         sphericity_old = `Was the sphericity checked? answers: yes, not reported`, #has been replaced by subsequent column (inclusion of sphericity corrections, not only tests)
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
  rename_with(\(x) x %>% gsub("\\s*\\([^)]*\\)", "", .) %>% gsub(" ", "_", .)) #get rid of info in parentheses & replace space with "_"

tibble(new = data_extract.original %>% names(), old = data_extract.original %>% names()) %>% print(n = nrow(.))


# Select Variables --------------------------------------------------------
data_extract = data_extract.original %>% 
  select(Extractor:prereg, 
         n_before_exclusion:mental_health_exclusion, 
         normality:homoscedasticity_how,
         sphericity:dt_rationale_ref,
         design_within_levels_max:comment)



# Longer Format: Data Transformations -------------------------------------
N_studies = data_extract %>% pull(doi) %>% unique() %>% length()

#data_extract %>% filter(doi %>% is.na()) %>% select(title) #manually replaced NAs
data_extract %>% count(doi) %>% filter(n != 1)
#TODO remove duplicated ROIs! Googlesheet strictly in wide format

data_extract.dt = data_extract %>% 
  pivot_longer(HR:PUPIL_SIZE, names_to = "DV", values_to = "transformation") %>% 
  filter(transformation %>% is.na() == F) %>% 
  mutate(DV = DV %>% gsub("EMG_", "", .)) %>% #just "startle" instead of "EMG_startle"
  mutate(DV = DV %>% gsub("PUPIL_SIZE", "pupil", .)) %>% 
  mutate(DV = DV %>% gsub("EYE_tracking", "eye", .)) %>% 
  filter(DV != "orbicularis_oculi") %>% #temporary fix
  relocate(DV)


# Check & Clean Columns of Interest ---------------------------------------
checkContent = function(df, col, print=T) {
  result = df %>% count(!!rlang::ensym(col)) %>% arrange(desc(n))
  if (print) {
    result %>% print(n = nrow(.))
    return(invisible(result))
  }
  return(result)
}

# * Data Transformations --------------------------------------------------
data_extract.dt %>% checkContent(DV, print=F) %>% mutate(p = n / N_studies)
#data_extract %>% filter(EMG_orbicularis_oculi %>% is.na() == F) %>% select(Extractor, doi:title, starts_with("EMG_"))
#data_extract %>% filter(EMG_orbicularis_oculi %>% is.na() == F, EMG_orbicularis_oculi != EMG_startle) %>% select(Extractor, doi:title, starts_with("EMG_"))
#manual check: EMG_orbicularis_oculi has never been used outside of fear potentiated startle => exclude

data_extract.dt %>% checkContent(transformation)
#TODO even longer format with separate_longer_delim ? but sequence is important!



# * Sample Sizes ----------------------------------------------------------
# * * n_before_exclusion --------------------------------------------------
data_extract %>% #start with data_extract to avoid duplicates from data transformations
  mutate(n_before_exclusion = n_before_exclusion %>% 
           #gsub("Exp\\.?\\w?:?\\w?", "ExpX:", .) %>%  #different experiments shall just be added up => recoded manually
           gsub("\\d+", "N", .) #generify number for check
  ) %>% 
  checkContent(n_before_exclusion)

# * * n_after_exclusion ---------------------------------------------------
data_extract.dt %>% #start with data_extract to avoid duplicates from data transformations
  mutate(n_after_exclusion = n_after_exclusion %>% 
           #gsub("Exp\\.?\\w?:?\\w?", "ExpX:", .) %>% 
           gsub("\\d+", "N", .) #generify number for check
  ) %>% 
  checkContent(n_after_exclusion)
#TODO check "partially not reported" (only valid if "partial" refers to experiments; for partial reporting of DVs: report individually)
#TODO check "cued fear" vs. "context fear": manual recode to cue conditioning (delete context conditioning)?


# * * Longer Format: Sample Sizes -----------------------------------------
data_extract.N = data_extract.dt %>% #start with data_extract.dt to retain DV row (if n_* has one entry but there are several DVs, N counts for all DVs and should be duplicated for explicitness)
  mutate(across(starts_with("n_"), \(x) x %>% gsub(",", ";", .) %>% na_if("not reported"))) %>% 
  mutate(n_after_exclusion = case_when(n_after_exclusion %>% str_detect("not reported") ~ NA, #temporary fix for "partially not reported"
                                       n_after_exclusion %>% str_detect("cued fear") ~ NA, #temporary fix 
                                       T ~ n_after_exclusion)) %>% 
  separate_longer_delim(starts_with("n_"), ";") %>% 
  #filter(n_before_exclusion %>% grepl("^\\d+$", .) == F) %>% 
  #filter(if_any(starts_with("n_"), \(x) x %>% grepl("^\\d+$", .) == F)) %>% #only entries that are not completely made up of digits
  mutate(DV2 = n_after_exclusion %>% str_extract("\\b[a-zA-Z]+\\b")) %>% relocate(starts_with("DV")) #extract measurement (dependent variable, DV) from n_after_exclusion (if n_before_exclusion has several measurements, so does n_after_exclusion)

dv.descriptors = data_extract.dt %>% pull(DV) %>% unique() %>% sort()
#data_extract.N %>% pull(DV2) %>% unique() %>% sort() %>% setdiff(dv.descriptors) #check invalid descriptors

data_extract.N = data_extract.N %>% 
  filter(DV == DV2 | DV2 %>% is.na() | DV2 %in% dv.descriptors == F) %>% 
  mutate(across(starts_with("n_"), \(x) x %>% gsub("\\D", "", .)), #delete everything that is not a digit
         across(starts_with("n_"), as.integer),
         retention = n_after_exclusion / n_before_exclusion, exclusion = 1 - retention) %>% 
  relocate(starts_with("DV"), exclusion, retention, starts_with("n_"))

data_extract.dt %>% anti_join(data_extract.N %>% select(DV, doi)) #detect entries with missing (sub-)sample size
#data_extract.N %>% filter(doi %in% {data_extract.N %>% count(doi) %>% filter(n > 1) %>% pull(doi)}) %>% View("multiple Entries")
#data_extract.N %>% filter(DV2 %>% is.na() == F) %>% View("changed entries")
data_extract.N %>% arrange(retention) #TODO check lowest entries for plausibility


# * * Write Tidy Sample Sizes into Data Transformations -------------------
if (nrow(data_extract.dt) != nrow(data_extract.N)) { warning("Rows in data_extract.dt and data_extract.N don't match. Check difference with anti_join.")
} else data_extract.dt = data_extract.N %>% select(-DV2)

# * mental_health_exclusion -----------------------------------------------
data_extract %>% checkContent(mental_health_exclusion)

# * Statistical Test ------------------------------------------------------
data_extract %>% checkContent(statistical_test)
#TODO check "general linear model" (could be an emulation of ANOVA/regression or even a mixed model)
#TODO collapse regression & correlation? (but ANOVA & ttest are also separate)
#TODO check "bayesian model" (what kind?)

#data_extract %>% filter(statistical_test %>% is.na()) %>% pull(doi)
data_extract %>% filter(statistical_test == "multiple") %>% select(doi, statistical_test_details)

data_extract.tests = data_extract %>% 
  relocate(statistical_test) %>% 
  #filter(statistical_test == "multiple") %>% #for testing
  mutate(statistical_test = if_else(statistical_test == "multiple", statistical_test_details, statistical_test)) %>% 
  separate_longer_delim(statistical_test, ", ") %>% 
  
  mutate(
    statistical_test = case_when(statistical_test == "rmANOVA" ~ "ANOVA", #should only be specified in details
                                 statistical_test == "ANCOVA" ~ "ANOVA", #should only be specified in details
                                 statistical_test %>% str_detect("multivariate") ~ "ANOVA", #should only be specified in details
                                 statistical_test %>% str_detect("planned contrasts") ~ "ANOVA", #we subsume planned contrasts into the omnibus model used for the contrasts
                                 
                                 statistical_test %>% str_detect("ttest") ~ "ttest",
                                 statistical_test %>% str_detect("Welch") ~ "ttest", #(for unequal variances)
                                 
                                 statistical_test %>% str_detect("Whitney") ~ "ordinal ttest", #for independent samples
                                 statistical_test %>% str_detect("Wilcoxon") ~ "ordinal ttest", #for paired samples
                                 
                                 statistical_test %>% str_detect("regression") ~ "regression",
                                 
                                 statistical_test %>% str_detect("equation") ~ "Structural Equation Modeling",
                                 statistical_test %>% str_detect("path") ~ "Structural Equation Modeling", 
                                 
                                 statistical_test %>% grepl("growth", .) ~ "computational modeling", #"multilevel growth" = "computational" not "hierarchical"
                                 #statistical_test == "growth curve models with multilevel modelling" ~ "multilevel growth curve model",
                                 statistical_test == "computational model" ~ "computational modeling",
                                 
                                 statistical_test %>% grepl("hierarchical", .) ~ "multilevel model",
                                 statistical_test %>% grepl("multilevel", .) ~ "multilevel model",
                                 
                                 statistical_test %>% grepl("mixed", .) ~ "mixed model",
                                 
                                 T ~ statistical_test),
    statistical_test = if_else(statistical_test == "multilevel model", "mixed model", statistical_test) #collapse multilevel & mixed model
)

#data_extract.tests %>% filter(doi %in% {data_extract %>% filter(statistical_test == "multiple") %>% pull(doi)}) %>% checkContent(statistical_test)
#data_extract.tests %>% select(statistical_test, statistical_test_details) %>% filter(statistical_test == "rmANOVA")

data_extract.tests %>% checkContent(statistical_test, print=F) %>% mutate(p = n / N_studies)


# Write to RDS ------------------------------------------------------------
data_extract %>% write_rds("data/data_extract.rds")
data_extract.dt %>% write_rds("data/data_extract.dt.rds")
data_extract.tests %>% write_rds("data/data_extract.tests.rds")
