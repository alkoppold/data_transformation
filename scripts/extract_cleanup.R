library(tidyverse)
library(gsheet)

data_extract.original = gsheet2tbl('https://docs.google.com/spreadsheets/d/1In5IKFNKbVj4WJCawDu2xfr_0CJ3off3Nr5chBaoYYU/edit?gid=1869532958')

# Renaming ----------------------------------------------------------------
#data_extract.original %>% names()
data_extract.full = data_extract.original %>% 
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

tibble(new = data_extract.full %>% names(), old = data_extract.original %>% names()) %>% print(n = nrow(.))


# Select Variables --------------------------------------------------------
data_extract = data_extract.full %>% 
  select(Extractor:prereg, 
         #deselecting cross_vs_long, n_with_exclusions, n_female_total, age_mean_total, age_sd_total
         n_before_exclusion:mental_health_exclusion, 
         #deselecting individual_level & individual_level_VOI
         normality:homoscedasticity_how, 
         #deselecting sphericity_old
         sphericity:dt_rationale_ref, 
         #deselecting dt_when, design
         design_within_levels_max:comment)


# Manual Edits ------------------------------------------------------------
data_extract = data_extract %>% 
  mutate(EMG_orbicularis_oculi=NA) %>% #manual check: orbicularis EMG has never been used outside of startle responses
  mutate(doi = case_when(doi %>% str_starts("http") ~ doi,
                         T ~ paste0("https://doi.org/", doi)))
#data_extract %>% filter(doi %>% str_detect("doi.org") == F) %>% pull(doi) #articles without DOIs


# Longer Format: Data Transformations -------------------------------------
N_studies = data_extract %>% pull(doi) %>% unique() %>% length()

#data_extract %>% filter(doi %>% is.na()) %>% select(title) #manually replaced NAs
#data_extract %>% count(doi) %>% filter(n != 1)

#TODO move to results?
data_extract.dt = data_extract %>% 
  pivot_longer(HR:PUPIL_SIZE, names_to = "DV", values_to = "transformation") %>% 
  mutate(DV = DV %>% gsub("EMG_", "", .)) %>% #just "startle" instead of "EMG_startle"
  mutate(DV = DV %>% gsub("PUPIL_SIZE", "pupil", .)) %>% 
  mutate(DV = DV %>% gsub("EYE_tracking", "eye", .)) %>% 
  mutate(DV = DV %>% as_factor()) %>% 
  filter(transformation %>% is.na() == F) %>% 
  relocate(DV)


# Check & Clean Columns of Interest ---------------------------------------
checkContent = function(df, col, print=T) {
  result = df %>% count(!!rlang::ensym(col), .drop=F) %>% arrange(desc(n))
  if (print) {
    result %>% print(n = nrow(.))
    return(invisible(result))
  }
  return(result)
}


# * Open Science ----------------------------------------------------------
data_extract %>% checkContent(open_data, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(open_code, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(open_material, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(prereg, print=F) %>% mutate(p = n / N_studies)


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

#sample size: check result
data_extract.dt %>% 
  mutate(n_before_exclusion = n_before_exclusion %>% gsub("\\d+", "N", .)) %>% 
  checkContent(n_before_exclusion, print=F) %>% mutate(p = n / N_studies)
data_extract.dt %>% 
  mutate(n_after_exclusion = n_after_exclusion %>% gsub("\\d+", "N", .)) %>% 
  checkContent(n_after_exclusion, print=F) %>% mutate(p = n / N_studies)

# Sanity check: n_before_exlusion should be > n_after_exclusion; check also range 
sanity_check_N <- data_extract.N[which(data_extract.N$n_after_exclusion > data_extract.N$n_before_exclusion), ]
range(data_extract.N$n_before_exclusion, na.rm =T)
range(data_extract.N$n_after_exclusion, na.rm =T)


# * mental_health_exclusion -----------------------------------------------
data_extract %>% checkContent(mental_health_exclusion, print=F) %>% mutate(p = n / N_studies)


# * Assumptions -----------------------------------------------------------

# * * Normality Checks ----------------------------------------------------

# * * * Normality ---------------------------------------------------------
#data_extract %>% filter(normality == "unclear") %>% select(doi, starts_with("normality")) #manually checked and split up into "unclear [IF normality test has been performed]" vs. "not specified" (test has been reported but not specified)
data_extract = data_extract %>% 
  mutate(normality = case_when(normality == "unclear" ~ "not reported", #"unclear" was supposed to be coded as "not reported" => drop and discuss
                               T ~ normality))
data_extract %>% checkContent(normality, print=F) %>% mutate(p = n / N_studies)
#data_extract %>% filter(normality != "not reported", normality_how %>% is.na()) %>% pull(doi) #inconsistencies manually corrected
#data_extract %>% filter(normality == "not specified") %>% select(doi, starts_with("normality")) #manually checked and split up into "unclear [IF normality test has been performed]" vs. "not specified" (test has been reported but not specified)


# * * * normality_how -----------------------------------------------------
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_how, print=F) %>% mutate(p = n / sum(n))
data_extract = data_extract %>% 
  mutate(normality_how = case_when(normality_how %>% str_detect("visually") ~ "visually", #code "visually, histograms" as "visually"
                                   normality_how %>% str_detect("Shapiro-Wilk test") ~ "Shapiro-Wilk test", #code "Q-Q plots, Shapiro-Wilk test" as "Shapiro-Wilk test"
                                   T ~ normality_how))
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_how, print=F) %>% mutate(p = n / sum(n))


# * * * normality_when ----------------------------------------------------
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_when, print=F) %>% mutate(p = n / sum(n))
#data_extract %>% filter(normality != "not reported", normality_when %>% is.na()) %>% select(doi, starts_with("normality"))

## Sanity checks: normality
sanity_check_normality_how <- data_extract[which(data_extract$normality != "not reported" & is.na(data_extract$normality_how)), ]
sanity_check_normality_when <- data_extract[which(data_extract$normality != "not reported" & is.na(data_extract$normality_when)), ]


# * * Homoscedasticity ----------------------------------------------------
data_extract %>% checkContent(homoscedasticity, print=F) %>% mutate(p = n / N_studies)
data_extract %>% filter(homoscedasticity != "not reported") %>% checkContent(homoscedasticity_how, print=F) %>% mutate(p = n / sum(n))
#data_extract %>% filter(homoscedasticity != "not reported", homoscedasticity_how %>% is.na()) %>% select(doi, starts_with("homoscedasticity"))

## Sanity checks: homoscedasticity
sanity_check_homoscedasticity_how <- data_extract[which(data_extract$homoscedasticity != "not reported" & is.na(data_extract$homoscedasticity_how)), ]


# * * Sphericity ----------------------------------------------------------

# * * * Within-Subject Levels ---------------------------------------------
data_extract.dt %>% checkContent(design_within_levels_max) 
#TODO long format


# * * * Statistical Model -------------------------------------------------
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

data_extract.tests %>% 
  mutate(design_within_levels_max = design_within_levels_max %>% gsub("\\d+", "N", .)) %>% 
  checkContent(design_within_levels_max, print=F) %>% mutate(p = n / N_studies)
#TODO separate_longer_delim (Exp1 vs. 2: just check if any number > 3)

data_extract.tests %>% checkContent(statistical_test, print=F) %>% mutate(p = n / N_studies)
data_extract.tests %>% checkContent(statistical_test_details, print=F) %>% mutate(p = n / N_studies)


# * * * Sphericity Handling -----------------------------------------------
#TODO long format?
data_extract = data_extract %>% 
  ## does not work because of multiple entries
  # mutate(sphericity = case_when(sphericity %>% str_detect("Greenhouse") ~ "Greenhouse-Geisser correction",
  #                               sphericity %>% str_detect("Huynh") ~ "Huynh-Feldt correction",))
  mutate(sphericity = sphericity %>% gsub("–", "-", .))

data_extract %>% 
  #TODO implement filters (not working yet)
  #filter(statistical_test == "ANOVA", design_within_levels_max > 2) %>% 
  checkContent(sphericity, print=F) %>% mutate(p = n / sum(n))
#checked: Epsilon correction is different from GG or HF corrections
#checked: Mendoza's sphericity test exists
#checked: "Greenhouse-Geisser correction & Huynh-Feldt correction" is different from rest (e.g., "Greenhouse-Geisser correction (ɛ < .75) or Huynh-Feldt correction (ɛ > .75)")
#checked: "unclear" is correct
#TODO check NA vs. "not reported"
#TODO split up into test vs. correction column? (could collapse "Greenhouse-Geisser" vs. "Mauchly's test, Greenhouse-Geisser)



# * * Independence of Residuals -------------------------------------------
data_extract %>% checkContent(independence, print=F) %>% mutate(p = n / N_studies)
data_extract %>% #filter(independence != "not reported") %>% #no one did this :')
  checkContent(independence_how, print=F) %>% mutate(p = n / sum(n))

## Sanity checks: independence
sanity_check_independence_how <- data_extract[which(data_extract$independence != "not reported" & is.na(data_extract$independence_how)), ]



# * * Linearity -----------------------------------------------------------
data_extract %>% checkContent(linearity, print=F) %>% mutate(p = n / N_studies)

#data_extract %>% filter(linearity != "not reported") %>% checkContent(linearity_how, print=F) %>% mutate(p = n / sum(n))
data_extract = data_extract %>% 
  mutate(linearity_how = case_when(linearity_how %>% str_detect("quadr") ~ "quadratic slope",
                                   T ~ linearity_how)) 
#data_extract %>% filter(linearity != "not reported", linearity_how %>% is.na() | linearity_how == "not reported") %>% select(doi, starts_with("linearity")) #manual check completed
#data_extract %>% filter(linearity_how == "linear trends") %>% select(doi, starts_with("linearity")) #manual check completed

data_extract %>% filter(linearity != "not reported") %>% checkContent(linearity_how, print=F) %>% mutate(p = n / sum(n))

## Sanity checks: linearity
sanity_check_linearity_how <- data_extract[which(data_extract$linearity != "not reported" & is.na(data_extract$linearity_how)), ]



# * * Multicollinearity ---------------------------------------------------
data_extract %>% checkContent(multicollinearity, print=F) %>% mutate(p = n / N_studies)
data_extract %>% filter(multicollinearity != "not reported") %>% checkContent(multicollinearity_how, print=F) %>% mutate(p = n / sum(n))
#data_extract %>% filter(multicollinearity != "not reported") %>% select(doi, starts_with("multicoll")) #manual check completed

## Sanity checks: multicollinearity
sanity_check_multicollinearity_how <- data_extract[which(data_extract$multicollinearity != "not reported" & is.na(data_extract$multicollinearity_how)), ]




# * Outlier Handling ------------------------------------------------------
data_extract %>% checkContent(outlier, print=F) %>% mutate(p = n / N_studies)

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_when, print=F) %>% mutate(p = n / sum(n))
#data_extract %>% filter(outlier != "no", outlier_when %>% is.na()) %>% select(doi, starts_with("outlier")) #problem fixed

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_how, print=F) %>% mutate(p = n / sum(n))
#TODO separate into outlier_procedure (SD-based vs. IQR-based) & outlier_parameter (numeric cutoff used)

#TODO check:
data_extract %>% filter(outlier != "no", outlier_how %>% is.na()) %>% select(doi, starts_with("outlier"))

## Sanity checks: outlier
sanity_check_outlier_how <- data_extract[which(data_extract$outlier == "yes" & is.na(data_extract$outlier_how)), ]    #TODO: CHECK!!
sanity_check_outlier_when <- data_extract[which(data_extract$outlier == "yes" & is.na(data_extract$outlier_when)), ]


# * Range correction type -------------------------------------------------
# Check content
data_extract %>% checkContent(Range_correction_type, print=F) %>% mutate(p = n / N_studies)

# Sanity checks: range correction type
cols_to_check <- c("SCR", "SCL")  # replace with your column names
sanity_check_Range_correction_type <- data_extract[rowSums(data_extract[, cols_to_check] == "rc", na.rm = TRUE) > 0 & is.na(data_extract$Range_correction_type), ] #TODO: CHECK!!


# * Rationale -------------------------------------------------------------
# Check content
data_extract %>% checkContent(dt_rationale, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(dt_rationale_details, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(dt_rationale_ref, print=F) %>% mutate(p = n / N_studies)

# Sanity checks: rationale
sanity_check_dt_rationale_details <- data_extract[which(data_extract$dt_rationale == "yes" & is.na(data_extract$dt_rationale_details)), ]           #TODO: CHECK!!
sanity_check_dt_rationale_details <- data_extract[which(data_extract$dt_rationale == "partially" & is.na(data_extract$dt_rationale_details)), ]     #TODO: CHECK!!
sanity_check_dt_rationale_ref <- data_extract[which(data_extract$dt_rationale == "yes" & is.na(data_extract$dt_rationale_ref)), ]                   #TODO: CHECK!!
sanity_check_dt_rationale_ref <- data_extract[which(data_extract$dt_rationale == "partially" & is.na(data_extract$dt_rationale_ref)), ]




# Check consistency of columns --------------------------------------------
#TODO check if longer format is needed for some columns

#data transformations
data_extract %>% checkContent(HR, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(HRV, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(EMG_orbicularis_oculi, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(EMG_startle, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(SCR, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(SCL, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(EYE_tracking, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(PUPIL_SIZE, print=F) %>% mutate(p = n / N_studies)

data_extract.dt %>% checkContent(DV, print=F) %>% mutate(p = n / N_studies)

data_extract %>% checkContent(dt_specs, print=F) %>% mutate(p = n / N_studies)
data_extract %>% checkContent(Range_correction_type, print=F) %>% mutate(p = n / N_studies)
#data_extract %>% filter(Range_correction_type %>% str_detect("baseline")) %>% select(doi, Range_correction_type)




# Write to RDS ------------------------------------------------------------
data_extract %>% write_rds("data/data_extract.rds")
data_extract.dt %>% write_rds("data/data_extract.dt.rds")
data_extract.tests %>% write_rds("data/data_extract.tests.rds")
