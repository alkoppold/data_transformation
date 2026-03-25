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
         sphericity = `Was the sphericity checked? answers: yes, not reported`, #has been replaced by subsequent column (inclusion of sphericity corrections, not only tests)
         sphericity_how = `How was sphericity handeled (test, correction)`,
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
  rename_with(\(x) x %>% str_replace_all("\\s*\\([^)]*\\)", "") %>% str_replace_all(" ", "_")) #get rid of info in parentheses & replace space with "_"

#tibble(new = data_extract.full %>% names(), old = data_extract.original %>% names()) %>% print(n = nrow(.))


# Select Variables --------------------------------------------------------
data_extract = data_extract.full %>% 
  select(Extractor:prereg, 
         #deselecting cross_vs_long, n_with_exclusions, n_female_total, age_mean_total, age_sd_total
         n_before_exclusion:mental_health_exclusion, 
         #deselecting individual_level & individual_level_VOI
         
         #TODO keep design column? (important for homoscedasticity but was not checked thoroughly) -> let's talk!
         design:statistical_test_details, #move columns forward (important for statistical assumptions)
         #design_within_levels_max:statistical_test_details, #move columns forward (important for statistical assumptions)
         
         normality:dt_rationale_ref, 
         #deselecting dt_when
         comment)


# Manual Edits ------------------------------------------------------------
data_extract = data_extract %>% 
  rename(orbicularis_oculi = EMG_orbicularis_oculi,
         startle = EMG_startle,
         pupil = PUPIL_SIZE,
         eye = EYE_tracking) %>% 
  mutate(orbicularis_oculi=NA) %>% #manual check: orbicularis EMG has never been used outside of startle responses
  
  mutate(across(starts_with("n_"), \(x) x %>% str_replace_all(",", ";") %>% 
                  #convert "not reported" to NA for columns that need to be numeric (will be done later for design_within_levels_max)
                  na_if("not reported") %>% na_if("partially not reported"))) %>% 
  
  mutate(doi = case_when(doi %>% str_starts("http") ~ doi,
                         T ~ paste0("https://doi.org/", doi))) #ensure doi is full link
#data_extract %>% filter(doi %>% str_detect("doi.org") == F) %>% pull(doi) #articles without DOIs


# Longer Format: Data Transformations -------------------------------------
N_studies = data_extract %>% pull(doi) %>% unique() %>% length()

#data_extract %>% filter(doi %>% is.na()) %>% select(title) #manually replaced NAs
#data_extract %>% count(doi) %>% filter(n != 1)

#TODO copy to results
data_extract.dt = data_extract %>% 
  pivot_longer(HR:pupil, names_to = "DV", values_to = "transformation") %>% 
  mutate(DV = DV %>% as_factor()) %>% #ensures that orbicularis is not dropped by count function (explicit 0)
  filter(transformation %>% is.na() == F) %>% 
  relocate(DV)


# Check & Clean Columns of Interest ---------------------------------------
checkContent = function(df, col, p.denominator=NA, print=T) {
  if (p.denominator %>% is.na() == F && p.denominator %>% is.numeric() == F) warning("p.denominator not numeric. Using sum(n).")
  result = df %>% count(!!rlang::ensym(col), .drop=F) %>% 
    arrange(desc(n)) %>% 
    mutate(p = n / if_else(p.denominator %>% is.numeric(), p.denominator, sum(n)))
  if (print) {
    result %>% print(n = nrow(.))
    return(invisible(result))
  }
  return(result)
}


# * Open Science ----------------------------------------------------------
data_extract %>% checkContent(open_data)
data_extract %>% checkContent(open_code)
data_extract %>% checkContent(open_material)
data_extract %>% checkContent(prereg)


# * Data Transformations --------------------------------------------------
data_extract.dt %>% checkContent(DV)
#data_extract %>% filter(orbicularis_oculi %>% is.na() == F) %>% select(Extractor, doi:title, starts_with(""))
#data_extract %>% filter(orbicularis_oculi %>% is.na() == F, orbicularis_oculi != startle) %>% select(Extractor, doi:title, starts_with(""))
#manual check: orbicularis_oculi has never been used outside of fear potentiated startle => exclude

data_extract.dt %>% checkContent(transformation)
#TODO even longer format with separate_longer_delim ? but sequence is important! -> separate by ";"


# Consistency check data transformations
data_extract %>% checkContent(HR)
data_extract %>% checkContent(HRV)
data_extract %>% checkContent(orbicularis_oculi)
data_extract %>% checkContent(startle)
data_extract %>% checkContent(SCR)
data_extract %>% checkContent(SCL)
data_extract %>% checkContent(eye)
data_extract %>% checkContent(pupil)

data_extract %>% checkContent(Range_correction_type)

data_extract %>% checkContent(dt_specs)

#data_extract %>% filter(Range_correction_type %>% str_detect("baseline")) %>% select(doi, Range_correction_type)




# * Sample Sizes ----------------------------------------------------------
# * * n_before_exclusion --------------------------------------------------
data_extract %>% #start with data_extract to avoid duplicates from data transformations
  mutate(n_before_exclusion = n_before_exclusion %>% 
           #str_replace_all("Exp\\.?\\w?:?\\w?", "ExpX:") %>%  #different experiments shall just be added up => recoded manually
           str_replace_all("\\d+", "N") #generify number for check
  ) %>% 
  checkContent(n_before_exclusion)

# * * n_after_exclusion ---------------------------------------------------
data_extract.dt %>% #start with data_extract to avoid duplicates from data transformations
  mutate(n_after_exclusion = n_after_exclusion %>% 
           #str_replace_all("Exp\\.?\\w?:?\\w?", "ExpX:") %>% 
           str_replace_all("\\d+", "N") #generify number for check
  ) %>% 
  checkContent(n_after_exclusion)


# * * Longer Format: Sample Sizes -----------------------------------------
#TODO copy to results
data_extract.N = data_extract.dt %>% #start with data_extract.dt to retain DV row (if n_* has one entry but there are several DVs, N counts for all DVs and should be duplicated for explicitness)
  separate_longer_delim(starts_with("n_"), ";") %>% 
  #filter(n_before_exclusion %>% str_detect("^\\d+$") == F) %>% 
  #filter(if_any(starts_with("n_"), \(x) x %>% str_detect("^\\d+$") == F)) %>% #only entries that are not completely made up of digits
  mutate(DV2 = n_after_exclusion %>% str_extract("\\b[a-zA-Z]+\\b")) %>% relocate(starts_with("DV")) #extract measurement (dependent variable, DV) from n_after_exclusion (if n_before_exclusion has several measurements, so does n_after_exclusion)

#dv.descriptors = data_extract.dt %>% pull(DV) %>% unique() %>% sort()
dv.descriptors = data_extract.dt %>% pull(DV) %>% levels()
#data_extract.N %>% pull(DV2) %>% unique() %>% sort() %>% setdiff(dv.descriptors) #check invalid descriptors

data_extract.N = data_extract.N %>% 
  filter(DV == DV2 | DV2 %>% is.na() | DV2 %in% dv.descriptors == F) %>% 
  mutate(across(starts_with("n_"), \(x) x %>% str_replace_all("\\D", "")), #delete everything that is not a digit
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
  mutate(n_before_exclusion = n_before_exclusion %>% str_replace_all("\\d+", "N")) %>% 
  checkContent(n_before_exclusion)
data_extract.dt %>% 
  mutate(n_after_exclusion = n_after_exclusion %>% str_replace_all("\\d+", "N")) %>% 
  checkContent(n_after_exclusion)

# Sanity check: n_before_exclusion should be > n_after_exclusion; check also range 
sanity_check_N <- data_extract.N[which(data_extract.N$n_after_exclusion > data_extract.N$n_before_exclusion), ]
range(data_extract.N$n_before_exclusion, na.rm =T)
range(data_extract.N$n_after_exclusion, na.rm =T)


# * mental_health_exclusion -----------------------------------------------
data_extract %>% checkContent(mental_health_exclusion)


# * Assumptions -----------------------------------------------------------
#TODO computational models do not imply a statistical test (but currently coded in column statistical_test)
# make sure that studies coded as computational models:
# 1) do NOT use a statistical test on "2nd level" (e.g., Rescorla-Wagner model + t-test with learning rates)
#    => if they use both, report that statistical test
# 2) those papers that use computational modeling without statistical tests (e.g., just model comparisons with AIC/BIC) 
#    => code "computational model" in design column
# 3) computational models without statistical tests should be removed from all descriptives on statistical assumptions
data_extract %>% filter(statistical_test %>% str_detect("comput")) %>% select(doi, design, starts_with("statistical_test"))

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
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_how)
data_extract = data_extract %>% 
  mutate(normality_how = case_when(normality_how %>% str_detect("visually") ~ "visually", #code "visually, histograms" as "visually"
                                   #normality_how %>% str_detect("Shapiro-Wilk test") ~ "Shapiro-Wilk test", #code "Q-Q plots, Shapiro-Wilk test" as "Shapiro-Wilk test"
                                   #leave "Q-Q plots, Shapiro-Wilk test" separate because it is one qualitative (visually) and one quantitative (p-value based) method
                                   T ~ normality_how))
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_how)



# * * * normality_how_category --------------------------------------------
data_extract = data_extract %>% mutate(normality_how_category = case_when(
  normality_how == "Q-Q plots, Shapiro-Wilk test" ~ "visually & descriptively",
  normality_how %>% str_detect("test") ~ "statistical test",
  normality_how %>% str_detect("skewness") ~ "descriptively", #"skewness and/or kurtosis"
  T ~ normality_how
  )) %>% relocate(normality_how_category, .after = normality_how)

data_extract %>% filter(normality != "not reported") %>% checkContent(normality_how_category)
#note: visually = qualitatively, statistical test + descriptively = quantitatively

# * * * normality_when ----------------------------------------------------
data_extract %>% filter(normality != "not reported") %>% checkContent(normality_when)
#data_extract %>% filter(normality != "not reported", normality_when %>% is.na()) %>% select(doi, starts_with("normality"))

## Sanity checks: normality
sanity_check_normality_how <- data_extract[which(data_extract$normality != "not reported" & is.na(data_extract$normality_how)), ]
sanity_check_normality_when <- data_extract[which(data_extract$normality != "not reported" & is.na(data_extract$normality_when)), ]


# * * Homoscedasticity ----------------------------------------------------

# * * * Design ------------------------------------------------------------
data_extract %>% checkContent(design)
# NAs checked -> stat. models not reported
data_extract %>% filter(design %>% is.na() | design == "not reported") %>% select(doi, design, starts_with("statistical_test"))

# * * * Homoscedasticity --------------------------------------------------
data_extract %>% checkContent(homoscedasticity)
data_extract %>% filter(design != "within") %>% checkContent(homoscedasticity)

#TODO Maren: check in data set
data_extract %>% filter(design == "within", homoscedasticity != "not reported") %>% select(doi, design, starts_with("homoscedasticity"))

# * * * Homoscedasticity How ----------------------------------------------
data_extract %>% checkContent(homoscedasticity_how)

data_extract %>% filter(homoscedasticity_how %>% is.na() == F) %>% select(doi, design, starts_with("homoscedasticity"))

data_extract %>% filter(design != "within", homoscedasticity != "not reported") %>% checkContent(homoscedasticity_how)
#data_extract %>% filter(design != "within", homoscedasticity != "not reported", homoscedasticity_how %>% is.na() == F) %>% select(doi, starts_with("homoscedasticity"))

## Sanity checks: homoscedasticity
sanity_check_homoscedasticity_how <- data_extract[which(data_extract$homoscedasticity != "not reported" & is.na(data_extract$homoscedasticity_how)), ]


# * * Sphericity ----------------------------------------------------------

# * * * Within-Subject Levels ---------------------------------------------
data_extract %>% mutate(design_within_levels_max = design_within_levels_max %>% str_replace_all("\\d+", "N")) %>% checkContent(design_within_levels_max)

#data_extract %>% filter(design_within_levels_max %>% str_detect(";")) %>% checkContent(design_within_levels_max)
data_extract = data_extract %>% mutate(design_within_levels_max = design_within_levels_max %>% 
                                         str_replace_all("EMG", "startle") %>% #EMG means startle
                                         str_replace_all("PUPIL", "pupil")) %>% #pupil in lowercase for consistency
  #only keep maximum number if different by DV
  separate_wider_delim(design_within_levels_max, "; ", names_sep = "_", too_few = "align_start") %>% #, cols_remove = F #doesn't work correctly together with names_sep
  mutate(across(starts_with("design_within_levels_max"), \(x) x %>% str_replace_all("\\D+", "") %>% as.integer())) %>% #extract integers (drop dv.descriptors)
  #select(starts_with("design_within_levels_max")) %>% filter(design_within_levels_max_1 %>% is.na() == F, design_within_levels_max_2 %>% is.na() == F) %>% 
  rowwise() %>% mutate(design_within_levels_max_1 = suppressWarnings(max(c(design_within_levels_max_1, design_within_levels_max_2), na.rm=T))) %>% ungroup() %>% #rowwise maximum
  rename(design_within_levels_max = design_within_levels_max_1) %>% select(-design_within_levels_max_2) %>% #acrobatics needed to preserve column position (since cols_remove = F doesn't work correctly together with names_sep)
  mutate(design_within_levels_max = design_within_levels_max %>% na_if(-Inf)) #set -Inf to NA again

data_extract %>% mutate(design_within_levels_max = design_within_levels_max %>% str_replace_all("\\d+", "N")) %>% checkContent(design_within_levels_max)
range(data_extract$design_within_levels_max, na.rm=T)

# * * * Statistical Model -------------------------------------------------
data_extract %>% checkContent(statistical_test)

#data_extract %>% filter(statistical_test %>% is.na()) %>% pull(doi)
data_extract %>% filter(statistical_test == "multiple") %>% select(doi, statistical_test_details)

data_extract.tests = data_extract %>% 
  relocate(statistical_test) %>% 
  #filter(statistical_test == "multiple") %>% #for testing
  mutate(statistical_test = if_else(statistical_test == "multiple", statistical_test_details, statistical_test)) %>% 
  separate_longer_delim(statistical_test, ", ") %>% 
  
  #TODO: Do we have to adjust also further specifications?
  mutate(
    statistical_test = case_when(statistical_test == "rmANOVA" ~ "ANOVA", #should only be specified in details
                                 statistical_test == "ANCOVA" ~ "ANOVA", #should only be specified in details
                                 statistical_test %>% str_detect("multivariate") ~ "ANOVA", #should only be specified in details
                                 statistical_test %>% str_detect("planned contrasts") ~ "ANOVA", #we subsume planned contrasts into the omnibus model used for the contrasts
                                 
                                 statistical_test %>% str_detect("ttest") ~ "ttest",
                                 
                                 #statistical_test %>% str_detect("Welch") ~ "ttest", #(for unequal variances)
                                 statistical_test %>% str_detect("Welch") ~ "Welch ttest", #leave Welch test separate because it does not require homoscedasticity
                                 
                                 statistical_test %>% str_detect("Whitney") ~ "ordinal ttest", #for independent samples (homoscedasticity not required due to rank transformation)
                                 statistical_test %>% str_detect("Wilcoxon") ~ "ordinal ttest", #for paired samples
                                 
                                 statistical_test %>% str_detect("regression") ~ "regression",
                                 
                                 statistical_test %>% str_detect("equation") ~ "Structural Equation Modeling",
                                 statistical_test %>% str_detect("latent") ~ "Structural Equation Modeling",
                                 statistical_test %>% str_detect("path") ~ "Structural Equation Modeling", 
                                 
                                 #statistical_test == "computational model" ~ "computational modeling",
                                 
                                 statistical_test %>% str_detect("growth") ~ "multilevel model", #growth curve = multilevel mixed model (exception: latent growth curve = SEM)
                                 statistical_test %>% str_detect("hierarchical") ~ "multilevel model", #hierarchical = multilevel (exception: hierarchical regression = regression)
                                 statistical_test %>% str_detect("multilevel") ~ "multilevel model",
                                 
                                 statistical_test %>% str_detect("mixed") ~ "mixed model", #note: mixed models extent GLMs by random effects
                                 
                                 T ~ statistical_test),
    statistical_test = if_else(statistical_test == "multilevel model", "mixed model", statistical_test) #collapse multilevel & mixed model
  )

#data_extract.tests %>% filter(doi %in% {data_extract %>% filter(statistical_test == "multiple") %>% pull(doi)}) %>% checkContent(statistical_test)
#data_extract.tests %>% select(statistical_test, statistical_test_details) %>% filter(statistical_test == "rmANOVA")


# Final check
data_extract.tests %>% checkContent(statistical_test, N_studies)


# * * * Statistical Model Details -----------------------------------------
data_extract.tests %>% checkContent(statistical_test_details, N_studies)


# * * * Sphericity Handling -----------------------------------------------
data_extract %>% checkContent(sphericity)

# All sphericity_how = anything should be sphericity = yes


# Check sphericity_how column
data_extract %>% checkContent(sphericity_how)

data_extract = data_extract %>% 
  ## does not work because of multiple entries
  # mutate(sphericity_how = case_when(sphericity_how %>% str_detect("Greenhouse") ~ "Greenhouse-Geisser correction",
  #                               sphericity_how %>% str_detect("Huynh") ~ "Huynh-Feldt correction",))
  mutate(sphericity_how = sphericity_how %>% str_replace_all("–", "-"))

data_extract %>% 
  filter(statistical_test == "ANOVA", design_within_levels_max > 2) %>% 
  checkContent(sphericity_how)
#checked: Epsilon correction is different from GG or HF corrections
#checked: Mendoza's sphericity test exists
#checked: "Greenhouse-Geisser correction & Huynh-Feldt correction" is different from rest (e.g., "Greenhouse-Geisser correction (ɛ < .75) or Huynh-Feldt correction (ɛ > .75)")

#TODO check NA vs. "not reported": due to previous column: was the sphericity checked? yes/not reported -> if not reported = NA // NA should be not reported?



# * * * Sphericity Category -----------------------------------------------
data_extract = data_extract %>% mutate(sphericity_category = case_when(
  sphericity_how %>% str_detect("test") & sphericity_how %>% str_detect("correction") ~ "both",
  sphericity_how %>% str_detect("test") ~ "test",
  sphericity_how %>% str_detect("correction") ~ "correction",
  T ~ "neither")) %>% relocate(sphericity_category, .after = sphericity_how)

data_extract %>% filter(statistical_test == "ANOVA", design_within_levels_max > 2, sphericity_how %>% is.na() == F, sphericity_how != "not reported") %>% checkContent(sphericity_category)


# * * Independence of Residuals -------------------------------------------
data_extract %>% checkContent(independence)
data_extract %>% #filter(independence != "not reported") %>% #no one did this :')
  checkContent(independence_how)

## Sanity checks: independence
sanity_check_independence_how <- data_extract[which(data_extract$independence != "not reported" & is.na(data_extract$independence_how)), ]



# * * Linearity -----------------------------------------------------------
data_extract %>% checkContent(linearity)
data_extract %>% checkContent(linearity_how)

#data_extract %>% filter(linearity != "not reported") %>% checkContent(linearity_how)
data_extract = data_extract %>% 
  mutate(linearity_how = case_when(linearity_how %>% str_detect("quadr") ~ "quadratic slope",
                                   T ~ linearity_how)) 
#data_extract %>% filter(linearity != "not reported", linearity_how %>% is.na() | linearity_how == "not reported") %>% select(doi, starts_with("linearity")) #manual check completed
#data_extract %>% filter(linearity_how == "linear trends") %>% select(doi, starts_with("linearity")) #manual check completed

data_extract %>% filter(linearity != "not reported") %>% checkContent(linearity_how)

## Sanity checks: linearity
sanity_check_linearity_how <- data_extract[which(data_extract$linearity != "not reported" & is.na(data_extract$linearity_how)), ]



# * * Multicollinearity ---------------------------------------------------
data_extract %>% checkContent(multicollinearity)
data_extract %>% filter(multicollinearity != "not reported") %>% checkContent(multicollinearity_how)
#data_extract %>% filter(multicollinearity != "not reported") %>% select(doi, starts_with("multicoll")) #manual check completed

## Sanity checks: multicollinearity
sanity_check_multicollinearity_how <- data_extract[which(data_extract$multicollinearity != "not reported" & is.na(data_extract$multicollinearity_how)), ]




# * Outlier Handling ------------------------------------------------------
data_extract %>% checkContent(outlier)

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_when)
#data_extract %>% filter(outlier != "no", outlier_when %>% is.na()) %>% select(doi, starts_with("outlier")) #problem fixed

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_how)

data_extract = data_extract %>% mutate(
  outlier_procedure = case_when(
    outlier_how %>% str_detect("SD") ~ "SD-based",
    outlier_how %>% str_detect("Z") ~ "SD-based", #based on z-values is also SD-based
    outlier_how %>% str_detect("IQR") ~ "IQR-based",
    T ~ outlier_how),
    #T ~ "absolute criterion") #TODO use this as soon as other entries are categorized
  outlier_parameter = outlier_how %>% str_extract_all("\\d*\\.?\\d+") %>% sapply(last) #any number (including decimals, excluding minus sign) & only last match
) %>% relocate(outlier_procedure, outlier_parameter, .after = outlier)

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_procedure)
#TODO expand categorization above

data_extract %>% filter(outlier != "no") %>% checkContent(outlier_parameter)

# Sanity check:
data_extract %>% filter(outlier != "no", outlier_how %>% is.na()) %>% select(title, starts_with("outlier"))



# * Range correction type -------------------------------------------------
# Check content
data_extract %>% checkContent(Range_correction_type, print=F) %>% mutate(p = n / N_studies)

# Sanity checks: range correction type
cols_to_check <- c("SCR", "SCL")  # replace with your column names
sanity_check_Range_correction_type <- data_extract[rowSums(data_extract[, cols_to_check] == "rc", na.rm = TRUE) > 0 & is.na(data_extract$Range_correction_type), ] 


# * Rationale -------------------------------------------------------------
# Check content
data_extract %>% checkContent(dt_rationale)
data_extract %>% checkContent(dt_rationale_details)
data_extract %>% checkContent(dt_rationale_ref)

# Sanity checks: rationale
sanity_check_dt_rationale_details <- data_extract[which(data_extract$dt_rationale != "no" & is.na(data_extract$dt_rationale_details)), ]
sanity_check_dt_rationale_ref <- data_extract[which(data_extract$dt_rationale != "no" & is.na(data_extract$dt_rationale_ref)), ]                   




#TODO check if longer format is needed for some columns



# * Add columns if assumptions are needed --------------------------------
# Create default columns
data_extract$normality_need <- NA
data_extract$homoscedasticity_need <- NA
data_extract$sphericity_need <- NA
data_extract$independence_need <- NA
data_extract$linearity_need <- NA
data_extract$multicollinearity_need <- NA

# Specify which assumptions should be met for statistical models
assump_normality <- c("Structural Equation Modeling","mixed model","general linear model","ANOVA","ttest","Welch test","regression","correlation")
assump_homoscedasticity <- c("Structural Equation Modeling","mixed model","general linear model","ANOVA","ttest","regression")
assump_sphericity <- c("ANOVA","Welch test")
assump_independence <- c("Structural Equation Modeling","mixed model","general linear model","ANOVA","ttest","regression","correlation")
assump_linearity <- c("Structural Equation Modeling","mixed model","general linear model","ANOVA","ttest","Welch test","ordinal ttest","regression","correlation")
assump_multicollinearity <- c("Structural Equation Modeling","mixed model","general linear model","ANOVA","regression")

# Fill the columns accordingly and add additional criteria
#TODO: Add "not bayesian" for e.g. t-tests
data_extract$normality_need <- ifelse(
                               data_extract$statistical_test %in% assump_normality,
                               "yes", "no")

data_extract$homoscedasticity_need <- ifelse(
                                      (data_extract$statistical_test %in% assump_homoscedasticity) & 
                                      (data_extract$design != "within") &
                                      (data_extract$design_within_levels_max < 3),
                                      "yes", "no")
data_extract$sphericity_need <- ifelse(
                                (data_extract$statistical_test %in% assump_sphericity) & 
                                (data_extract$design != "between") &
                                (data_extract$design_within_levels_max > 2),
                                "yes", "no")

data_extract$independence_need <- ifelse(
                                  data_extract$statistical_test %in% assump_independence,
                                 "yes", "no")

data_extract$linearity_need <- ifelse(
                               data_extract$statistical_test %in% assump_linearity,
                               "yes", "no")

data_extract$multicollinearity_need <- ifelse(
                                       data_extract$statistical_test %in% assump_multicollinearity,
                                       "yes", "no")

# Write to RDS ------------------------------------------------------------
data_extract %>% write_rds("data/data_extract.rds")
# data_extract.dt %>% write_rds("data/data_extract.dt.rds")
# data_extract.tests %>% write_rds("data/data_extract.tests.rds")
