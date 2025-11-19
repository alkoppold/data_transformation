library(tidyverse)

load("data/Bach_DoxMem2_export.RData")

# heart rate --------------------------------------------------------------
data_acq_cond
#not on trial-level but subject-level!

data_acq_cond %>% pull(sub) %>% unique() %>% length()
#data_acq_cond %>% pull(CS) %>% summary()
data_acq_cond %>% count(CS)

#data_acq_cond %>% select(sub, drug) %>% unique() %>% arrange(sub) #condition not needed: only after acq

data_acq_cond %>% pull(HPR) %>% summary() #HPR: heart period response (heart rate change in milliseconds)


# Pupil & SCR -------------------------------------------------------------
data_acq_trl
#PSR: pupil size response (mm)
#SCR: skin conductance response (muS)

data_acq_trl %>% pull(trial) %>% unique() %>% sort() %>% length()
#data_acq_trl %>% summarize(.by = sub, minTrial = min(trial), maxTrial = max(trial)) %>% filter(maxTrial != 45)

data_acq_trl %>% count(CS)


# SCR ---------------------------------------------------------------------
data_acq_trl %>% pull(SCR) %>% summary()
data_acq_trl %>% pull(SCR) %>% hist() #beautiful!


# Pupil -------------------------------------------------------------------
data_acq_trl %>% pull(PSR) %>% summary()
data_acq_trl %>% pull(PSR) %>% hist() #correct for outliers!

data_acq_trl %>% pull(PSR) %>% quantile(probs = c(.01, .99))
pupilCutoff = 3
#data_acq_trl %>% filter(abs(PSR) < pupilCutoff) %>% nrow() %>% {data_acq_trl %>% nrow() - .}
data_acq_trl %>% filter(abs(PSR) >= pupilCutoff)
#data_acq_trl %>% filter(sub==7) %>% pull(PSR) %>% hist()

data_acq_trl %>% filter(abs(PSR) < pupilCutoff) %>% pull(PSR) %>% hist()
